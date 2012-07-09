#if INTERACTIVE
#load @"__solmerged.fsx"
#load @"App.fs"
#endif


if false then
   let f =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\IntegerArray.txt")

   let ar = f |> Array.map (double) 
   let lar = ar |> Array.toList

   ar  |> MainApp.mergesortcountmutable |> ignore
   //lar  |> MainApp.mergesSort stack overflow !


let f2 =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\QuickSort.txt")

let ar1 = f2 |> Array.map (double) 
MainApp.quickSortAndCount   ar1 MainApp.getpivot1
let ar2 = f2 |> Array.map (double) 
MainApp.quickSortAndCount   ar2 MainApp.getpivot2
let ar3 = f2 |> Array.map (double) 
MainApp.quickSortAndCount   ar3 MainApp.getpivot3


let rnd = System.Random()

let atRandom (s:'a seq) = s |> Seq.nth (rnd.Next(Seq.length s))

type Node<'TDaton> =
    int (* node identifier *) *
    'TDaton (* node data *)
 
type Connection<'TDaton> =
    int (* source Id *) *
    int (* sink Id *) *
    int (* weight *) *
    'TDaton (* connection data *)
 
type Adjacency<'TDaton> =
    Connection<'TDaton> list (* a list of connections to other nodes*)
 
type Atom<'TNodeDaton, 'TConnectionDaton> =
    Node<'TNodeDaton> * Adjacency<'TConnectionDaton>
 
type Graph<'TNodeDaton, 'TConnectionDaton> =
    Atom<'TNodeDaton, 'TConnectionDaton> list


let contractone (graph:Graph<_,_>):Graph<_,_> = 
      let (node, edges) = atRandom graph 
      let contractedge =  atRandom edges 
      let (head, tail, w, data) = contractedge
      //printfn "%A %A %A" node head tail
      let findAtom ref = graph |> List.find(fun ((r,_), _) -> r = ref )
      let atomhead, atomtail = findAtom head, findAtom tail

      let filterAdjacency  (al:Adjacency<_>) ref        : Adjacency<_> = al |> List.filter (fun (h,t,_,_) -> t <> ref)
      let replaceAdjacency (al:Adjacency<_>) ref refnew : Adjacency<_> = al |> List.map    (fun (h,t,a,b) -> if h = ref then ((refnew,t,a,b))
                                                                                                             elif t = ref then ((h,refnew,a,b))
                                                                                                             else (h,t,a,b))

      //I remove cross references, and merge the connection list
      //Then I go trough all atoms, replace them with the new, and replace all connections to tail with head
      let alh, alt = filterAdjacency (snd atomhead) tail, filterAdjacency (snd atomtail) head
      let atomnew = fst atomhead,  replaceAdjacency (List.append alh alt)  tail head
      //printfn "atomnew %A" atomnew

      let newgraph = graph |> List.choose(fun ((ref,a), edges) -> if ref = head then
                                                                     Some atomnew
                                                                  elif ref = tail then
                                                                     None
                                                                  else
                                                                     Some ((ref, a ), replaceAdjacency edges tail head)
                                          )                         
      newgraph


let rec contract (graph:Graph<_,_>) = 
   if graph.Length > 2 then
      contract (contractone graph)
   else
      graph

let mincut (graph:Graph<_,_>) = 
   let g = contract graph
   g.Head |> snd |> List.length

let f =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\kargerMinCut.txt")
let f1 = f.[0]
let aa = f1.Split('\t')

let g:Graph<_,_> = f |> Array.map(fun l -> let a = l.Split('\t')
                                           let ref, edges = int a.[0], a |> Seq.skip 1
                                           let at:Atom<_,_> = ((ref,()), edges |>Seq.filter(fun s -> s <> "" ) |> Seq.map(fun s -> (ref, int s ,0,())) |> Seq.toList)
                                           at)
                     |> Array.toList


[1.. 10] |> List.map(fun _ ->  let cg = contract g
                               let l = snd g.Head 
                               l.Length)


//let (node, edges) = atRandom g in atRandom edges 
