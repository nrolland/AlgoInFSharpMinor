module Graph

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
      let contractedge = let (node, edges) = atRandom graph in atRandom edges 
      let (head, tail, w, data) = contractedge

      let findAtom ref = graph |> List.find(fun ((r,_), _) -> r = ref )
      let atomhead, atomtail = findAtom head, findAtom tail

      let filterAdjacency  (al:Adjacency<_>) ref        : Adjacency<_> = al |> List.filter (fun (h,t,_,_) -> h <> ref &&  t <> ref)
      let replaceAdjacency (al:Adjacency<_>) ref refnew : Adjacency<_> = al |> List.map    (fun (h,t,a,b) -> if h = ref then (refnew,t,a,b)
                                                                                                             elif t = ref then (h,refnew,a,b)
                                                                                                             else (h,t,a,b))

      //I remove cross references, and merge the connection list
      //Then I go trough all atoms, replace them with the new, and replace all connections to tail with head
      let alh, alt = filterAdjacency (snd atomhead) tail, filterAdjacency (snd atomtail) head
      let atomnew = fst atomhead, List.append alh alt 

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