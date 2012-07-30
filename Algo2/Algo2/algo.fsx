#if INTERACTIVE
#load @"__solmerged.fsx"
#load @"App.fs"
#endif

open DataStructure.Graph
open System.Collections.Generic

let sorts, mincut, SCC, shortestpath = false, false, true, false
let other  = false

if shortestpath then
   let f =  System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\dijkstraData.txt")
   let gl = f  |> Seq.map(fun l -> 
                                   let a = l.Split('\t')
                                   (int a.[0], a |> Seq.skip(1) 
                                                 |> Seq.choose( fun s -> let e = s.Split(',')
                                                                         if e.Length = 2 then
                                                                               Some (int e.[0], int e.[1])
                                                                         else  None )))
   let graph = MutableGraph(gl)

   let d = graph.computeShortest 1

   let assignment = [7;37;59;82;99;115;133;165;188;197]
   let r = assignment |> List.map(fun i -> d.[i].ToString())
                      |> String.concat ","
   ()

if SCC then 
   let f =  System.IO.File.ReadLines (__SOURCE_DIRECTORY__ + @"\SCC2.txt")
   let gl = f  |> Seq.map(fun l -> let a = l.Split(' ')
                                   (int a.[0], int a.[1]))
               |> Seq.groupBy fst
               |> Seq.map (fun (a,b) -> a, b |> Seq.map snd)
   let graph = MutableGraph(gl)
   let scc = graph.SCCKosaraju
   let biggest = scc |> Seq.sortBy(fun scc -> -(scc |>  Seq.length)) 
   let biggestsize = biggest |> Seq.take(5) |> Seq.map(fun scc -> scc |> Seq.length) |> Seq.toArray

   let n = graph.nodes() |> Set.count
   let nn = biggest |> Seq.sumBy(fun i -> i |> Seq.length)
   let second = biggest |> Seq.nth 1 |> Seq.map (fun it -> it.ToString()) |> String.concat ","
   printfn "%A" second
   ()


if other then
   let a = [(1, [2;3]);(2, [4]);(3, [4]);(4, [7])] |> List.map(fun (a,els)-> a,els |> List.toSeq) 
   let g = MutableGraph(a)
   let gg = g.reverse 
   gg.vertexList |> Seq.nth 3
   gg.vertexList |> Seq.length
   let l = (g.nodes() |> Seq.toList)
   let lll = g.loopDFS l |> Seq.toArray
   let gt = g.SCCKosaraju |> Seq.map(Seq.toArray) |> Seq.toArray


   let depthseq = lll |> Seq.concat |> Seq.distinct

   g.loopDFS depthseq
   g.SCCKosaraju

   let b = [(1, [4]);(2, [8]);(3, [6]);(4, [7]);(5, [2]);(6, [9]);(7, [1]);(8, [6]);(9, [7])] |> List.map(fun (a,els)-> a,els |> List.toSeq) 
   let g2 = MutableGraph(b)
   g2.SCCKosaraju |> Seq.map(Seq.toArray) |> Seq.toArray
   ()

//val biggestsize : int [] = [|434821; 969; 459; 313; 211|]

if sorts then
   let ar =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\IntegerArray.txt") |> Array.map (double)  |> MainApp.mergesortcountmutable |> ignore
   let f =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\QuickSort.txt") |> Array.map (double)
   let r1 = MainApp.quickSortAndCount (f.Clone() :?> _) MainApp.getpivot1 
   let r2 = MainApp.quickSortAndCount (f.Clone() :?> _) MainApp.getpivot2 
   let r3 = MainApp.quickSortAndCount (f.Clone() :?> _) MainApp.getpivot3 
   ()

if mincut then
   let lines =  System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + @"\kargerMinCut.txt")
   let gl =lines |> Seq.map(fun l -> let a = l.Split('\t')
                                     (int a.[0]), a |> Seq.skip 1 
                                                       |> Seq.filter(fun s -> s <> "" )
                                                       |> Seq.map (int)
                                                       |> Seq.map(fun it -> it))
   let g = MutableGraph(gl)
   let mincut (g:MutableGraph) =      let n = g.vertexList.Count
                                      seq {1 .. n } |> Seq.map(fun _ -> let graph = g.Copy()
                                                                        graph.contract()
                                                                        let f = (graph.vertexList |> Seq.head).Value
                                                                        printfn "count %A" f.adjacency.Count 
                                                                        f.adjacency.Count)
                                                    |> Seq.min
   mincut g
   ()
                                         


