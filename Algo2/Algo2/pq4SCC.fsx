#if INTERACTIVE
#load @"__solmerged.fsx"
#load @"App.fs"
#endif

open DataStructure.Graph
open System.Collections.Generic

let other  = false

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