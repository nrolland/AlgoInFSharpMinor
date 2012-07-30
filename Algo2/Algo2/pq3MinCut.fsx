#if INTERACTIVE
#load @"__solmerged.fsx"
#load @"App.fs"
#endif

open Graph
open System.Collections.Generic



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
                                         



