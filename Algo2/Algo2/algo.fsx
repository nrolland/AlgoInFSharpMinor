#if INTERACTIVE
#load @"__solmerged.fsx"
#load @"App.fs"
#endif

open Graph
fsi.AddPrintTransformer (fun (s:EdgeData<unit> seq) -> s |> Seq.map (fun x -> x.targetVertex.ToString() )  |> String.concat " " |> box )
if false then
   let f =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\IntegerArray.txt")
   let ar = f |> Array.map (double) 
   ar  |> MainApp.mergesortcountmutable |> ignore
   let f2 =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\QuickSort.txt")
   let ar1 = f2 |> Array.map (double) in MainApp.quickSortAndCount   ar1 MainApp.getpivot1 |> ignore
   let ar2 = f2 |> Array.map (double) in MainApp.quickSortAndCount   ar2 MainApp.getpivot2 |> ignore
   let ar3 = f2 |> Array.map (double) in MainApp.quickSortAndCount   ar3 MainApp.getpivot3 |> ignore

let lines =  System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + @"\kargerMinCut.txt")
let gl =lines |> Seq.map(fun l -> let a = l.Split('\t')
                                  (int a.[0],()), a |> Seq.skip 1 
                                                    |> Seq.filter(fun s -> s <> "" )
                                                    |> Seq.map (int)
                                                    |> Seq.map(fun it -> it,()))
let mincut (g:MutableGraph<_,_>) = let n = g.vertexList.Count
                                   seq {1 .. n } |> Seq.map(fun _ -> let graph = g.Copy()
                                                                     //g.vertexList |> Seq.take 1 |> Seq.iter(fun v -> printfn "avant %A" (v.adjacency |> Seq.map (fun x -> x.targetVertex.ToString() ) |> String.concat " " ) ) 
                                                                     graph.contract()
                                                                     //g.vertexList |> Seq.take 1 |> Seq.iter(fun v -> printfn "apres %A" (v.adjacency |> Seq.map (fun x -> x.targetVertex.ToString() ) |> String.concat " " ) ) 
                                                                     let f = graph.vertexList |> Seq.head
                                                                     printfn "count %A" f.adjacency.Count 
                                                                     f.adjacency.Count)
                                                 |> Seq.min
                                                 
mincut (MutableGraph(gl))

let g =(MutableGraph(gl))
let graph = g.Copy()
graph.vertexList.RemoveAll(fun _ -> true)
g

[1 .. 10 ] |> List.map (fun _->
                                 let graph = MutableGraph(gl)
                                 graph.contract()
                                 let f = graph.vertexList |> Seq.head
                                 printfn "%A" f.adjacency.Count 
                                 f.adjacency.Count)
()