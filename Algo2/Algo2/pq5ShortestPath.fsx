#if INTERACTIVE
#load @"__solmerged.fsx"
#load @"App.fs"
#endif

open Graph
open System.Collections.Generic


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
