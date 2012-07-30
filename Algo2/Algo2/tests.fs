module tests
open System
open FsCheck
open Xunit
open Swensen.Unquote
open DataStructure 
open DataStructure.Graph

[<Fact>]
let ``should fail`` () =
    test <@ ([3; 2; 1; 0] |> List.map ((+) 1)) = [1 + 3..1 + 0] @>

[<Fact>]
let ``reverse should work`` () =
    //mincut (MutableGraph(gl))
    let a = [(1, [2;3]);(2, [4]);(3, [4]);(4, [])] |> List.map(fun (a,els)-> a,els |> List.toSeq) 
    let g = MutableGraph(a)
    let gg = g.reverse 
    test <@ (gg.vertexList.Values |> Seq.nth 3).id = 4 @>
    test <@ (gg.vertexList.Values |> Seq.nth 3).adjacency |> Seq.length = 2 @>

[<Fact>]
let ``BFS should work`` () =
    //mincut (MutableGraph(gl))
    let a = [(1, [2;3]);(2, [4]);(3, [4]);(4, [])] |> List.map(fun (a,els)-> a,els |> List.toSeq) 
    let g = MutableGraph(a)
    test <@ g.searchBF 1   |> Seq.toList = [1;2;3;4]@>

[<Fact>]
let ``DFS should work`` () =
    //mincut (MutableGraph(gl))
    let a = [(1, [2;3]);(2, [4]);(3, [4]);(4, [])] |> List.map(fun (a,els)-> a,els |> List.toSeq) 
    let g = MutableGraph(a)
    test <@ g.searchDF 1   |> Seq.toList = [1;3;4;2]@>

[<Fact>]
let ``HeapMax should track highest element`` () =
    let ar = [|1 .. 100|] |> Array.rev
    printfn "inserted %A" ar 
    let hp = List.foldBack  Heap.insert (ar |> MainApp.shuffle) HeapMax.empty
    let a = Seq.unfold (fun (hp, sint)  -> 
                                  if sint |> Seq.isEmpty then
                                    None
                                  else
                                    let max, h = hp |> HeapMax.removeMax 
                                    printfn "removed %A" max 
                                    Some(max = Seq.head sint, (h, Seq.skip 1 sint))
               ) (hp, ar :> seq<_>)
    test <@  a |> Seq.forall (id) @>

[<Fact>]
let `` HeapMax compare =  > `` () =
    let ar = [|1 .. 100|] |> Array.rev
    let hp = List.foldBack  Heap.insert (ar |> MainApp.shuffle) HeapMax.empty
    test <@  Heap.compare 101 hp =  1 @>
    test <@  Heap.compare 100 hp =  0 @>
    test <@  Heap.compare  -9 hp = -1 @>

[<Fact>]
let ``any element is > HeapMax empty `` () =
   test <@  Heap.compare   0 HeapMax.empty = 1 @>
   test <@  Heap.compare -99 HeapMax.empty = 1 @>

[<Fact>]
let ``HeapMin should track smallest element`` () =
    let ar = [|1 .. 100|] 
    let hp = List.foldBack  Heap.insert (ar |> MainApp.shuffle) HeapMin.empty
    let a = Seq.unfold (fun (hp, sint)  -> 
                                  if sint |> Seq.isEmpty then
                                    None
                                  else
                                    let min, h = hp |> HeapMax.removeMax 
                                    printfn "removed %A" min 
                                    Some(min = Seq.head sint, (h, Seq.skip 1 sint))
               ) (hp, ar :> seq<_>)
    test <@  a |> Seq.forall (id) @>

[<Fact>]
let `` HeapMin compare =  < `` () =
    let ar = [|1 .. 100|] |> Array.rev
    let hp = List.foldBack  Heap.insert (ar |> MainApp.shuffle) HeapMin.empty
    test <@  Heap.compare 101 hp = -1 @>
    test <@  Heap.compare   1 hp =  0 @>
    test <@  Heap.compare  -9 hp =  1 @>

[<Fact>]
let ``any element is inf to  HeapMin empty `` () =
   test <@  Heap.compare   0 HeapMin.empty = 1 @>
   test <@  Heap.compare -99 HeapMin.empty = 1 @>

   