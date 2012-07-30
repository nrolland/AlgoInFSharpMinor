namespace DataStructure

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type 'a PairingHeap = E | T of 'a * 'a PairingHeap list * int

module PairingHeap =
   let empty = E
    
   let isEmpty = function | E -> true | T_ -> false
    
   let merge h1 h2 =
      match h1, h2 with
      | E, h | h, E -> h
      | T(x, hs1, n1), T(y, hs2, n2) ->
            if x <= y then T(x, h2::hs1, n1+n2) else T(y, h1::hs2, n1+n2)
    
   let insert x h = merge (T(x, [], 1)) h
    
   let mergePairs xs =
      let rec mergePairs xs k =
         match xs with
         | [] -> k E
         | [h] -> k h
         | h1::h2::hs -> mergePairs hs (fun hs -> k(merge (merge h1 h2) hs))
      mergePairs xs id
    
   let removeMin h =
      match h with
      | E -> failwith "Empty"
      | T(x, hs, n) -> x, mergePairs hs

   let length h =  function | E -> 0  | T(_,_,n) -> n
   let getMax h =  function | E -> failwith "empty heap"  | T(x,_,_) -> x

   let content h =
      let rec lcontent (s:seq<'a>) = function |  E  -> s  
                                              |  T(xt,l,_) ->  let s' = List.fold(fun s el -> lcontent s el) s l
                                                               seq{ yield! s'; yield xt}
                                                                
      lcontent Seq.empty h





type 'a Heap = | Heap of ('a->'a->int) * 'a PairingHeap

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Heap =
   let empty c = Heap (c, E)
    
   let isEmpty = function |  Heap (c, E) -> true |  Heap (c, T_) -> false
    
   let internal merge c h1 h2 =
      match h1, h2 with
      | E, h | h, E -> h
      | T(x, hs1, n1), T(y, hs2, n2) ->
            if  c x y > 0 then T(x, h2::hs1, n1+n2) else T(y, h1::hs2, n1+n2)
    
   let insert x h =
      let (Heap (c, t)) = h
      Heap(c, merge c (T(x, [], 1)) t)
    
   let internal mergePairs c xs =
      let rec mergePairs c xs k =
         match xs with
         | [] -> k E
         | [h] -> k h
         | h1::h2::hs -> mergePairs c hs (fun hs -> k(merge c (merge c h1 h2) hs))
      mergePairs c xs id
    
   let removeExtr h =
      let (Heap (c, h)) = h
      match h with
      | E -> failwith "Empty"
      | T(x, hs, n) -> x, Heap (c, mergePairs c hs)

   let length     = function | Heap (c, E)  -> 0  |  Heap (c, T(_,_,n)) -> n
   let getExtr    = function | Heap (c, E)  -> failwith "empty heap"  |  Heap (c, T(x,_,_)) -> x
   let compare x  = function | Heap (c, E)  -> 1  |  Heap (c, T(xt,_,_)) -> c x xt
   let content h  = let (Heap (c, t)) = h in PairingHeap.content t



    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HeapMin =
   let empty = Heap((fun x y -> - compare x y), E)
   let removeMin h = Heap.removeExtr
   let getMin h =   Heap.getExtr h
   let isBiggerThanMin x h = Heap.compare  x h <= 0

   let isEmpty = Heap.isEmpty
   let insert x h = Heap.insert x h
   let length h =  Heap.length h

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HeapMax =
   let empty  = Heap(compare,E)
   let removeMax h = Heap.removeExtr h
   let getMax h =   Heap.getExtr h
   let isSmallerThanMax x h =  Heap.compare x h <= 0


   let isEmpty = Heap.isEmpty
   let insert x h = Heap.insert x h
   let length h =  Heap.length h


