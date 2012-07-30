#if INTERACTIVE
#load @"__solmerged.fsx"
#load @"App.fs"
#endif

open DataStructure
open System.Collections.Generic

if false then
   let ar =  System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + @"\HashInt.txt") |> Array.map (int) 
   let hsNumbers = HashSet(ar)
 
   let pairsumingto n = hsNumbers 
                        |> Seq.choose(fun it -> if hsNumbers.Contains (n - it) && n <> (n-it)
                                                then Some(it, n-it)
                                                else None )

   let cansumto  = (pairsumingto >> Seq.isEmpty >> not )

   let t = cansumto 2500  //  seq [(643, 1857); (993, 1507); (1507, 993); (583, 1917); ...]

   let r = [2500 .. 4000] |> Seq.filter  cansumto
   r |> Seq.length

   let rand = System.Random()
   let n = 10
   let xs = Array.init n (fun _ -> rand.NextDouble())
   let mutable heap = PairingHeap.empty
   for i=0 to n-1 do
      heap <- PairingHeap.insert xs.[i] heap
   let ys = Array.zeroCreate n
   for i=0 to n-1 do
      let x, heap' = PairingHeap.removeMin heap
      heap <- heap'
      let (T(a,l,n)) = heap
      printfn "%A" n 
      ys.[i] <- x
   printf "Correct %A\n" (ys = Array.sort xs)
   ()


let stream =  System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + @"\Median.txt") |> Array.map (int) 


let medians (stream:int array) = Seq.unfold   (fun (i,lower, upper) ->  
                                                      if i > stream.Length - 1 then
                                                         None
                                                      else 
                                                         let x = stream.[i] 
                                                         //printfn "received value %A" x 
                                                         let lower', upper' = 
                                                            match HeapMax.isSmallerThanMax x lower, HeapMin.isBiggerThanMin x upper with
                                                            | true, false      -> Heap.insert x lower,  upper
                                                            | false , true     -> lower, Heap.insert x upper   
                                                            | true, true       -> lower, Heap.insert x upper    
                                                            | false ,false     -> Heap.insert x lower, upper  

                                                         //printfn "lower, upper %A %A" (lower' |> Heap.content) (upper'|> Heap.content)

                                                         let lbal, ubal =
                                                            match Heap.length lower', Heap.length upper' with
                                                            | l, u when l > u+1 -> let e, m = Heap.removeExtr lower'
                                                                                   m, Heap.insert e upper'
                                                            | l, u when l < u   -> let e, m = Heap.removeExtr upper'
                                                                                   Heap.insert e lower', m
                                                            | _ -> lower', upper'
                                                         //printfn "lbal, ubal   %A %A"  (lbal |> Heap.content)   (ubal |> Heap.content)  

                                                         let median, lbal, ubal = Heap.getExtr lbal, lbal, ubal
                                                         Some (median, (i+1, lbal, ubal))
                                             ) 
                                             (0, HeapMax.empty, HeapMin.empty)

let rs = medians [|1;2;3;4;4;4;4;4|]
rs |> Seq.toList

stream |> Seq.length
let mstream = medians stream  |> Seq.toArray


