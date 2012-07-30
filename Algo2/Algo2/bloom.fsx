#load "__solmerged.fsx"
 
open System
open DataStructure


let defaultArgs def = function | Some x -> x | None -> def

///
type Bloom (sizearray, ?setCount) = 
   let ar = Bitarray.create sizearray
   
   let k = match setCount with 
           |Some setcount -> 0.693 * float (sizearray / setcount) |> int
           |None -> 5

   let moduloPositive n k = ((k%n)+n)%n
   let generate = fun (el:'a) ->  [for i in 1 .. k -> i, el] |> Seq.map (hash >> (moduloPositive sizearray))

   member x.Add el = 
      generate el |> Seq.iter(fun h -> printfn "hash %A" h; Bitarray.set h ar )

   member x.Contains el =
      generate el |> Seq.forall(fun h -> Bitarray.isSet h ar )

   member x.Clear () = Bitarray.clear ar

let b = Bloom(10)
b.Add "Hi"
b.Contains "Hi2"

