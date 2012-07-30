namespace DataStructure
 
open System


(** Notes

8 bits per byte
4 bytes to represent a signed integer

0xf = 15 * (2. ** 4.) = 16 ^- 1
0xff = 15 * (2. ** 4.) + 15 = 16 ^ 2 - 1
...
0xffff = 16 ^ 4 - 1
...
0xffffffff = 16 ^ 8 - 1

n <<< i = b * (2 ^ i)

n >>> i = b / (2 ^ i) as an integer division, i.e. 17 / 2 = 8

n &&& -b returns the value of the first bit Set to 1 in n,
i.e. [2 ^ pos] where [pos] is the index of the minimum Set bit.

b &&& i =  0 if the ith bit is not Set in n, ith's value otherwise
*)

type Bitarray =
  { Data : byte[]
    Length : int
  }
  static member (~~~) ba        =  { ba with Data = Array.map (fun e -> ~~~e) ba.Data }
  static member (^^^) (ba, ba') =  if ba.Length <> ba'.Length then
                                    invalidArg "ba" "input Bitarrays have different length"
                                   { ba with Data = Array.map2 (fun e e' -> e ^^^ e') ba.Data ba'.Data }
  static member (|||) (ba, ba') =  if ba.Length <> ba'.Length then
                                    invalidArg "ba" "input Bitarrays have different length"
                                   { ba with Data = Array.map2 (fun e e' -> e ||| e') ba.Data ba'.Data }
  static member (&&&) (ba, ba') =  if ba.Length <> ba'.Length then
                                    invalidArg "ba" "input Bitarrays have different length"
                                   { ba with Data = Array.map2 (fun e e' -> e &&& e') ba.Data ba'.Data }
  static member (+) (b:Bitarray, b') = b ||| b'
  static member (-) (b:Bitarray, b') = b &&& ~~~b'  
  
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bitarray =
  type t = Bitarray
  let byte_size = 7
  let shift_size = 3 

  let bitToByteShift nbits = nbits >>> shift_size
  let staticBits = Array.init (byte_size+1) (fun i -> byte(1 <<< i))

  let length ba = ba.Length
  let create nbits =
    if nbits < 0 then invalidArg "nbits" "create : negative number of elements"
    { Data = (nbits + 7) |> bitToByteShift |> Array.zeroCreate
      Length = nbits
    }

  let copy ba =
    { ba with Data = Array.copy ba.Data }

  let isSet pos ba =
    if pos < 0 || pos >= ba.Length then raise <| IndexOutOfRangeException()
    ba.Data.[bitToByteShift pos] &&& staticBits.[pos &&& byte_size] <> 0uy

  let set pos ba =
    if not <| isSet pos ba then
      let newValue = ba.Data.[bitToByteShift pos] ||| staticBits.[pos &&& byte_size]
      ba.Data.[bitToByteShift pos] <- newValue

  let setAll ba =
    ba.Data |> Array.iteri (fun i _ -> ba.Data.[i] <- 255uy)

  let unset pos ba =
    if isSet pos ba then
      let newValue = ba.Data.[bitToByteShift pos] ^^^ staticBits.[pos &&& byte_size]
      ba.Data.[bitToByteShift pos] <- newValue

  let unsetAll ba =
    ba.Data |> Array.iteri (fun i _ -> ba.Data.[i] <- 0uy)

  let put pos flag ba  =
    (if flag then set else unset) pos ba

  let init nbits f =
    let res = create nbits
    for i in 0 .. nbits - 1 do
      put i (f i) res
    res

  let clear ba =
    unsetAll ba 

  let inter (b:Bitarray) b' = b &&& b'

  let union (b:Bitarray) b' = b ||| b'

  let diff (negged:Bitarray) baseBitarray = baseBitarray ^^^ negged

  let iter f ba =
    let res = create (length ba)
    for i in 0 .. (length ba) - 1 do
      f <| isSet i ba   

  let iteri f ba =
    let res = create (length ba)
    for i in 0 .. (length ba) - 1 do
      f i <| isSet i ba      

  let mapi f ba =
    let res = create (length ba)
    for i in 0 .. length ba - 1 do
      put i (f i (isSet i ba)) res
    res     

  let map f ba =
    let res = create (length ba)
    for i in 0 .. length ba - 1 do
      put i (f (isSet i ba)) res
    res  

  let setBits ba =
    [ for i in 0 .. length ba - 1 do if isSet i ba then yield i ]

  let unsetBits ba =
    [ for i in 0 .. length ba - 1 do if not <| isSet i ba then yield i ]

  let toBool ba =
    [|for i in 0 .. length ba - 1 -> isSet i ba |]

  let fromBool xs =
    init (Array.length xs) (fun i -> xs.[i])    

  let toBytes ba =
    Array.copy ba.Data

  let fromBytes xs numBits =
    if (numBits + byte_size) / 8 <> (Array.length xs) then
      invalidArg "numBits" "fromBytes : data inconsistent with number of bits"
    { Data = Array.copy xs
      Length = numBits
    }

  let areAllSet ba =
    let nFull = ba.Length / 8
    let i = ref 0
    let ok = ref true
    //check bytes that could be full
    while !i < nFull && !ok do
      if ba.Data.[!i] <> 255uy then ok := false
      incr i
    //check remaining bytes
    if !ok then
      i := nFull * 8
      while !i < length ba && !ok do
        if not <| isSet !i ba then ok := false
        incr i
    !ok

  let areAllUnset ba =
    let nFull = ba.Length / 8
    let i = ref 0
    let ok = ref true
    //check bytes that could be full
    while !i < nFull && !ok do
      if ba.Data.[!i] <> 0uy then ok := false
      incr i
    //check remaining bytes
    if !ok then
      i := nFull * 8
      while !i < length ba && !ok do
        if isSet !i ba then ok := false
        incr i
    !ok