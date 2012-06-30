#if INTERACTIVE
#load @"__solmerged.fsx"
#load @"App.fs"
#endif


if false then
   let f =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\IntegerArray.txt")

   let ar = f |> Array.map (double) 
   let lar = ar |> Array.toList

   ar  |> MainApp.mergesortcountmutable |> ignore
   //lar  |> MainApp.mergesSort stack overflow !


let f2 =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\QuickSort.txt")

let ar1 = f2 |> Array.map (double) 
MainApp.quickSortAndCount   ar1 MainApp.getpivot1
let ar2 = f2 |> Array.map (double) 
MainApp.quickSortAndCount   ar2 MainApp.getpivot2
let ar3 = f2 |> Array.map (double) 
MainApp.quickSortAndCount   ar3 MainApp.getpivot3

