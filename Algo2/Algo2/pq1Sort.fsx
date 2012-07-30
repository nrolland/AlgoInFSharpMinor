#if INTERACTIVE
#load @"__solmerged.fsx"
#load @"App.fs"
#endif



let ar =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\IntegerArray.txt") |> Array.map (double)  |> MainApp.mergesortcountmutable |> ignore
let f =  System.IO.File.ReadAllLines (@"\\psf\Home\Documents\pc\AlgoInFSharpMinor\Algo2\Algo2\QuickSort.txt") |> Array.map (double)
let r1 = MainApp.quickSortAndCount (f.Clone() :?> _) MainApp.getpivot1 
let r2 = MainApp.quickSortAndCount (f.Clone() :?> _) MainApp.getpivot2 
let r3 = MainApp.quickSortAndCount (f.Clone() :?> _) MainApp.getpivot3 
