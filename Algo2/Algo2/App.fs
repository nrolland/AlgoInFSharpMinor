module MainApp

open System
open System.Windows
open System.Windows.Controls
open System.Collections.Generic
open FSharpx
open MSDN.FSharp
open System.Threading

let rnd = Random()
type Agent<'T> = MailboxProcessor<'T>
let printer = 
      MailboxProcessor.Start(fun x -> async {
      while true do 
         let! str = x.Receive()
         printfn "%s" str })
let printpost msg  = printer.Post (sprintf "%O | Id=%d, IsTP=%b, IsBg=%b|%s"  DateTime.Now Thread.CurrentThread.ManagedThreadId Thread.CurrentThread.IsThreadPoolThread Thread.CurrentThread.IsBackground  msg)
let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f() 
    timer.ElapsedTicks

// Algo measure
let shuffle (l:'a array) = 
   let ileft = LinkedList<int>(seq { 0 .. (l.Length - 1)})
   let rec pick (ar:'a array)  r = 
      match ileft.Count with | 0 -> r
                             | n -> let ik =   ileft |> Seq.nth (rnd.Next(n))
                                    ileft.Remove(ik) |> ignore
                                    pick ar (ar.[ik]::r)
   pick l  []

let timealgo2 algo examplegen n = 
   let ex = examplegen n
   let r = duration (fun () -> algo ex)
   int r

let buildperfarray timealgo nmax = 
   let rec one n = seq {
      let t =  timealgo  n
      yield (n,t)
      if t < 50000  &&  n < nmax then
         yield! one  (n * 3) }
   one 1 |> Seq.toArray

//test
let test  = timealgo2      (fun ar ->  ar |> List.sort) (fun n -> [|1 .. n|] |> shuffle) 10000
let test1 = buildperfarray (timealgo2 (fun ar ->  ar |> List.sort) (fun n -> [|1 .. n|] |> shuffle)) 10000 
let test2 = buildperfarray (timealgo2 (fun ar ->  ar |> Seq.sort)  (fun n -> [|1 .. n|] |> shuffle)) 10000 

//ALGOS
type algotrial = {Title:string; mainalgo:(int -> int ); nlimit:int}

let seqsortwcons s = let r = s |> Seq.sort
                     let a = r|> Seq.head
                     ()

let algos = [|  { Title = "List sort"           ; mainalgo = (timealgo2 (List.sort)     (fun n -> [|1 .. n|] |> shuffle)) ; nlimit = 10000 } 
                { Title = "Seq sort"            ; mainalgo = (timealgo2 (Seq.sort)      (fun n -> [|1 .. n|] |> shuffle)) ; nlimit = 10000 }  
                { Title = "Seq sort with cons"  ; mainalgo = (timealgo2 (seqsortwcons)  (fun n -> [|1 .. n|] |> shuffle)) ; nlimit = 10000 } 
                    |]

//GUI
type stringdisplay = {Algo:string}
let tostringable s = {Algo = s.ToString()}
type MainWindow = XAML<"MainWindow.xaml">
let loadWindow() =
   let window = MainWindow()
   let area = Charting.FSharpChart.Area [for x in 1 .. 10 -> sprintf "Column %i" x, x]
   let formsHost = new Forms.Integration.WindowsFormsHost(Child =  new Charting.ChartControl(area))
   window.Root.Title <- "Algorithm comparison"
   window.Graph.Children.Add(formsHost)
   window.Algos.ItemsSource <- algos
   window.Run.Click.Add(fun _ -> let usel = window.Algos.SelectedItem 
                                 if usel <> null then
                                    let sel = usel :?> algotrial
                                    let xy = buildperfarray (sel.mainalgo) (sel.nlimit)
                                    let area = Charting.FSharpChart.Line xy 
                                    let formsHost = new Forms.Integration.WindowsFormsHost(Child =  new Charting.ChartControl(area))
                                    window.Graph.Children.Add(formsHost) |> ignore
                                 else
                                    ()
                        )
                                    
   // Your awesome code code here and you have strongly typed access to the XAML via "window"
   
   window.Root

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore

