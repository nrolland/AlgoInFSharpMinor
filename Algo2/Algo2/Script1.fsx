#r "System.Xml.Linq"
open System
open System.IO
open System.Xml.Linq
open System.Text.RegularExpressions

let rec findsolutiondir (p:DirectoryInfo) = 
      if (p.GetFiles("*.sln") |> Array.length > 0) 
      then p
      else findsolutiondir p.Parent

let root = findsolutiondir (DirectoryInfo(__SOURCE_DIRECTORY__))

//It might need some manual cleaning of the resulting files but gets the job done
let refsforaproject (dirproject:DirectoryInfo) =   seq  {
         //TODO: this currently loads fsproj's in alphabeticall order, we should instead
         //build the dependencies graph of the fsproj's and load them in topological sort order
         for fsProjFile in dirproject.GetFiles("*.fsproj") do
                 let getElemName name = XName.Get(name, "http://schemas.microsoft.com/developer/msbuild/2003")
                 let getElemValue name (parent:XElement) =
                     let elem = parent.Element(getElemName name)
                     if elem = null || String.IsNullOrEmpty elem.Value then None else Some(elem.Value)

                 let getAttrValue name (elem:XElement) =
                     let attr = elem.Attribute(XName.Get name)
                     if attr = null || String.IsNullOrEmpty attr.Value then None else Some(attr.Value)

                 let (|??) (option1: 'a Option) option2 =
                     if option1.IsSome then option1 else option2

                 let fsProjFile = dirproject.GetFiles("*.fsproj") |> Seq.head
                 let fsProjXml = XDocument.Load fsProjFile.FullName

                 let refspath = 
                     fsProjXml.Document.Descendants(getElemName "Reference")
                     |> Seq.choose (fun elem -> getElemValue "HintPath" elem)
                     |> Seq.map (fun ref -> //printfn "%A %A %A" (dirproject.FullName)  (dirproject.FullName + ref) (DirectoryInfo(dirproject.FullName + ref).FullName)
                                            ("#r ", true,  DirectoryInfo(dirproject.FullName +  ".\\" + ref).FullName))

                 let refsgac = 
                     fsProjXml.Document.Descendants(getElemName "Reference")
                     |> Seq.choose (fun elem -> if (getElemValue "HintPath" elem).IsNone then getAttrValue "Include" elem else None)
                     |> Seq.map (fun ref -> ("#r ", false, ref))

                 let fsFiles = 
                     fsProjXml.Document.Descendants(getElemName "Compile")
                     |> Seq.choose (fun elem -> getAttrValue "Include" elem)
                     |> Seq.map (fun fsFile -> ("#load ", true, DirectoryInfo(dirproject.FullName +  ".\\" + fsFile).FullName))

                 let projDll = 
                     fsProjXml.Document.Descendants(getElemName "ProjectReference")
                     |> Seq.choose (fun elem -> getAttrValue "Include" elem)
                     |> Seq.map (fun projFile -> let refedPrjDir = DirectoryInfo(dirproject.FullName + "\\" + projFile).Parent
                                                 ("#r " ,  true, refedPrjDir.FullName +   "\\bin\\Debug\\" + refedPrjDir.Name + ".dll"))  //refedPrjDir.Name -> assembly name
                 yield! refspath
                 yield! refsgac
                 yield! projDll
                 yield! fsFiles
   }


let toabsolute root rel = DirectoryInfo(root + rel).FullName
let writerelative root path = 
      let rec intwriterelative (root:DirectoryInfo) (path:DirectoryInfo) first = 
         if path.FullName.Contains(root.FullName) then (if first then @".\" else @"") + path.FullName.Remove(0,root.FullName.Length)    //most common acestor = root
         else (if first then @".." else @"\..") + intwriterelative root.Parent path false
      intwriterelative root path true


let getprojectdir (root:DirectoryInfo) = 
   let rec getdirs (root:DirectoryInfo) = seq {
      yield! root.GetDirectories() |> Array.filter(fun f -> f.GetFiles("*.fsproj") |> Array.length > 0 ) 
      yield! root.GetDirectories() |> Array.map(fun d -> getdirs d) |> Seq.concat}
   
   getdirs root   

let merged writeRelativeTo  = seq { 
   for fsProj in getprojectdir root do
      yield! refsforaproject fsProj |> Seq.filter (fun (cmd, _, _) ->  cmd <> "#load ")
                                    |> Seq.map    (fun (cmd, isfile, abspath) ->  cmd +  "@\""  +  (if isfile then writerelative writeRelativeTo (DirectoryInfo(abspath)) else abspath)  +  "\"")
      }

for fsProj in getprojectdir root do
   File.WriteAllLines(fsProj.FullName+ "\\" + "__solmerged.fsx", merged fsProj |> Set.ofSeq)    


//   
//let testroot = @"C:\Users\e021230\Documents\Visual Studio 11\Projects\divsharp\"
//let testrel =  "..\\..\\..\\.\\..\\Documents\\Visual Studio 11\\Projects\\divsharp\\Financial\\office.dll"
//let testres = toabsolute testroot  testrel  // "C:\Users\e021230\Documents\Visual Studio 11\Projects\divsharp\Financial\office.dll"
//let absfile = DirectoryInfo(testroot + testrel).Parent.FullName
//let difile = DirectoryInfo("C:\Users\e021230\Documents\Visual Studio 11\Projects\office.dll")
//let diroot = DirectoryInfo(testroot)
//let difile1 = DirectoryInfo @"C:\Users\e021230\Documents\Visual Studio 11\"
//let difile2 = DirectoryInfo testrel
//let test = difile
//
//difile2.FullName = DirectoryInfo(diroot.FullName + "\\" + writerelative diroot difile2 ).FullName
//writerelative diroot difile
//writerelative diroot difile1
//writerelative diroot difile2
