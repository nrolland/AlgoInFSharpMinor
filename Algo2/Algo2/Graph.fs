module Graph
  open Microsoft.FSharp.Collections

  type System.Collections.Generic.IEnumerable<'T> with member this.Random = let rnd = System.Random()
                                                                            fun () -> //printfn "random %A" this
                                                                                      this |> Seq.nth(rnd.Next(Seq.length this))
  let defaultArg def = function | Some x -> x | None -> def

  type VertexData<'V> (identifier:int,data:'V)=
    member val id = identifier with get,set
    member val data = data with get,set

  type EdgeData<'E> = { id:int;  weight :int; mutable targetVertex:int; data:'E}

  type Vertex<'V, 'E>(vertexData : VertexData<'V>, adjacency:EdgeData<'E> seq ) =
   let aadjacency = ResizeArray(adjacency)
   member val vertexData = vertexData with get,set
   member val adjacency = aadjacency with get

  let v ref refs = Vertex(VertexData<_>(fst ref,snd ref), refs |> Seq.map(fun s -> { id= 0;  weight=0; targetVertex= fst s; data=snd s})) 


  type MutableGraph<'V, 'E>  (vertexList:Vertex<'V, 'E> seq )=
   let vlist = ResizeArray(vertexList)

   new(arg:((int*'V)*(int*'E) seq) seq) = MutableGraph<_, _>(arg |> Seq.map(fun (ref,refs) -> v ref refs))
   member this.vertexList with get() = vlist
   member this.Copy() =
      let a  = vlist |> Seq.map(fun v -> ((v.vertexData.id, v.vertexData.data), v.adjacency |> Seq.map(fun a -> a.targetVertex, a.data)))
      MutableGraph<_,_>(a) 
   member this.contract() = 
      while vlist.Count > 2 do
         let v = vlist.Random()
         if v.adjacency.Count = 0 then ()
         else
            let edge = v.adjacency.Random()
            this.mergeVertex (v.vertexData.id, edge.targetVertex)
            vlist |> Seq.iter (fun v -> v.adjacency.RemoveAll(fun edge -> edge.targetVertex = v.vertexData.id) |> ignore)

   member this.vertex with get(id:int) = vlist.Find(fun v -> v.vertexData.id = id)

   member this.mergeVertex(oldid,withid,?mergedata) = 
      let fmergedata = defaultArg (fst) mergedata
      vlist |> Seq.iter(fun vertex -> vertex.adjacency |> Seq.iter(fun edge -> if edge.targetVertex = oldid 
                                                                               then edge.targetVertex <- withid ))
      let oldnode = vlist |> ResizeArray.tryFind( fun v -> v.vertexData.id = oldid) 
      if oldnode.IsSome then       
          let newnode = vlist |> ResizeArray.tryFind( fun v -> v.vertexData.id = withid) 
          if newnode.IsNone then       
            oldnode.Value.vertexData.id <- withid
          else
            newnode.Value.adjacency.AddRange(oldnode.Value.adjacency)
            newnode.Value.vertexData.data <- fmergedata (oldnode.Value.vertexData.data,newnode.Value.vertexData.data)
          vlist.RemoveAll(fun v -> v.vertexData.id = oldid) |> ignore

   member this.searchBF start =
      let visited = Set.empty
      let queue = 
      ()