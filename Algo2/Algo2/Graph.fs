namespace DataStructure

module Graph = 
  open Microsoft.FSharp.Collections
  open System.Collections.Generic

  type System.Collections.Generic.IEnumerable<'T> with 
   member this.Random = 
      let rnd = System.Random()
      fun () -> this |> Seq.nth(rnd.Next(Seq.length this))

  let defaultArg def = function | Some x -> x | None -> def
  type  IContainer<'a> = 
    abstract Count : int
    abstract Next  : unit -> 'a
    abstract Stage : 'a -> unit  

  [<CustomEquality;NoComparison>]
  type Edge = { startVertex:int;  weight :int; mutable targetVertex:int }
                     override x.Equals other = let o = (other :?> Edge)
                                               x.startVertex = o.startVertex && x.targetVertex = o.targetVertex
                     override x.GetHashCode() = hash (x.startVertex,x.targetVertex) 
  [<CustomEquality;NoComparison>]
  type VertexAdj = { mutable id : int; adjacency:HashSet<Edge> }
                     override x.Equals other = x.id = (other :?> VertexAdj).id
                     override x.GetHashCode() = hash x.id

  let v id refs = { id= id ; adjacency = HashSet(refs |> Seq.map(fun s -> { startVertex= id; weight=0; targetVertex= s})) }
  let v2 id refs = { id= id ; adjacency = HashSet(refs |> Seq.map(fun (t,w) -> { startVertex= id; weight=w; targetVertex= t})) }

  type MutableGraph  =
   val private vlist : Dictionary<int, VertexAdj> //todo transform to hashmultimap
   new((arg:(int*int seq) seq) ) = { vlist = arg |> Seq.fold (fun d (ref,refs) -> (d.Add(ref, v ref refs);d)) (Dictionary<int, VertexAdj>()) }
   new((arg:(int*(int*int) seq) seq) ) = { vlist = arg |> Seq.fold (fun d (ref,refs) -> (d.Add(ref, v2 ref refs);d)) (Dictionary<int, VertexAdj>()) }
                                  
        
                                   
   new(h:Dictionary<_,_>) = { vlist = h }
   override this.ToString() =  String.concat "\n" (this.vlist |> Seq.map (fun it -> it.ToString()))
   member this.vertexList with get() = this.vlist
   member this.Copy() =
      let a  = this.vlist |> Seq.map(fun v -> ((v.Value.id), v.Value.adjacency |> Seq.map(fun a -> a.targetVertex)))
      MutableGraph(a) 

   member this.nodes = 
      let a = seq { for v in this.vlist do
                     yield v.Value.id
                     yield! v.Value.adjacency |> Seq.map(fun e -> e.targetVertex)
                  } |> Set.ofSeq
      fun () -> a
   member this.contract() = 
      while this.vlist.Count > 2 do
         let v = this.vlist.Random().Value
         if v.adjacency.Count = 0 then ()
         else
            let edge = v.adjacency.Random()
            this.mergeVertex (v.id, edge.targetVertex)
            this.vlist |> Seq.iter (fun v -> v.Value.adjacency.RemoveWhere(fun edge -> edge.targetVertex = edge.startVertex ) |> ignore)

   member this.tryFind id =
      if this.vlist.ContainsKey id then
         Some this.vlist.[id]
      else
         None

   member this.mergeVertex(oldid,withid) = 
      this.vlist.Values |> Seq.iter(fun vertexAdj -> vertexAdj.adjacency 
                                                     |> Seq.iter(fun edge -> 
                                                         if edge.targetVertex = oldid 
                                                         then edge.targetVertex <- withid ))
      match this.tryFind oldid with
      | Some oldnode ->   
          match  this.tryFind withid with 
          | Some newnode -> newnode.adjacency.UnionWith(oldnode.adjacency)
          | _            -> oldnode.id <- withid 

      | _ -> this.vlist.Remove(oldid) |> ignore

   member this.searchInt (visited:HashSet<_>) (container:IContainer<_>)  =
      let lastprint = ref 0
      let n = ref 0

      let rec visit () = seq {
         if container.Count > 0 then
            let nxt = container.Next()
            if not(visited.Contains nxt) then
               yield nxt
               visited.Add nxt |> ignore

               if this.vlist.ContainsKey(nxt) then 
                  let node =  this.vlist.[nxt]
                  node.adjacency |> Seq.iter(fun it ->  container.Stage it.targetVertex) 
               //else printfn "node #%A only mentionned in edges of some other" nxt
            incr n
            if !n - !lastprint > 1000 then lastprint := !n
                                           printfn "tested %A nodes, visited %A, in queue %A" !lastprint visited.Count container.Count
            yield! visit()
         }
      visit()

   member this.searchBF (startid,?visited) =
      let queue = Queue()
      let visited = defaultArg (HashSet())  visited

      queue.Enqueue(startid)
      this.searchInt visited
                     { new IContainer<_> with 
                           member x.Count = queue.Count
                           member x.Next () = queue.Dequeue()
                           member x.Stage e = queue.Enqueue e  } 
      |> Seq.cache
 
   member this.searchDF (startid,?visited)  =
     let stack = System.Collections.Generic.Stack<_>()
     let visited = defaultArg (HashSet())  visited

     stack.Push(startid)
     this.searchInt visited
                    { new IContainer<_> with 
                           member x.Count = stack.Count
                           member x.Next () = stack.Pop()
                           member x.Stage e = stack.Push e  }       
     |> Seq.cache

      
   member this.incoming targetid = 
      let a = this.vlist.Values |> Seq.choose(fun it -> 
               let inward =  it.adjacency |> Seq.filter (fun it -> it.targetVertex = targetid) 
               if inward |> Seq.length > 0 then Some(it, inward) else None )
      a |> Seq.map(fun (node, edges) -> edges |> Seq.map (fun edge -> node, edge)) |> Seq.concat

   member this.reversenaive = 
      let a = this.vlist.Values |> Seq.map(fun it -> 
               let incoming = this.incoming (it.id)
               (it.id), incoming |> Seq.map (fun (node,edge) -> node.id ))
      MutableGraph(a) 
 
   member this.reverse = 
      let vertexlist = Dictionary<_,_>()

      this.nodes () |> Seq.map(fun i -> i, {id = i; adjacency= HashSet<_>() })
                    |> Seq.iter(fun (i,v) -> vertexlist.Add(i,v))

      this.vlist.Values |> Seq.iter (fun va -> va.adjacency 
                                                |> Seq.iter(fun e -> 
                                                      let newedge = {e with startVertex = e.targetVertex ; targetVertex = e.startVertex}
                                                      vertexlist.[e.targetVertex].adjacency.Add(newedge) |> ignore
                                                )
                                  )
      MutableGraph(vertexlist)

   member this.loopDFS (order:int seq) =
      let lorder = LinkedList<_>(order)
      let visited = HashSet()
      let lastprint = ref -1000

      let rec loop() = seq {
         match lorder.Count with 
         | 0 -> ()
         | _ -> let newlyvisited = this.searchDF (lorder.First.Value, visited)
                newlyvisited |> Seq.iter (lorder.Remove >> ignore)
                if visited.Count - !lastprint > 1000 then lastprint :=  visited.Count
                                                          printfn "visited %A nodes" !lastprint
                yield newlyvisited
                yield! loop()
      }
      loop() |> Seq.cache

   member this.SCCKosaraju =
      printfn "******reversing"
      let ginv = this.reverse
      printfn "******computing order"
      let t = ginv.loopDFS (this.nodes()) |> Seq.concat |> List.ofSeq 
      printfn "forward %A" (t|> Seq.take 5)
      let depthseq = t|> List.rev
      printfn "backward  %A" (depthseq|> Seq.take 5)
      printfn "******computing scc"
      
      this.loopDFS (depthseq)

   member this.computeShortest fromid =
      let explored = Dictionary<int,int>()
      let unexplored = HashSet(this.nodes())

      let addexplored id dist = 
         unexplored.Remove(id) |> ignore
         explored.Add(id,dist)

      let distance edge  =  explored.[edge.startVertex] + edge.weight
      
      addexplored fromid 0

      while unexplored.Count > 0 do 
         let outer = explored |> Seq.filter(fun n -> this.vertexList.ContainsKey(n.Key)) //qui ont des outgoing edges
                              |> Seq.map(fun n -> this.vertexList.[n.Key].adjacency)
                              |> Seq.concat
                              |> Seq.filter(fun e -> unexplored.Contains(e.targetVertex))

         let nxt = outer |> Seq.map (fun it-> it, distance it) 
                         |> Seq.minBy snd

         addexplored (fst nxt).targetVertex (snd nxt)
      explored
