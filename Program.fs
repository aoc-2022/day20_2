open System.IO

let file = "/tmp/aoc/input"

type Id = int * int64 * int

let idVal ((_, _, v): Id) = v
let idOrig ((_, v, _): Id) = v

let idsFromFile : Id list =
    File.ReadAllLines file
    |> Seq.map int
    |> Seq.indexed
    |> Seq.map (fun (i, v) -> (i, v |> int64, v))
    |> Seq.toList

let toDecrypted (i,orig,v) =
    let key = 811589153L
    let orig = orig * key
    let v = orig % ((idsFromFile.Length-1) |> int64)
    let v = v |> int
    (i,orig,v)

let idsForTask1 = idsFromFile |> List.map (fun (i,v1,v2) -> (i,v1,v2 % (idsFromFile.Length-1)))
let idsForTask2 = idsFromFile |> List.map toDecrypted

// let ids = ids1

type Node(id: Id, prev: Id, next: Id, prev20: Option<Id>, next20: Option<Id>) =
    member this.Id = id
    member this.Value = idVal id
    member this.Prev = prev
    member this.Next = next
    member this.Prev20 = prev20
    member this.Next20 = next20

    member this.SetNext(next: Id) = Node(id, prev, next, prev20, next20)
    member this.SetPrev(prev: Id) = Node(id, prev, next, prev20, next20)

    override this.ToString() = $"{id |> idOrig}"

let toNodes (ids: Id list) =
    let last: Id list = ids |> List.skip (ids.Length - 1)
    let first: Id list = [ ids.Head ]
    printfn $"last = {last}"
    printfn $"first = {first}"
    let ids = [ last; ids; first ] |> List.concat
    ids |> List.map (fun id -> $"{idOrig id} ") |> String.concat " " |> printfn "%A"

    let rec toNodes (ids: Id list) =
        match ids with
        | prev :: curr :: next :: _ -> Node(curr, prev, next, None, None) :: (toNodes ids.Tail)
        | _ -> []

    toNodes ids

type NodeMap = Map<Id, Node>

let nodeMapToString (nodes: NodeMap) =
    let start = nodes.Keys |> Seq.head

    let rec ids (start: Id) (curr: Id) =
        if start = curr then
            []
        else
            curr :: (ids start nodes[curr].Next)

    (start :: (ids start (nodes[start].Next)))
    |> List.map idOrig
    |> List.map (fun i -> $"{i}")
    |> String.concat " "
    |> fun s -> $"NODES: [{s} ] {nodes.Count}"

let nodesTask1: Map<Id, Node> =
    toNodes idsForTask1 |> List.map (fun node -> node.Id, node) |> Map.ofList

let nodesTask2: Map<Id,Node> =
    toNodes idsForTask2 |> List.map (fun node -> node.Id, node) |> Map.ofList 

let moveId (id: Id) (nodes: NodeMap) : NodeMap =
    let node = nodes[id]
    let nodes = nodes.Add(node.Prev, nodes[ node.Prev ].SetNext(node.Next)) // removing this - but curr pos is after prev
    let nodes = nodes.Add(node.Next, nodes[ node.Next ].SetPrev(node.Prev))

    let rec jump (i: int) (curr: Id) : Id =
        if i = 0 then curr
        elif i > 0 then jump (i - 1) nodes[curr].Next
        else jump (i + 1) nodes[curr].Prev

    let target = jump node.Value node.Prev
    let after = nodes[target].Next
    let node = (node.SetPrev target).SetNext after
    let nodes = nodes.Add(target, nodes[ target ].SetNext id)
    let nodes = nodes.Add(after, nodes[ after ].SetPrev id)
    let nodes = nodes.Add(id, node)
    nodes

let rec moveAll (nodes: NodeMap) (ids: Id list) =
    match ids with
    | [] -> nodes
    | id :: rest ->
        let nodes = moveId id nodes
        moveAll nodes rest

let rec mixN (n:int) (nodes: NodeMap) (ids: Id list) =
    if n = 0 then nodes
    else
        let nodes = moveAll nodes ids 
        mixN (n - 1) nodes ids

let getResult (nodes: NodeMap) =
    let zero = nodes.Keys |> Seq.filter (fun id -> idOrig id = 0) |> Seq.head
    printfn $"zero={zero}"

    let rec jump (i: int) (curr: Id) : Id =
        if i = 0 then curr
        elif i > 0 then jump (i - 1) nodes[curr].Next
        else jump (i + 1) nodes[curr].Prev

    let n1000 = jump 1000 zero
    printfn $"n1000 = {n1000}"
    let n2000 = jump 1000 n1000
    printfn $"n2000 = {n2000}"
    let n3000 = jump 1000 n2000
    printfn $"n3000 = {n3000}"
    (idOrig n1000) + (idOrig n2000) + (idOrig n3000)

let resultNodesTask1 = moveAll nodesTask1 idsForTask1
printfn $"{nodeMapToString resultNodesTask1}"

let resultTask1 = getResult resultNodesTask1
printfn $"RESULT 1: {resultTask1}"

let resultNodesTask2 = mixN 10 nodesTask2 idsForTask2
printfn $"{nodeMapToString resultNodesTask2}"

let resultTask2 = getResult resultNodesTask2
printfn $"RESULT 2: {resultTask2}"


