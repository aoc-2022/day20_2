open System.IO

let file = "/tmp/aoc/input"

type Id = int * int * int

let idVal ((_, _, v): Id) = v
let idOrig ((_, v, _): Id) = v

let ids1: Id list =
    File.ReadAllLines file
    |> Seq.map int
    |> Seq.indexed
    |> Seq.map (fun (i, v) -> (i, v, v))
    |> Seq.toList

let ids = ids1 |> List.map (fun (i,v1,v2) -> (i,v1,v2 % (ids1.Length-1)))
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

    override this.ToString() = $"{id |> idVal}"

let toNodes (ids: Id list) =
    let last: Id list = ids |> List.skip (ids.Length - 1)
    let first: Id list = [ ids.Head ]
    printfn $"last = {last}"
    printfn $"first = {first}"
    let ids = [ last; ids; first ] |> List.concat
    ids |> List.map (fun id -> $"{idVal id} ") |> String.concat " " |> printfn "%A"

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
    |> List.map idVal
    |> List.map (fun i -> $"{i}")
    |> String.concat " "
    |> fun s -> $"NODES: [{s} ] {nodes.Count}"

let nodes: Map<Id, Node> =
    toNodes ids |> List.map (fun node -> node.Id, node) |> Map.ofList

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

let nodes1 = moveId ids.Head nodes

let rec moveAll (nodes: NodeMap) (ids: Id list) =
    match ids with
    | [] -> nodes
    | id :: rest ->
        let nodes = moveId id nodes
        moveAll nodes rest

let nodesX = moveAll nodes ids

printfn $"{nodeMapToString nodesX}"

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

let res = getResult nodesX
printfn $"RESULT: {res}"

let nx = -20 % 7
printfn $"-10/7 = {nx}"

// zero=(694, 5000, 0)
// n1000 = (1226, -8811, -3811)
// n2000 = (3733, -5654, -654)
// n3000 = (4723, -1396, -1396)
// RESULT: -15861
// -10/7 = -6

// zero=(881, 0, 0)
// n1000 = (3577, 8861, 8861)
// n2000 = (2964, -4565, -4565)
// n3000 = (4525, 9047, 9047)
// RESULT: 13343
// -10/7 = -6

let test1 (nodes: NodeMap) =
    let zero: Id = nodes.Keys |> Seq.filter (fun id -> idOrig id = 0) |> Seq.head

    let rec jump (i: int) (curr: Id) : Id =
        if i = 0 then curr
        elif i > 0 then jump (i - 1) nodes[curr].Next
        else jump (i + 1) nodes[curr].Prev

    let oneRot = jump 5000 zero
    let oneRot2 = jump 5000 zero
    let node1 = jump -7628 zero
    let node2 = jump -2628 zero
    printfn $"nerfed: {oneRot} {oneRot2} not={node1} yes={node2}"

test1 (nodes)
