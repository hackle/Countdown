type Value = V of int | Invalid with
    override x.ToString() =
        match x with
        | V v -> string v
        | _ -> ""

type Op = Add | Sub | Mul | Div with
    override x.ToString() =
        match x with
        | Add _ -> "+"
        | Sub _ -> "-"
        | Mul _ -> "*"
        | Div _ -> "/"
    
    member x.Apply l r =
        match x with
        | Add -> if l > r then Invalid else V (l + r)
        | Sub -> if l < r then Invalid else V (l - r)
        | Mul -> if l > r || l = 1 || r = 1 then Invalid else V (l * r)
        | Div -> if r = 0 || l < r || r = 1 || l % r <> 0 then Invalid else V (l / r)

type OperandPosition = Left | Right

type CalcTree = Leaf of Value | Node of (CalcTree * CalcTree * Op)

// unrefined
// let applyOp op l r =
//     match op with
//     | Add -> V (l + r)
//     | Sub -> if l < r then Invalid else V (l - r)
//     | Mul -> V (l * r)
//     | Div -> if r = 0 || l % r <> 0 then Invalid else V (l / r)


let applyOp (op: Op)  (l: Value) (r: Value) : Value =
    match l, r with
    | Invalid, _ -> Invalid
    | _, Invalid -> Invalid
    | V l', V r' -> op.Apply l' r'

let split xs =
    [ 1 .. (Seq.length xs - 1)]
    |> Seq.map (fun n -> Seq.take n xs, Seq.skip n xs)

let rec runCalcTree (ctree: CalcTree) =
    match ctree with
    | Leaf v -> v
    | Node (lv, rv, op) -> applyOp op (runCalcTree lv) (runCalcTree rv)


let allOps = [ Add; Sub; Mul; Div ]
let rec generateCalcs (xs: Value list) : CalcTree seq =
    seq {
        match xs with
        | [x] -> if x <> Invalid then yield Leaf x
        | _ ->
            for l, r in split xs do
                match List.ofSeq l, List.ofSeq r with
                | ls, [] -> yield! generateCalcs ls
                | [], rs -> yield! generateCalcs rs
                | ls, rs -> 
                    for ltree in generateCalcs ls do
                        for rtree in generateCalcs rs do
                            for op in allOps do 
                                let tree = Node (ltree, rtree, op)
                                let res = runCalcTree tree
                                if res <> Invalid then yield tree
    }

let findSolutions (target: int) (xs: int seq) =
    let t = V target
    seq {
        for calcTree in generateCalcs (Seq.map V xs |> List.ofSeq) do
            if runCalcTree calcTree = t then yield calcTree
    }

//************ for printing *********
let printNode lv rv op treeAbove fromPos =
    let shouldBracket = 
        match treeAbove with
        | Leaf _ -> false
        | Node (lt, rt, opAbove) ->
            match op, opAbove, fromPos with
            | Add, Sub, Left | Add, Add, _ -> false
            | Add, _, _ -> true
            | Sub, Sub, Left | Sub, Add, _ -> false
            | Sub, _, _ -> true
            | Mul, Div, Right -> true
            | Mul, _, _ -> false
            | Div, Div, Right -> true
            | Div, _, _ -> false
        
    match shouldBracket with
    | true -> "(" + lv + (string op) + rv + ")"
    | false -> lv + (string op) + rv
let printTree (ctree: CalcTree) =
    let rec printTree' (ctree: CalcTree) (treeAbove: CalcTree) (fromPos: OperandPosition) =
        match ctree with
        | Leaf v -> string v
        | Node (lv, rv, op) -> printNode (printTree' lv ctree Left) (printTree' rv ctree Right) op treeAbove fromPos
    printTree' ctree (Leaf Invalid) Left

//************ end for printing *********

let rec choices xs =
    seq {
        for x in xs do
            yield [x]
            for x' in choices (List.except [x] xs) do
                yield x::x'
    }

let findAllSolutions (target: int) (xs: int list) =
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()

    let results = 
        xs
        |> choices
        |> Seq.collect (fun s -> findSolutions target s)
        |> List.ofSeq
        
    sw.Stop()

    results
    |> List.map printTree
    |> List.distinct
    |> List.indexed
    |> List.iter (printfn "%A")

    printfn "Time used: %f" sw.Elapsed.TotalMilliseconds

// findAllSolutions 20 [1;2;3;4;5]
// findAllSolutions 250 [1; 3; 7; 10; 25; 50]
 // findAllSolutions 765 [1; 3; 7; 10; 25; 50]