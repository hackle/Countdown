let rec perm l =
    seq {
        for x in l do
            yield [x]
            for x' in perm (List.except [x] l) do
                yield x::x'
    }

type Value = V of int | Invalid with
    override x.ToString() =
        match x with
        | V v -> string v
        | _ -> ""

type Result = Value * string

type Op = Add | Sub | Mul | Div with
    override x.ToString() =
        match x with
        | Add _ -> "+"
        | Sub _ -> "-"
        | Mul _ -> "*"
        | Div _ -> "/"


type CalcTree = Single of Value | Complex of (CalcTree * CalcTree * Op)

let applyOp op l r =
    match op with
    | Add -> if l > r then Invalid else V (l + r)
    | Sub -> if l < r then Invalid else V (l - r)
    | Mul -> if l > r || l = 1 || r = 1 then Invalid else V (l * r)
    | Div -> if r = 0 || l < r || r = 1 || l % r <> 0 then Invalid else V (l / r)

// let applyOp op l r =
//     match op with
//     | Add -> V (l + r)
//     | Sub -> if l < r then Invalid else V (l - r)
//     | Mul -> V (l * r)
//     | Div -> if r = 0 || l % r <> 0 then Invalid else V (l / r)

let allOps = [ Add; Sub; Mul; Div ]

let apply (op: Op)  (l: Value) (r: Value) : Value =
    match l, r with
    | Invalid, _ -> Invalid
    | _, Invalid -> Invalid
    | V l', V r' -> applyOp op l' r'

let split xs =
    [ 1 .. (Seq.length xs - 1)]
    |> Seq.map (fun n -> Seq.take n xs, Seq.skip n xs)

let rec runCalcTree (ctree: CalcTree) =
    match ctree with
    | Single v -> v
    | Complex (lv, rv, op) -> apply op (runCalcTree lv) (runCalcTree rv)
let rec applySet (xs: Value list) : CalcTree seq =
    seq {
        match xs with
        | [x] -> if x <> Invalid then yield Single x
        | _ ->
            for l, r in split xs do
                match List.ofSeq l, List.ofSeq r with
                | ls, [] -> yield! applySet ls
                | [], rs -> yield! applySet rs
                | ls, rs -> 
                    for ltree in applySet ls do
                        for rtree in applySet rs do
                            for op in allOps do 
                                let tree = Complex (ltree, rtree, op)
                                let res = runCalcTree tree
                                if res <> Invalid then yield tree
    }

let ElementPosition = Left | Right
let printComplex lv rv op treeAbove pos =
    let shouldBracket = 
        match treeAbove with
        | Single _ -> false
        | Complex (lt, rt, opAbove) ->
            match op, opAbove with
            | Add, Add -> false
            | Add, _ -> true
            | Sub, Add -> false
            | Sub, _ -> true
            | Mul, Div -> true
            | Mul, _ -> false
            | Div, Div -> true
            | Div, _ -> false
        
    match shouldBracket with
    | true -> "(" + lv + (string op) + rv + ")"
    | false -> lv + (string op) + rv
let printTree (ctree: CalcTree) =
    let rec printTree' (ctree: CalcTree) (treeAbove: CalcTree) =
        match ctree with
        | Single v -> string v
        | Complex (lv, rv, op) -> printComplex (printTree' lv ctree) (printTree' rv ctree) op treeAbove
    printTree' ctree (Single Invalid)

// solve' 20 [1;2;3;4;6] |> Seq.map printTree |> Seq.iter (printfn "%s")
let solve' (target: int) (xs: int seq) =
    let t = V target
    seq {
        for calcTree in applySet (Seq.map V xs |> List.ofSeq) do
            if runCalcTree calcTree = t then yield calcTree
    }

let solve (target: int) (xs: int list) =
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()
    xs
    |> perm
    |> Seq.collect (fun s -> solve' target s)
    |> Seq.map printTree
    |> Seq.distinct
    |> Seq.indexed
    |> Seq.iter (printfn "%A")
    // |> Seq.iter (fun (i, v) -> printfn "%i %A" i v)
    sw.Stop()
    printfn "Time elapsed: %f" sw.Elapsed.TotalMilliseconds

// solve 20 [1;2;3;4;5]
// solve 250 [1; 3; 7; 10; 25; 50]
 // solve 765 [1; 3; 7; 10; 25; 50]