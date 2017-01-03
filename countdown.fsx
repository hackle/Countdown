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

type Calc = int -> int -> Value
type Op = Add | Sub | Mul | Div with
    override x.ToString() =
        match x with
        | Add _ -> "+"
        | Sub _ -> "-"
        | Mul _ -> "*"
        | Div _ -> "/"
let applyOp op l r =
    match op with
    | Add -> if l > r || l = 1 || r = 1 then Invalid else V (l + r)
    | Sub -> if l < r then Invalid else V (l - r)
    | Mul -> if l > r || l = 1 || r = 1 then Invalid else V (l * r)
    | Div -> if r = 0 || l < r || r = 1 || l % r <> 0 then Invalid else V (l / r)

let allOps = [ Add; Sub; Mul; Div ]

let apply (op: Op)  (l: Value) (r: Value) : Value =
    match l, r with
    | Invalid, _ -> Invalid
    | _, Invalid -> Invalid
    | V l', V r' -> applyOp op l' r'

let split xs =
    [ 1 .. (Seq.length xs - 1)]
    |> Seq.map (fun n -> Seq.take n xs, Seq.skip n xs)

let rec applySet (xs: Value list) : Result seq =
    seq {
        match xs with
        | [] -> () // this should never happen as empty sets are handled below
        | [x] -> yield x, string x
        | _ ->
            for l, r in split xs do
                match List.ofSeq l, List.ofSeq r with
                | ls, [] -> yield! applySet ls
                | [], rs -> yield! applySet rs
                | ls, rs -> 
                    for (lval, ldes) in applySet ls do
                        for (rval, rdes) in applySet rs do
                            for op in allOps do 
                                let res = apply op lval rval, "(" + (string ldes) + (string op) + (string rdes) + ")"
                                // throw away invalid results
                                match res with
                                | V v, _ -> yield res
                                | _, _ -> ()
    }

let solve' (target: int) (xs: int seq) =
    let t = V target
    seq {
        for (v, cs) in applySet (Seq.map V xs |> List.ofSeq) do
            if v = t then yield (System.String.Join("", cs))
    }

let solve (target: int) (xs: int list) =
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()
    xs
    |> perm
    |> Seq.collect (fun s -> solve' target s)
    // |> Seq.length
    // |> printfn "%i"
    |> Seq.indexed
    |> Seq.iter (fun (i, v) -> printfn "%i %A" i v)
    sw.Stop()
    printfn "Time elapsed: %f" sw.Elapsed.TotalMilliseconds

// solve 20 [1;2;3;4;5]
// solve 250 [1; 3; 7; 10; 25; 50]
 // solve 765 [1; 3; 7; 10; 25; 50]