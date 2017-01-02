open System
let rec perm l =
    seq {
        if Seq.isEmpty l then 
            yield []
        for x in l do
            for x' in perm (List.except [x] l) do
                yield x::x'
    }

let split n xs = Seq.take n xs, Seq.skip n xs
let split2 xs = 
    [ 1 .. (Seq.length xs) ]
    |> Seq.map (fun n -> split n xs)
let splitAll (xs: 'a seq) : 'a seq seq =
    xs
    |> Seq.map split2
    |> Seq.collect id
    |> Seq.collect (fun (l, r) -> l::r)
    |> Seq.filter (Seq.isEmpty>>not)

type Value = V of int | Invalid with
    override x.ToString() =
        match x with
        | V v -> string v
        | _ -> String.Empty

type Result = Value * string list

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
    | Add -> V (l + r)
    | Sub -> if l < r then Invalid else V (l - r)
    | Mul -> V (l * r)
    | Div -> if r = 0 || l % r <> 0 then Invalid else V (l / r)

let allOps = [ Add; Sub; Mul; Div ]

let apply (op: Op)  (l: Value) (r: Value) : Value =
    match l, r with
    | Invalid, _ -> Invalid
    | _, Invalid -> Invalid
    | V l', V r' -> applyOp op l' r'

let rec applySet (xs: Value seq) : Result seq =
    seq {
        for l, r in xs do
            match xs |> List.ofSeq with
            | [] -> yield Invalid, []
            | [x] -> yield x, [string(x)]
            | x::xs' ->
                for op in allOps do
                    for (v, cs) in applySet xs' do
                        yield apply op x v, string(x)::(string(op)::cs)
    }

let solve' (target: int) (xs: int seq) =
    let t = V target
    seq {
        for xs' in splitAll xs do
            for (v, cs) in applySet (Seq.map V xs') do
                if v = t then yield (System.String.Join("", cs))
    }

let solve (target: int) (xs: int list) =
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()
    xs
    |> perm
    |> Seq.collect (fun s -> solve' target s)
    |> Seq.indexed
    |> Seq.iter (fun (i, v) -> printfn "%i %A" i v)
    sw.Stop()
    printfn "Time elapsed: %f" sw.Elapsed.TotalMilliseconds

// solve 20 [1;2;3;4;5]
// solve 250 [1; 3; 7; 10; 25; 50]
 // solve 750 [1; 3; 7; 10; 25; 50]