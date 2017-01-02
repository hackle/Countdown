open System
let rec perm l =
    seq {
        if Seq.isEmpty l then 
            yield []
        for x in l do
            for x' in perm (List.except [x] l) do
                yield x::x'
    }

let split (xs: int list) : int list list =
    [ 1 .. (List.length xs) ]
    |> List.collect (fun n -> [ List.take n xs; List.skip n xs ])
    |> List.filter (fun l -> List.length l > 1)

[<StructuredFormatDisplay("{AsString}")>]
type Value = V of int | Invalid with
    member x.ToString() =
        match x with
        | V v -> string v
        | _ -> String.Empty
    member x.AsString = x.ToString()

type Result = Value * string list
let add l r = V (l + r)
let sub l r = if l < r then Invalid else V (l - r)
let mul l r = V (l * r)
let div l r = if r = 0 || l % r <> 0 then Invalid else V (l / r)
let apply (op: int -> int -> Value)  (l: Value) (r: Value) : Value =
    match l, r with
    | Invalid, _ -> Invalid
    | _, Invalid -> Invalid
    | V l', V r' -> op l' r'

let allOps = [ add; sub; mul; div; ]
let rec applySet (xs: Value list) : Result seq =
    seq {
        match xs with
        | [] -> yield Invalid, []
        | [x] -> yield x, [string(x)]
        | x::xs' ->
            for op in allOps do
                for (v, cs) in applySet xs' do
                    yield apply op x v, string(x)::cs
    }

let solve' (xs: int list) (target: int) =
    let t = V target
    seq {
        for xs' in split xs do
            for (v, cs) in applySet (List.map V xs') do
                if v = t then yield (System.String.Join("", cs))
    }
