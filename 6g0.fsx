let rec cfrac2float (lst:int list) : float =
  match lst with
    | [] -> 0.0
    | [int] -> float lst.Head
    | _ -> float lst.Head + 1.0 / (cfrac2float(lst.Tail))

let test = [1;2;3]
printfn "%A" (cfrac2float test)

(* let rec cfrac2string (lst:int list) : string =
  match lst with
    | [] -> "tom"
    | _ -> string lst.Head + " + 1 / " + cfrac2string lst.Tail

printfn "%A" (cfrac2string test) *)
