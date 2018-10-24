let rec cfrac2float (lst:int list) : float =
  match lst with
    | [int] -> float lst.Head
    | _ -> float lst.Head + 1.0 / (cfrac2float(lst.Tail))

let test = [1;2;3]
printfn "%A" (cfrac2float test)
