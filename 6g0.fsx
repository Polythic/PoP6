let rec cfrac2float (lst:int list) : float =
  match lst with
    | [] -> 0.0
    | _ -> float lst.Head + 1.0 / (cfrac2float(lst.Tail))

let test = [1;2]
printfn "%A" (cfrac2float test)
