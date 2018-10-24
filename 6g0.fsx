let rec cfrac2float (lst:int list) : float =
  match lst with
    | [] -> 0.0
    | [int] -> float lst.Head
    | _ -> float lst.Head + 1.0 / (cfrac2float(lst.Tail))

let test0 = [1;2;3]
let test1 = [9;1;2]
let test2 = [6;3;1;1]
printfn "%A" (cfrac2float test0)
printfn "%A" (cfrac2float test1)
printfn "%A" (cfrac2float test2)
