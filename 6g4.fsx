let rec cfrac2float (lst:int list) : float =
  match lst with
    | [] -> 0.0
    | [int] -> float lst.Head
    | _ -> float lst.Head + 1.0 / (cfrac2float(lst.Tail))

let rec float2cfrac (x: float) : int list =
  let qi = floor (System.Math.Round(x,5))
  let ri = x-qi
  let decimalri = decimal ri
  if System.Math.Round(ri,7)=0.0 then
    [int qi]
  else
    int qi :: float2cfrac(System.Math.Round(1.0/ri,15))

printfn "White -box testing of 6g0.fsx"
printfn " Unit: cfrac2float"
printfn " Branch: 1a - %b" (cfrac2float [] = 0.0)
printfn " Branch: 2a - %b" (cfrac2float [1] = 1.0)
printfn " Branch: 2b - %b" (cfrac2float [20] = 20.0)
printfn " Branch: 3a - %A" (cfrac2float [1;2;3] = 10.0/7.0)
// Et kald til funktionen giver ikke nok signifikante decimaler, til at forventet output kan skrives

printfn "White-box testing of 6g1.fsx"
printfn " Unit: float2cfrac"
printfn " Branch: 1a - %b" (float2cfrac 0.0 = [0])
