//Da black-box test er en delmængde af white-box test, er dette inkluderet i følgende:

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

let rec frac2cfrac (t:int) (n:int) : int list =
  if n = 0 then
    []
  else
    let qi = t / n
    let ri = t % n
    qi :: (frac2cfrac n ri)


let rec cfrac2frac (lst: int list) (i: int) : int * int =
  let qi = lst.[i]
  if i = 0 then
    let ti = qi * 1 + 0
    let ni = qi * 0 + 1
    (ti,ni)
  elif i = 1 then
    let ti = qi * fst(cfrac2frac lst (i-1)) + 1
    let ni = qi * snd(cfrac2frac lst (i-1)) + 0
    (ti,ni)
  else
    let ti = qi * fst(cfrac2frac lst (i-1)) + fst(cfrac2frac lst (i-2))
    let ni = qi * snd(cfrac2frac lst (i-1)) + snd(cfrac2frac lst (i-2))
    (ti,ni)

printfn "White -box testing of 6g0.fsx"
printfn " Unit: cfrac2float"
printfn " Branch: 1a - %b" (cfrac2float [] = 0.0)
printfn " Branch: 2a - %b" (cfrac2float [1] = 1.0)
printfn " Branch: 2b - %b" (cfrac2float [20] = 20.0)
printfn " Branch: 3a - %b" (cfrac2float [1;2;3] = 10.0/7.0)
// Et kald til funktionen giver ikke nok signifikante decimaler, til at forventet output kan skrives

printfn "White-box testing of 6g1.fsx"
printfn " Unit: float2cfrac"
printfn " Branch: 1a - %b" (float2cfrac 0.0 = [0])
printfn " Branch: 1b - %b" (float2cfrac 3.0 = [3])
printfn " Branch: 2a - %b" (float2cfrac 3.263 = [3;3;1;4;17;3])
printfn " Branch: 2b - %b" (float2cfrac 2.521 = [2;1;1;11;2;2;8])

printfn "White -box testing of 6g2.fsx"
printfn " Unit: frac2cfrac"
printfn " Branch: 1a - %b" (frac2cfrac 243 0 = [])
printfn " Branch: 2a - %b" (frac2cfrac 649 182 = [3; 1; 1; 3; 3; 2; 3])
printfn " Branch: 2b - %b" (frac2cfrac 457 315 = [1; 2; 4; 1; 1; 2; 1; 1; 2])

printfn "White -box testing of 6g3.fsx"
printfn " Unit: cfrac2frac"
printfn " Branch: 1a - %b" (cfrac2frac [3;4;12;4] 0 = (3,1))
printfn " Branch: 2a - %b" (cfrac2frac [3;4;12;4] 1 = (13,4))
printfn " Branch: 3a - %b" (cfrac2frac [3;4;12;4] 3 = (649,200))
printfn " Branch: 3b - %b" (cfrac2frac [3;4;12;5] 3 = (808,249))
printfn " Branch: 3c - %b" (cfrac2frac [3;4;10;4] 3 = (545,168))
printfn " Branch: 3d - %b" (cfrac2frac [3;4;-12;4] 3 = (-599,-184))
printfn " Branch: 3f - %b" (cfrac2frac [3;4;-12;-4] 3 = (625,192))
