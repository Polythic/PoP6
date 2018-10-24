let rec cfrac2frac (lst: int list) (i: int) : int * int =
  let qi = lst.[i]
  if i = 0 then
    let ti = qi * 1 + 0
    let ni = qi * 0 + 1
    (ti,ni)
  else
    let ti = qi * fst(cfrac2frac lst (i-1)) + fst(cfrac2frac lst (i-2))
    let ni = qi * snd(cfrac2frac lst (i-1)) + snd(cfrac2frac lst (i-2))
    (ti,ni)

printfn "%A" (cfrac2frac [3;4;12;4] 3)
