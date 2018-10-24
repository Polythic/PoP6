let rec frac2cfrac (t:int) (n:int) : int list =
  if n = 0 then
    []
  else
    let qi = t / n
    let ri = t % n
    qi :: (frac2cfrac n ri)

printfn "%A" (frac2cfrac 649 182)
