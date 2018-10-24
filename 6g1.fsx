let rec float2cfrac (x: float) : int list =
  let qi = floor x
  let ri = x-qi
  printfn "%A" ri
  match ri with
    | 0.0 -> []
    | _ -> int qi :: float2cfrac(1.0/ri)

printfn "%A" (float2cfrac 3.245)
