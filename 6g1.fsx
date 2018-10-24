let rec float2cfrac (x: float) : int list =
  let qi = floor x
  let ri = x-qi
  printfn "%A" ri
  match ri with
    | 0.0 -> []
    | _ -> int qi :: float2cfrac(System.Math.Round(1.0/ri,7))

printfn "%A" (float2cfrac 3.245)
