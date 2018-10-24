///<summary> Denne rekursive funktion tager udgangspunkt i et reelt tal. </summary>
///<param name="x"> Parameteret x er af typen float, der bliver brugt i omregningen til en liste af heltal. </param>
///<returns> Funktionen returnerer det reelle tals repræsentation i kædebrøken. </returns>

let rec float2cfrac (x: float) : int list =
  let qi = floor (System.Math.Round(x,5))
  let ri = x-qi
  let decimalri = decimal ri
  if System.Math.Round(ri,7)=0.0 then
    [int qi]
  else
    int qi :: float2cfrac(System.Math.Round(1.0/ri,15))

printfn "%A" ((float2cfrac 3.263) = [3;3;1;4;17;3])
printfn "%A" ((float2cfrac 2.521) = [2;1;1;11;2;2;8])
printfn "%A" ((float2cfrac 2.523) = [2; 1; 1; 10; 2; 1; 2; 2; 2])
printfn "%A" ((float2cfrac 2.543) = [2; 1; 1; 5; 3; 5; 2; 2])
printfn "%A" ((float2cfrac 3.527) = [3; 1; 1; 8; 1; 3; 6; 2])
printfn "%A" ((float2cfrac 3.528) = [3; 1; 1; 8; 2; 3])
printfn "%A" ((float2cfrac 3.529) = [3; 1; 1; 8; 8; 3; 2])
printfn "%A" ((float2cfrac 3.530) = [3;1;1;7;1;5])
printfn "%A" ((float2cfrac 3.540) = [3;1;1;5;1;3])
printfn "%A" ((float2cfrac 3.678) = [3;1;2;9;2;8])
printfn "%A" ((float2cfrac 3.679) = [3;1;2;8;1;2;12])
