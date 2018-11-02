///<summary> Den rekursive funktionen tager udgangspunkt i en liste af heltal. </summary>
///<param name="lst"> Dette parameter er en liste af integers. </param>
///<returns> Udfra listen af heltal returnerer funktionen det reelle tal. </returns>

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

printfn "%b" ((cfrac2float test0)=10.0/7.0)
printfn "%b" ((cfrac2float test1)=29.0/3.0)
printfn "%b" ((cfrac2float test2)=44.0/7.0)
//Da F# ikke skriver det præcise antal decimaler den forventer ved sammenligning, skriver vi den som brøk til sammenligningen.
