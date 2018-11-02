///<summary> Denne rekursive funktion tager udgangspunkt i en kædebrøk og et indeks. </summary>
///<param name="lst"> Er en liste af heltal. </param>
///<param name="i"> Dette parameter er af typen heltal, som viser hvilket "skridt" vi er på i vores udregning. </param>
///<returns> Funktionen returnerer derefter ti/ni approximationen som en tupel (ti, ni)</returns>

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

printfn "%A" (cfrac2frac [3;4;12;4] 0)
printfn "%A" (cfrac2frac [3;4;12;4] 1)
printfn "%A" (cfrac2frac [3;4;12;4] 2)
printfn "%A" (cfrac2frac [3;4;-12;-4] 3)
