///<summary> Denne rekursive funktion begynder indledningsvist med at tage en tæller og en nævner i en brøk. </summary>
///<param name="t"> Er brøkens tæller og er af typen heltal. </param>
///<param name="n"> Er brøkens nævner og er af typen heltal. </param>
///<returns> Med denne tæller og nævner omregner funktionen denne brøk
///til dens repræsentation i kædebrøken ved hjælp af heltalstyper.</returns>

let rec frac2cfrac (t:int) (n:int) : int list =
  if n = 0 then
    []
  else
    let qi = t / n
    let ri = t % n
    qi :: (frac2cfrac n ri)

printfn "%A" (frac2cfrac 649 182)
printfn "%A" (frac2cfrac 457 315)
printfn "%b" ((frac2cfrac 649 182)=[3; 1; 1; 3; 3; 2; 3])
printfn "%b" ((frac2cfrac 457 315)=[1; 2; 4; 1; 1; 2; 1; 1; 2])
