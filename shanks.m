Shanks[g_, h_, p_] := Module[{n, baby, giant, i, j},
  If[! PrimeQ[p], Return["p is not a prime"]];
  If[! MemberQ[PrimitiveRootList[p], g], 
   Return["g is not a primitive root of p"]];
  n = Ceiling[Sqrt[MultiplicativeOrder[g, p]]];
  baby = Table[PowerMod[g, i, p], {i, 0, n}];
  giant = 
   Table[Mod[h*PowerMod[PowerMod[ModularInverse[g, p], n, p], j, p], 
     p], {j, 0, n}];
  Print[baby];
  Print[giant];
  For[i = 1, i <= n, ++i,
   For[j = 1, j <= n, ++j,
    If[baby[[i]] == giant[[j]], Return[(i - 1) + (j - 1)*n]]
    ]
   ]
  ]