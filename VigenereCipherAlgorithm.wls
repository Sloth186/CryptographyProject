#!/usr/bin/env wolframscript
(* ::Package:: *)

CaesarShift[m_Integer, k_Integer] :=
	If[Between[m, {65, 90}],
		Return[Mod[m - 65 + k, 26] + 65],
		If[Between[m, {97, 122}],
			Return[Mod[m - 97 + k, 26] + 97],
			Return[m]
		]
	]


VigenereCipherEncrypt[m_String, k_String] := Module[{mchars = ToCharacterCode[m], mlen = StringLength[m], kchars = LetterNumber[Characters[k]] - 1, klen = StringLength[k], i, cipher = {}},
	If[MemberQ[kchars, -1], Return["Error: key contains non-alphabetical characters"]];
	
	Do[AppendTo[cipher, CaesarShift[mchars[[i]], kchars[[Mod[i - 1, klen] + 1]]]]
	, {i, mlen}];
	Return[FromCharacterCode[cipher]]
]


VigenereCipherEncrypt["It was night again. The Waystone Inn lay in silence, and it was a silence of three parts.", "Kvothe"]
