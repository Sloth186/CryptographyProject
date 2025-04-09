#!/usr/bin/env wolframscript
(* ::Package:: *)

VigenereCipher[from_, key_, direction_:True] := Module[
{fChars = ToCharacterCode[ToUpperCase[from]], fLen = StringLength[from],
kChars = LetterNumber[Characters[key]] - 1, kLen = StringLength[key],
op = If[MemberQ[{True,"e",0}, direction], Plus, Subtract], i = 1, j = 1, to = {}},
	If[MemberQ[kChars, -1], Return["Error: key contains non-alphabet characters"]];
	
	While[i <= fLen,
		If[Between[fChars[[i]], {65, 90}],
			AppendTo[to, Mod[op[fChars[[i]], kChars[[j]]] - 65, 26] + 65];
			j = Mod[j, kLen] + 1,
			AppendTo[to, fChars[[i]]]
		];
		++i
	];
	
	FromCharacterCode[to]
]


VigenereCipher["ST OLAF COLLEGE", "LION"]
VigenereCipher["DB CYLN QBWTSTP", "LION", False]
VigenereCipher["ST OLAF COLLEGE", "LION", True]
VigenereCipher["ST OLAF COLLEGE", "LION", "e"]
VigenereCipher["ST OLAF COLLEGE", "LION", 0]
VigenereCipher["DB CYLN QBWTSTP", "LION", 1]
VigenereCipher["DB CYLN QBWTSTP", "LION", "d"]



