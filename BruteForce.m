(* ::Package:: *)

Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/VigenereCipherAlgorithmShen.m"]


cipher = VigenereCipher["Ole the Lion", "K"]


VCBruteForceKey1[cipher_, showReject_:False, print_:False] := Module[
{key, decrypt, decryptSplit, wordCount, i, possibles = {}, rejected = {}},
	For[i = 1, i <= 26, ++i,
		key = ToUpperCase[FromLetterNumber[i]];
		
		If[print==True, Print["Checking key: " <> key]];
		decrypt = VigenereCipher[cipher, StringJoin[key], 1];

		If[print, Print["  Decrypted: " <> decrypt]];
		decryptSplit = StringSplit[decrypt, RegularExpression["\\W+"]];
		
		wordCount = Count[DictionaryWordQ /@ decryptSplit, True];
		If[wordCount > 0,
			AppendTo[possibles, {decrypt, key, wordCount}];
			If[print, Print["    Found possible word"]],
			AppendTo[rejected, {decrypt, key}];
			If[print, Print["    Did not find a word"]]]
	];
	
	If[Empty[possibles], Return["No possible plaintexts found"]];
	
	possibles = ReverseSortBy[possibles, Last][[All,1;;2]];
	
	If[showReject,
	<|"Possible" -> possibles, "Rejected" -> rejected|>
	, possibles]
]


VCBruteForceKey1[cipher]


cipher = VigenereCipher["We come from St. Olaf, we sure are the real stuff.", "o"];
Print["Cipher: ", cipher];
VCBruteForceKey1[cipher]


VCBruteForceKey2[cipher_, showReject_:False, print_:False] := Module[
{key, decrypt, decryptSplit, wordCount, i, j, possibles = {}, rejected = {}},
	For[i = 1, i <= 26, ++i,
		For[j = 1, j <= 26, ++j,
			key = StringJoin[ToUpperCase /@ FromLetterNumber /@ {i, j}];
			
			If[print==True, Print["Checking key: " <> key]];
			decrypt = VigenereCipher[cipher, key, 1];
	
			If[print, Print["  Decrypted: " <> decrypt]];
			decryptSplit = StringSplit[decrypt, RegularExpression["\\W+"]];
			
			wordCount = Count[DictionaryWordQ /@ decryptSplit, True];
			If[wordCount > 0,
				AppendTo[possibles, {decrypt, key, wordCount}];
				If[print, Print["    Found possible word"]],
				AppendTo[rejected, {decrypt, key}];
				If[print, Print["    Did not find a word"]]]
		]
	];
	
	If[Empty[possibles], Return["No possible plaintexts found"]];
	
	possibles = ReverseSortBy[possibles, Last][[1;;Min[Length[possibles],10],1;;2]];
	
	If[showReject,
	<|"Possible" -> possibles, "Rejected" -> rejected|>
	, possibles]
]


cipher = VigenereCipher["We come from St. Olaf, we sure are the real stuff.", "um"]
VCBruteForceKey2[cipher]


VCBruteForceKey3[cipher_, showReject_:False, print_:False] := Module[
{key, decrypt, decryptSplit, wordCount, i, j, k, possibles = {}, rejected = {}},
	For[i = 1, i <= 26, ++i,
		For[j = 1, j <= 26, ++j,
			For[k = 1, k <= 26, ++k,
				key = StringJoin[ToUpperCase /@ FromLetterNumber /@ {i, j, k}];
				
				If[print==True, Print["Checking key: " <> key]];
				decrypt = VigenereCipher[cipher, key, 1];
		
				If[print, Print["  Decrypted: " <> decrypt]];
				decryptSplit = StringSplit[decrypt, RegularExpression["\\W+"]];
				
				wordCount = Count[DictionaryWordQ /@ decryptSplit, True];
				If[wordCount > 0,
					AppendTo[possibles, {decrypt, key, wordCount}];
					If[print, Print["    Found possible word"]],
					AppendTo[rejected, {decrypt, key}];
					If[print, Print["    Did not find a word"]]]
			]
		]
	];
	
	If[Empty[possibles], Return["No possible plaintexts found"]];
	
	possibles = ReverseSortBy[possibles, Last][[1;;Min[Length[possibles],10],1;;2]];
	
	If[showReject,
	<|"Possible" -> possibles, "Rejected" -> rejected|>
	, possibles]
]


cipher = VigenereCipher["St. Olaf College", "YAH"]
VCBruteForceKey3[cipher]

