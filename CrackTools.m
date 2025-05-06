(* ::Package:: *)

(* Import and uncompress the -grams from GitHub *)
mono = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/monoFreqAlpha.txt"]];
bi = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/biFreqAlpha.txt"]];
tri = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/triFreqAlpha.txt"]];
tetra = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/tetraFreqAlpha.txt"]];

(* Join monograms, bigrams, trigrams, and tetragrams together, in descending order of frequency *)
fourgrams = Sort[AssociateTo[tetra, AssociateTo[tri, AssociateTo[bi, mono]]], Greater]


(* Calculates how closely a piece of text resembles English - the closer to 0, the better *)
Fitness[text_] := Module[{
T = Flatten[
		If[Length[#] < 4, {#}, Subsequences[#, {4}]]&
			/@ Characters[StringSplit[ToLowerCase[text], RegularExpression["[^a-z]+"]]]
	, 1]
,
result = 0, i},
	Do[
		If[!KeyExistsQ[fourgrams, i] || fourgrams[i] == 0,
			result -= 15,
			result += Log[fourgrams[i]]]
	, {i, T}];
	(result / Length[T]) // N
]
