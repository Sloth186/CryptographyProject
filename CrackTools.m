(* ::Package:: *)

(* Import and uncompress the -grams from GitHub *)
mono = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/monoFreqAlpha.txt"]];
bi = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/biFreqAlpha.txt"]];
tri = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/triFreqAlpha.txt"]];
tetra = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/tetraFreqAlpha.txt"]];

(* Join monograms, bigrams, trigrams, and tetragrams together, in descending order of frequency *)
fourgrams = Sort[AssociateTo[tetra, AssociateTo[tri, AssociateTo[bi, mono]]], Greater];


(* Calculates how closely a piece of text resembles English - the closer to 0, the better *)
Fitness[text_] := Module[{
	T = Flatten[
		If[Length[#] < 4, {#}, Subsequences[#, {4}]]&
			/@ Characters[StringSplit[ToLowerCase[text], RegularExpression["[^a-z]+"]]]
	, 1], result = 0, i},
	
	Do[
		If[!KeyExistsQ[fourgrams, i] || fourgrams[i] == 0,
			result -= 15,
			result += Log[fourgrams[i]]]
	, {i, T}];
	(result / Length[T]) // N
]


(* Helper function to slice a text into n parts, where each part contains every nth character *)
Slice[list_, n_] := Module[{sliced = Table[{}, n], length = Length[list]},
	Do[AppendTo[sliced[[Mod[i, n, 1]]], list[[i]]], {i, length}];
	sliced
]


(* Measures the likelihood that any two characters of a text are the same *)
Period[text_] := Module[{cleaned, length, ioc, found, period},
	cleaned = Characters[StringReplace[ToLowerCase[text], RegularExpression["[^a-z]"] -> ""]];
	length = Length[cleaned];

	ioc = 0;
	period = 0;
	While[ioc <= 1.7 && period < Floor[length / 2],
		period++;
		slices = Slice[cleaned, period];
		sum = 0;
		Do[sum += 26 * Sum[j * (j - 1), {j, Values[Counts[i]]}] / (Length[i] * (Length[i] - 1))
		, {i, slices}];
		ioc = sum / period;
	];
	
	period
]
