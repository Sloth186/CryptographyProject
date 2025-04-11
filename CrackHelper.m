(* ::Package:: *)

books = {84, 2701, 1342, 1513, 11, 26184, 64317, 2542, 844, 100, 145, 174, 2641, 37106, 43, 11};


text = {};
Do[
	AppendTo[text, ToLowerCase[Import["https://www.gutenberg.org/cache/epub/" <> i <> "/pg" <> i <> ".txt"]]]
, {i, ToString /@ books}]
text = StringSplit[StringRiffle[text], RegularExpression["\\W+"]];
numWords = Length[text];
numChars = Plus @@ StringLength /@ text;


text
numWords
numChars


MonoFreq[text_, words_] := Module[
{monofrequencies = Table[0, 26], word, char},
	Do[
		Do[
			If[Between[char, {1, 26}],
				monofrequencies[[char]] += 1]
		, {char, word}]
	, {word, LetterNumber[text[[1;;words]]]}];
	
	monofrequencies
]

MonoFreq[text_] := MonoFreq[text, Length[text]]


monofrequencies = MonoFreq[text, 10000]
BarChart[monofrequencies, ChartLabels -> Alphabet[]]


DiFreq[text_, words_] := Module[
{difrequencies = Nest[Table[#, 26]&, 0, 2], word, i},
	Do[
		If[Length[word] > 1,
			For[i = 1, i < Length[word], ++i,
				If[And @@ (Between[#, {1, 26}]& /@ word[[i;;i+1]]),
					difrequencies[[word[[i]], word[[i+1]]]] += 1
				]
			]
		]
	, {word, LetterNumber[text[[1;;words]]]}];
	
	difrequencies
]


difrequencies = DiFreq[text, 10000];
barcharts = Table[BarChart[difrequencies[[i]], ChartLabels->Alphabet[], Frame->True, FrameLabel->{FromLetterNumber[i]}], {i, 26}]


TetraFreq[text_, words_] := Module[
{tetrafrequencies = Nest[Table[#,26]&,0,4], word, i, x},
	Do[
		If[Length[word] > 1,
			For[i = 1, i < Length[word] - 2, ++i,
				If[And @@ (Between[#, {1, 26}]& /@ word[[i;;i+3]]),
					tetrafrequencies[[word[[i]], word[[i+1]], word[[i+2]], word[[i+3]]]] += 1
				]
			]
		]
	, {word, LetterNumber[text[[1;;words]]]}];
	
	tetrafrequencies
]

TetraFreq[text_] := TetraFreq[text, Length[text]]


tetrafrequencies = TetraFreq[text, 1000]


For[i = 1, i <= 10, ++i,
	For[j = 1, j <= 10, ++j,
		For[k = 1, k <= 10, ++k,
			If[!AllMatch[tetrafrequencies[[i, j, k]], 0],
				Print[StringJoin[FromLetterNumber[{i, j, k}]] <> ": " <> ToString[tetrafrequencies[[i, j, k]]]]
			]
		]
	]
]



!AllMatch[tetrafrequencies[[20, 8, 5]],0]
