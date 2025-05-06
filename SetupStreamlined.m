(* ::Package:: *)

books = {3, 11, 43, 45, 76, 84, 98, 100, 145, 158, 174, 219,
345, 394, 408, 768, 844, 1080, 1184, 1232, 1259, 1260, 1342, 1400,
1513, 1661, 1952, 1998, 2000, 2160, 2542, 2554, 2591, 2600, 2641, 2701,
3207, 4085, 4300, 4363, 5197, 5200, 6593, 6761, 16389, 25344, 26184, 28054,
37106, 41445, 64317, 67979};

(* Extracts raw text for each book from Gutenberg *)
text = {};
Do[
	(* Grab book text from Project Gutenberg and lowercase everything *)
	bookText = ToLowerCase[Import[URL["https://www.gutenberg.org/cache/epub/" <> i <> "/pg" <> i <> ".txt"]]];
	(* Split the text into words and add to existing list of text *)
	text = Join[text, StringSplit[bookText, RegularExpression["[^a-z]+"]]]
, {i, ToString /@ books}]
numWords = Length[text];
numChars = Plus @@ StringLength /@ text;
 
monoFreqAlpha =
	KeySelect[
		KeySort[
			KeyMap[
				List,
				Map[
					#/numChars&,
					Counts[Characters[StringJoin[text]]]
				]
			]
		],
		!MemberQ[LetterNumber[#], 0]&
	];

bigrams = Flatten[Subsequences[#, {2}]& /@ Select[Characters[text], Length[#]>1&], 1];
biFreqAlpha =
	KeySelect[
		KeySort[Map[# / Length[bigrams]&, Counts[bigrams]]],
		!MemberQ[LetterNumber[#], 0]&
	];
Clear[bigrams];

trigrams = Flatten[Subsequences[#, {3}]& /@ Select[Characters[text], Length[#]>2&], 1];
triFreqAlpha =
	KeySelect[
		KeySort[Map[#/Length[trigrams]&, Counts[trigrams]]],
		!MemberQ[LetterNumber[#], 0]&
	];
Clear[trigrams];

tetragrams = Flatten[Subsequences[#, {4}]& /@ Select[Characters[text], Length[#]>3&], 1];
tetraFreqAlpha =
	KeySelect[
		KeySort[Map[#/Length[tetragrams]&, Counts[tetragrams]]],
		!MemberQ[LetterNumber[#], 0]&
	];
Clear[tetragrams];

Export[FileNameJoin[NotebookDirectory[], "monoFreqAlpha.txt"], Compress[monoFreqAlpha]];
Export[FileNameJoin[NotebookDirectory[], "biFreqAlpha.txt"], Compress[biFreqAlpha]];
Export[FileNameJoin[NotebookDirectory[], "triFreqAlpha.txt"], Compress[triFreqAlpha]];
Export[FileNameJoin[NotebookDirectory[], "tetraFreqAlpha.txt"], Compress[tetraFreqAlpha]];
