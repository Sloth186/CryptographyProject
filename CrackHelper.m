(* ::Package:: *)

(* Gutenberg URL numbers for each book *)
books = {84, 2701, 1342, 1513, 11, 26184, 64317, 2542, 844, 100, 145, 174, 2641, 37106, 43, 11};


(* Extracts raw text for each book from Gutenberg *)
text = {};
Do[
	(* Reduces all characters to lowercase and splits the extracted text into individual words, dropping punctuation *)
	(* Joins the resulting list to previously obtained text *)
	text = Join[text
		, StringSplit[
			ToLowerCase[Import[URL["https://www.gutenberg.org/cache/epub/" <> i <> "/pg" <> i <> ".txt"]]]
			, RegularExpression["\\W+"]]]
, {i, ToString /@ books}]
numWords = Length[text];
numChars = Plus @@ StringLength /@ text;


text
numWords
numChars


(* Counts the number of times each character appears across all text and sorts by said characters and divides by the total number of characters *)
monoFreqAll = KeySort[KeyMap[List, Map[#/numChars&, Counts[Characters[StringJoin[text]]]]]];
(* Obtains frequencies only for the standard 26 alphabetic letters *)
monoFreqAlpha = KeySelect[monoFreqAll, !MemberQ[LetterNumber[#], 0]&];


(* Bar chart to visualize monogram frequencies *)
BarChart[monoFreqAlpha, ChartLabels->Keys[monoFreqAlpha]]


(* Convert each word in the text to a list of characters *)
(* Select only the lists that have strictly more than 1 element - in other words, drop 1-letter words *)
(* Obtain all bigrams for each word *)
(* The above produces a list of lists, so flatten at the first level to make counting easier *)
bigrams = Flatten[Subsequences[#, {2}]& /@ Select[Characters[text], Length[#]>1&], 1]


(* Count the frequency of each digram, divided by the total number of bigrams *)
biFreqAll = KeySort[Map[#/Length[bigrams]&, Counts[bigrams]]];
(* Keep only digrams containing strictly alphabetical characters *)
biFreqAlpha = KeySelect[biFreqAll, !MemberQ[LetterNumber[#], 0]&];


(* Visualize the top 25 digrams *)
top25biFreqAlpha = Sort[biFreqAlpha, Greater][[1;;25]]
BarChart[top25biFreqAlpha, ChartLabels->StringJoin/@Keys[top25biFreqAlpha]]


(* Convert each word in the text to a list of characters *)
(* Select only the lists that have strictly more than 2 elements - in other words, drop 1-letter and 2-letter words *)
(* Obtain all trigrams for each word *)
(* The above produces a list of lists, so flatten at the first level to make counting easier *)
trigrams = Flatten[Subsequences[#, {3}]& /@ Select[Characters[text], Length[#]>2&], 1]


(* Count the frequency of each trigram, divided by the total number of trigrams *)
triFreqAll = KeySort[Map[#/Length[trigrams]&, Counts[trigrams]]];
(* Keep only trigrams containing strictly alphabetical characters *)
triFreqAlpha = KeySelect[triFreqAll, !MemberQ[LetterNumber[#], 0]&];


(* Convert each word in the text to a list of characters *)
(* Select only the lists that have strictly more than 3 elements - in other words, drop 1-letter, 2-letter, and 3-letter words *)
(* Obtain all tetragrams for each word *)
(* The above produces a list of lists, so flatten at the first level to make counting easier *)
tetragrams = Flatten[Subsequences[#, {4}]& /@ Select[Characters[text], Length[#]>3&], 1]


(* Count the frequency of each tetragram, divided by the total number of tetragrams *)
tetraFreqAll = KeySort[Map[#/Length[tetragrams]&, Counts[tetragrams]]];
(* Keep only tetragrams containing strictly alphabetical characters *)
tetraFreqAlpha = KeySelect[tetraFreqAll, !MemberQ[LetterNumber[#], 0]&];


(* Keep the 25 most frequent tetragrams and graph on a bar chart *)
top25tetra = Take[Sort[tetraFreqAlpha, Greater], 25]
BarChart[top25tetra, ChartLabels -> Placed[StringJoin /@ Keys[top25tetra], Below, Rotate[#, 60 Degree]&]]


(* Compress the monograms, bigrams, trigrams, and tetragrams and export as plaintext *)
Export[FileNameJoin[NotebookDirectory[], "monoFreqAlpha.txt"], Compress[monoFreqAlpha]];
Export[FileNameJoin[NotebookDirectory[], "biFreqAlpha.txt"], Compress[biFreqAlpha]];
Export[FileNameJoin[NotebookDirectory[], "triFreqAlpha.txt"], Compress[triFreqAlpha]];
Export[FileNameJoin[NotebookDirectory[], "tetraFreqAlpha.txt"], Compress[tetraFreqAlpha]];


(* Import and uncompress the -grams from GitHub *)
mono = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/monoFreqAlpha.txt"]];
bi = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/biFreqAlpha.txt"]];
tri = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/triFreqAlpha.txt"]];
tetra = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/tetraFreqAlpha.txt"]];


(* Join monograms, bigrams, trigrams, and tetragrams together, in descending order of frequency *)
fourgrams = Sort[AssociateTo[tetra, AssociateTo[tri, AssociateTo[bi, mono]]], Greater];


(* For fun - visualize top 50 -grams *)
BarChart[Take[fourgrams, 50], ChartLabels->StringJoin/@Take[Keys[fourgrams], 50], Frame->True, FrameLabel->{"Top 50 fourgrams"}]


Fitness[text_, freq_] := Module[
{T = Flatten[If[Length[#] < 4, {#}, Subsequences[#, {4}]]& /@ Characters[StringSplit[ToLowerCase[text], RegularExpression["\\W+"]]], 1]
, result = 0, i},
	Do[
		If[freq[i] == 0,
			result -= 15,
			result += Log[freq[i]]]
	, {i, T}];
	(result / Length[T]) // N
]


text = "There's no time to waste, in this famous goodbye. There's angels landing on the shore, so lay down with me. Let the river run dry, it's Sunday in this six day war. Smile darling, don't be sad. Stars are going to shine tonight, tell me where the good men go, before I wash away. Walk me down the old brick road, so I can die where I met you. Hold me like we're going home, turn your tears to rain. Bury me beautiful. Heaven knows, how I loved you.";
Fitness[text, fourgrams]
