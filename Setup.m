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


text
numWords
numChars


(* Counts the number of times each character appears across all text and sorts by said characters and divides by the total number of characters *)
monoFreqAll = KeySort[KeyMap[List, Counts[Characters[StringJoin[text]]]]] / numChars;
(* Obtains frequencies only for the standard 26 alphabetic letters *)
monoFreqAlpha = KeySelect[monoFreqAll, !MemberQ[LetterNumber[#], 0]&];


(* Bar chart to visualize monogram frequencies *)
BarChart[monoFreqAlpha, ChartLabels->StringJoin /@ Keys[monoFreqAlpha]]


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
