(* ::Package:: *)

(* ::Title:: *)
(*Cryptography Final Project*)


(* ::Subtitle:: *)
(*Vigen\[EGrave]re Cipher*)
(*Shen Rothermel, Jessica Schmidt*)


(* ::Section:: *)
(*Encryption/Decryption*)


(* ::Text:: *)
(*This function does both encryption and decryption. Provide it with the text and the key to encrypt. To decrypt, add a third argument that is anything other than True or 1.*)


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


(* ::Section:: *)
(*Frequency Analysis*)


(* ::Text:: *)
(*The second following chunk below builds a frequency table. Or you can just run the chunk right below to import the already-built frequency table from my GitHub, because building it takes a few minutes.*)


(* Import and uncompress the -grams from GitHub *)
mono = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/monoFreqAlpha.txt"]];
bi = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/biFreqAlpha.txt"]];
tri = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/triFreqAlpha.txt"]];
tetra = Uncompress[Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/tetraFreqAlpha.txt"]];

(* Join monograms, bigrams, trigrams, and tetragrams together, in descending order of frequency *)
fourgrams = Sort[AssociateTo[tetra, AssociateTo[tri, AssociateTo[bi, mono]]], Greater];


(* ::Subsection::Closed:: *)
(*Frequency Table Construction*)


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


(* ::Section:: *)
(*Helper Tools*)


(* ::Text:: *)
(*Below are three helper functions for cracking the Vigen\[EGrave]re cipher. Fitness and Period are explained in the paper. Slice just partitions any text into m groups where each group contains every mth letter. You should run all three chunks before moving onto the next section.*)


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


(* Calculates what must have been the length of the key used to encrypt a piece of text through the Vigenere cipher *)
Period[text_] := Module[{cleaned, length, ioc, found, period},
	cleaned = Characters[StringReplace[ToLowerCase[text], RegularExpression["[^a-z]"] -> ""]];
	length = Length[cleaned];

	ioc = 0;
	period = 0;
	While[ioc <= 1.7 && period < Floor[length / 2],
		period++;
		slices = Slice[cleaned, period];
		sum = 0;
		(* Calculates the index of coincidence for each sliced group, which 
		measures the likelihood that any two characters of a text are the same *)
		Do[sum += 26 * Sum[j * (j - 1), {j, Values[Counts[i]]}] / (Length[i] * (Length[i] - 1))
		, {i, slices}];
		ioc = sum / period;
	];
	
	period
]


(* ::Section:: *)
(*Your Inputs*)


(* ::Text:: *)
(*Enter the plaintext message and key you want to use here! Also enter a time limit (in seconds) for functions, if you want them to abort after a certain amount of time. This is mostly relevant for brute force. The key should be no alphabetical letters, no spaces. Casing does not matter. The message should be fairly long (I do not have any advice on how long...). I recommend choosing a 2-letter or 3-letter key for brute force, and then a longer key for the other two methods.*)


messageText = "Four score and seven years ago our fathers brought forth, on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.
Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.
But, in a larger sense, we can not dedicate\[LongDash]we can not consecrate\[LongDash]we can not hallow\[LongDash]this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us\[LongDash]that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion\[LongDash]that we here highly resolve that these dead shall not have died in vain\[LongDash]that this nation, under God, shall have a new birth of freedom\[LongDash]and that government of the people, by the people, for the people, shall not perish from the earth.";
key = "NO";
timeLimit = 600;
cipherText = VigenereCipher[messageText, key];
StringTake[cipherText, UpTo[50]]


(* ::Text:: *)
(*See the period function calculate what the key length of your ciphertext is. This will be less accurate for shorter messages. If you enter your own message, check that the output of this function is correct, and if not, make your message longer.*)


Period[cipherText]


(* ::Section:: *)
(*Brute Force*)


(* ::Text:: *)
(*Brute force, nothing special. If you wanna try it out, first run the chunk below to define the functions. Then enter your plaintext message and your key in the second chunk, and modify the second argument in TimeConstrained if you wish (currently, it will abort the function if 5 minutes have passed). I recommend keeping your key no longer than 3 letters.*)


(* ::Text:: *)
(*Crack methods in this project are setup to return the most likely keys, providing an association of possible keys corresponding to what the fitness of the decrypted text was. 99% of the time the top or second top result will be the correct key.*)


BruteForce[cipherText_] := Module[{keyLength, plainText, possibleKeys = <||>},
	keyLength = Period[cipherText];
	
	If[keyLength > 3,
		If[ChoiceDialog[
"The period of this text is calculated to be greater than 3. This will result in brute force taking a significant amount of time to run. Are you sure you want to continue?",
			{"Yes" -> 1, "Cancel" -> 2}] == 2, Return["Canceled"]]];
	
	possibleKeys = BruteForceHelper[cipherText, "", keyLength];
	
	KeyMap[ToUpperCase, Sort[possibleKeys, Greater]][[;;UpTo[10]]]
]

BruteForceHelper[cipherText_, subKey_, depth_] := Module[{plainText, key, possibleKeys = <||>},
	If[depth == 1, 
		Do[
			key = subKey <> a;
			plainText = VigenereCipher[cipherText, key, False];
			AppendTo[possibleKeys, key -> Fitness[plainText]];
		, {a, Alphabet[]}],
		Do[
			AppendTo[possibleKeys, BruteForceHelper[cipherText, subKey <> a, depth - 1]]
		, {a, Alphabet[]}]
	];
	
	possibleKeys
]


TimeConstrained[BruteForce[cipherText], timeLimit]


(* ::Section:: *)
(*Crib Method*)


(* ::Text:: *)
(*Run the first chunk below, give a small piece of your original text (about 10-20 letters long) in the second chunk, and run it.*)


Crib[cipherText_, cribText_] := Module[{
cribTextCleaned = StringDelete[cribText, Except[LetterCharacter]],
extendedCipherText = StringRepeat[cipherText, 2],
period = Period[cipherText],
possibleKeys = <||>, cribLen, diff, regex, piece, decryptedPiece, keyPartitions, fitness},
	cribLen = StringLength[cribTextCleaned];
	If[cribLen <= period, Return["Crib is too short"]];
	
	If[2 * period - cribLen > 0,
		regex = RegularExpression[".*" <> StringRepeat["(.)", period] <> "\\1"],
		regex = RegularExpression[".*(.{" <> ToString[period] <> "})\\1.*"]
	];
	
	Do[
		piece = StringTake[extendedCipherText, {i, i + cribLen}];
		decryptedPiece = StringDelete[VigenereCipher[piece, cribTextCleaned, False],
							Except[LetterCharacter]];
		If[StringMatchQ[decryptedPiece, regex],
			keyPartitions = StringPartition[decryptedPiece, period, 1][[1;;UpTo[period]]];
			Do[
				If[!KeyExistsQ[possibleKeys, keyPart],
					fitness = Fitness[VigenereCipher[cipherText, keyPart, False]];
					AppendTo[possibleKeys, keyPart -> fitness];
				]
			, {keyPart, keyPartitions}]
		]
	, {i, StringLength[cipherText] - cribLen}];
	
	Sort[possibleKeys, Greater][[;;UpTo[10]]]
]


crib = "engaged in a civil war";
Crib[cipherText, crib]


(* ::Section:: *)
(*Variational Method*)


(* ::Text:: *)
(*Run the chunk below and then the second chunk below.*)


Variational[cipherText_] := Module[{period = Period[cipherText], fitness = -99, key, keyVariate, x, fitnessVariate},
	key = Table["a", period];
	
	While[fitness < -10,
		keyVariate = key;
		x = RandomInteger[{1, period}];
		Do[
			keyVariate[[x]] = letter;
			fitnessVariate = Fitness[VigenereCipher[cipherText, StringJoin[keyVariate], FALSE]];
			If[fitnessVariate > fitness,
				key = keyVariate;
				fitness = fitnessVariate
			]
		, {letter, Alphabet[]}]
	];
	
	ToUpperCase[StringJoin[key]]
]


Variational[cipherText]


(* ::Section:: *)
(*End*)


(* ::Text:: *)
(*And there you go! Feel free to use the chunk below to play around with stuff if you'd like.*)


coolCode
