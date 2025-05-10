(* ::Package:: *)

Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/VigenereCipherShen.m"]
Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/CrackTools.m"]


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


messageText = "Four score and seven years ago our fathers brought forth, on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.
Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.
But, in a larger sense, we can not dedicate\[LongDash]we can not consecrate\[LongDash]we can not hallow\[LongDash]this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us\[LongDash]that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion\[LongDash]that we here highly resolve that these dead shall not have died in vain\[LongDash]that this nation, under God, shall have a new birth of freedom\[LongDash]and that government of the people, by the people, for the people, shall not perish from the earth.";

cipherText = VigenereCipher[messageText, "F"];

BruteForce[cipherText]


Do[
	key = StringJoin[RandomChoice[Alphabet[], RandomInteger[{10,50}]]];
	len = StringLength[key];
	cipherText = VigenereCipher[messageText, key];
	period = Period[cipherText];
	Print["Key length is " <> ToString[len] <> ", calculated period is " <> ToString[period]]
, 10]


Do[
	cipherText = VigenereCipher[messageText, a];
	Print[Timing[BruteForce[cipherText]]]
, {a, Alphabet[]}]


Do[
	If[a == b, Continue[]];
	cipherText = VigenereCipher[messageText, a <> b];
	Print[Timing[BruteForce[cipherText]]]
, {a, Alphabet[]}, {b, Alphabet[]}]



option = ChoiceDialog["Are you sure?", {"Yes" -> 1, "Cancel" -> 2}]
