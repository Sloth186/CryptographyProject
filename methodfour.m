(* ::Package:: *)

Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/VigenereCipherShen.m"]
Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/CrackTools.m"]


cosAngle[x_] := Module[{numerator = 0, lengthx = 0, lengthy = 0},
	Do[
		numerator += mono[[i]] * x[[i]];
		lengthx += mono[[i]] * mono[[i]];
		lengthy += x[[i]] * x[[i]]
	, {i, Length[mono]}];
	
	numerator / Sqrt[lengthx * lengthy]
]


messageText = "Four score and seven years ago our fathers brought forth, on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.
Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.
But, in a larger sense, we can not dedicate\[LongDash]we can not consecrate\[LongDash]we can not hallow\[LongDash]this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us\[LongDash]that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion\[LongDash]that we here highly resolve that these dead shall not have died in vain\[LongDash]that this nation, under God, shall have a new birth of freedom\[LongDash]and that government of the people, by the people, for the people, shall not perish from the earth.";

cipherText = VigenereCipher[messageText, "OLAF"];

frequencies = {};
period = Period[cipherText];
slices = Slice[Characters[cipherText], period];
Do[AppendTo[frequencies,
		KeySelect[Counts[slice], LetterQ]]
, {slice, slices}]
frequencies /= Length[slices];
frequencies = KeySort /@ frequencies;
key = Table["A", period];
Do[
	If[cosAngle[frequencies[[i]]] > 0.9, key[[i]] = letter];
	frequencies[[i]] = RotateRight[frequencies[[i]], 1];
, {i, period}, {letter, Alphabet[]}]
key


Do[Print[FromLetterNumber[j]];Print[cosAngle[RotateRight[frequencies[[i]],j]]//N],{i,4},{j,26}]



