(* ::Package:: *)

Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/VigenereCipherShen.m"]
Import["https://raw.githubusercontent.com/Sloth186/CryptographyProject/refs/heads/main/CrackTools.m"]


messageText = "Four score and seven years ago our fathers brought forth, on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.
Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.
But, in a larger sense, we can not dedicate\[LongDash]we can not consecrate\[LongDash]we can not hallow\[LongDash]this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us\[LongDash]that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion\[LongDash]that we here highly resolve that these dead shall not have died in vain\[LongDash]that this nation, under God, shall have a new birth of freedom\[LongDash]and that government of the people, by the people, for the people, shall not perish from the earth.";


cipherText = VigenereCipher[messageText, "I"]


Period[cipherText]


BruteForce[ciphertext_] := Module[{keylength, plaintext, fitness, possiblekeys = <||>},
	keylength = Period[ciphertext];
	
	Switch[
		keylength, 1,
			Do[
				plaintext = VigenereCipher[ciphertext, a, False];
				fitness = Fitness[plaintext];
				If[fitness > -10, AppendTo[possiblekeys, a -> fitness]]
			, {a, Alphabet[]}],
		2,
			Print[2],
		3,
			Print[3]
	];
	
	Return[Keys[Sort[possiblekeys]]]
]


BruteForce[cipherText]
