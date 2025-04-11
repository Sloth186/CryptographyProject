(* ::Package:: *)

(* ::Input:: *)
(*vigenereEncrypt[plaintext_,key_]:=Module[{plaintextNumList,keyNumList,ciphertextNumList,textLength},*)
(*plaintextNumList=LetterNumber[plaintext]-1;*)
(*textLength=Length[plaintextNumList];*)
(*If[StringQ[key],*)
(*keyNumList=LetterNumber[key]-1; keyNumList=PadRight[keyNumList,textLength,keyNumList],*)
(*keyNumList=PadRight[key,textLength,key]];*)
(*ciphertextNumList=Table[Mod[plaintextNumList[[i]]+keyNumList[[i]],26],{i,1,textLength}];*)
(*StringJoin[FromLetterNumber[ciphertextNumList+1]]*)
(*]*)


(* ::Input:: *)
(*vigenereDecryptWithKey[ciphertext_,key_]:=Module[{ciphertextNumList,keyNumList,plaintextNumList,textLength},*)
(*ciphertextNumList=LetterNumber[ciphertext]-1;*)
(*textLength=Length[ciphertextNumList];*)
(*If[StringQ[key],*)
(*keyNumList=LetterNumber[key]-1; keyNumList=PadRight[keyNumList,textLength,keyNumList],*)
(*keyNumList=PadRight[key,textLength,key]];*)
(*plaintextNumList=Table[Mod[ciphertextNumList[[i]]-keyNumList[[i]],26],{i,1,textLength}];*)
(*StringJoin[FromLetterNumber[plaintextNumList+1]]*)
(*]*)


(* ::Input:: *)
(*message="hello";*)
(*key="bb";*)
(*ctext=vigenereEncrypt[message,key]*)
(*ptext=vigenereDecryptWithKey[ctext,key]*)


(* ::Input:: *)
(*vigenereDecryptBruteForce[ciphertext_,maxKeySize_]:=Module[{ciphertextNumList,textLength,keyTestList,plaintextTestNumList,numKeys,plaintextTestStringList},*)
(*ciphertextNumList=LetterNumber[ciphertext]-1;*)
(*textLength=Length[ciphertextNumList];*)
(*Do[*)
(*keyTestList=Permutations[PadRight[Range[26]-1,m*26,Range[26]-1],{m}];*)
(*numKeys=Length[keyTestList];*)
(*plaintextTestNumList=Table[Table[Mod[ciphertextNumList[[j]]-keyTestList[[i]][[Mod[j-1,m]+1]],26],{j,1,textLength}],{i,1,numKeys}];*)
(*plaintextTestStringList=Table[StringJoin[FromLetterNumber[plaintextTestNumList[[k]]+1]],{k,1,numKeys}];*)
(*Do[If[LanguageIdentify[plaintextTestStringList[[q]]]==Entity["Language","English"]&&DictionaryWordQ[StringJoin[FromLetterNumber[keyTestList[[q]]+1]]]==True*)
(*,Print["Possible plaintext is '",plaintextTestStringList[[q]],"' from key of ",StringJoin[FromLetterNumber[keyTestList[[q]]+1]],"."]]*)
(*,{q,1,numKeys}]*)
(*,{m,1,maxKeySize}]*)
(*]*)


(* ::Input:: *)
(*vigenereEncrypt["artificialamateursarenotatallamazing","up"]*)


(* ::Input:: *)
(*LanguageIdentify["artificialamateursarenotatallamazing"]*)


(* ::Input:: *)
(*vigenereDecryptBruteForce["ugnxzxwxuaubuiyjlhugyciiuiuafpgptxhv",3]*)


(* ::Input:: *)
(**)
