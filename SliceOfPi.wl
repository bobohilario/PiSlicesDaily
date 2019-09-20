(* ::Package:: *)

(* ::Subsection:: *)
(*Configuration*)


$adminEmail="putyouremailhere@example.com";


(* ::Subsection:: *)
(*Context*)


BeginPackage["SliceOfPi`"];
SliceOfPi`InitializeSliceOfPi;
SliceOfPi`SliceOfPi;
Begin["`Private`"];


(* ::Subsection:: *)
(*Utilities*)


createDirectory[dir_?DirectoryQ]:=Null
createDirectory[str_String]:=CreateDirectory[str,CreateIntermediateDirectories->True]


(* ::Subsection:: *)
(*Initialization*)


InitializeSliceOfPi[]:=(
createDirectory/@{
	$base,
	$logs,
	$last
};
Put[-1,$lastChunk];

)


(* ::Subsection:: *)
(*Error handling*)


failedalert[str_,data_]:=With[{file=errorFile[DateList[]]},
createDirectory[FileNameDrop[file]];
Put[{DateObject[],str,data},file];
SendMail[<|"To"->$adminEmail,"Subject"->"Pi Slice Error: "<>str,
	"Body"->StringTake[ToString[data],UpTo[10000]]|>];
Throw[Failure[str,<||>],"slicefail"]
]


(* ::Subsection:: *)
(*File system*)


$base="SliceOfPi";
$logs:=FileNameJoin[{$base,"Logs"}];
$last:=FileNameJoin[{$base,"Last"}];


$lastTweet:=FileNameJoin[{$last,"TweetResult"}]
$lastChunk:=FileNameJoin[{$last,"Chunk"}]
$lastDigits:=FileNameJoin[{$last,"Digits"}]


$errors:=FileNameJoin[{$logs,"Errors"}]
errorFile[{y_,m_,d_,h_,__}]:=FileNameJoin[Flatten[ToString/@{$errors,y,h,d,h,$SessionID}]]


(* ::Subsection:: *)
(*Service Connection*)


$twitterid="DailySliceOfPi"; (* prd *)
$twitterid="SlicePiTest"; (* test *)


$twitter:=With[{tw=ServiceConnect["Twitter"]},
If[Head[tw]=!=ServiceObject,
	failedalert["Slice of Pi ServiceConnect failed",tw],
	If[
		ServiceExecute["Twitter", "UserData"][[1, "ScreenName"]]=!=$twitterid,
		failedalert["Slice of Pi Wrong twitter id",tw],
		tw
	]
]
]


sendTweet[digits_String]:=With[{
	twitterres=EvaluationData[ServiceExecute[ServiceConnect["Twitter"], "RawUpdate", 
		{"status" -> digits}
	]]},
	Put[twitterres,$lastTweet];
	twitterres["Result"]
]



(* ::Subsection:: *)
(*Digits*)


$tweetlength=280;


lastChunk[]:=With[{ch=Quiet@Get[$lastChunk]},
	If[IntegerQ[ch],ch,failedalert["No Previous Chunk",ch]]
]


lastDigits[]:=With[{ch=Quiet@Get[$lastDigits]},
	If[StringQ[ch],ch,failedalert["No Previous Digits",ch]]
]


lastTweet[]:=With[{ch=Quiet@Get[$lastTweet]},
	If[AssociationQ[ch],ch,failedalert["No Previous Tweet Data",ch]]
]


piChunk[0]:="3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196442881097566593344612847564823378678316527120190914564856692346034861045432664"


piChunk[n_]:=StringJoin[ToString/@First@RealDigits[Pi,10,$tweetlength,-$tweetlength*n+1]]/;n>0


(* ::Subsection:: *)
(*Verify Last*)


verifyLast[]:=With[{n=lastChunk[]},
	If[n>-1,
		With[{dig=lastDigits[]},
			If[piChunk[n]===dig,
				verifyLastTweet[dig];
				n+1
				,
				failurealert["wrong last digits",{n,dig}]
			]
		]
		,
		0
	]
]


verifyLastTweet[dig_]:=With[{lasttw=lastTweet[]},
Print[lasttw]

]


(* ::Subsection:: *)
(*validResultQ*)


validResultQ[expr_]:=!FailureQ[expr]


(* ::Subsection:: *)
(*Main Event*)


SliceOfPi[]:=Catch[With[{n=verifyLast[]},
	sliceOfPi[n]
],"slicefail"]


sliceOfPi[n_]:=With[{dig=piChunk[n]},
	With[{res=sendTweet[dig]},
		If[validResultQ[res],
			Put[n,$lastChunk];
			Put[dig,$lastDigits];
			SendMail[<|"To"->$adminEmail,"Subject"->"Pi Slice Success: "<>ToString[n],
				"Body"->"https://twitter.com/PiSlicesDaily"|>];
			
			<|"Chunk"->n,"Digits"->dig,"Result"->res|>
			,
			failurealert["invalid result",res]
		]
	]
]


(* ::Subsection:: *)
(*Context*)


End[]
EndPackage[]
