(* ::Package:: *)

(* ::Subsubsection:: *)
(*Begin package*)


(* Generic utility functions. *)
(* Author: Henri Jukkala *)
(* For copyright and license information see included LICENSE.txt *)

(* Last changed 20. Apr 2021 *)

BeginPackage["Util`"];

Unprotect["Util`*"];
ClearAll["Util`*"];

(* Exports. *)
Tic;
Toc;
CacheDataCalculation;
SetRecalculateAll;


(* ::Subsubsection:: *)
(*Begin private*)


Begin["`Private`"]; (* Begin private context. *)

(* Private temporary variables used in the implementation. *)
$tictocStack;
$dataHash;

(* Package wide global setting. *)
$recalcAll;


(* ::Subsubsection:: *)
(*Timing functions*)


(* Timing functions \[AGrave] la Matlab. *)
Tic[msg_:""] := Module[{},
	$tictocStack = Append[$tictocStack, <|"start" -> AbsoluteTime[], "msg" -> msg|>];
	Off[General::stop];
	Message[Tic::message, msg, DateString@TimeObject[Now]];
	On[General::stop];
];

Toc[] := Module[{t1, t2, msg, elapsedTime},
	If[$tictocStack === {}, Return[Message[Toc::noTic], Module]];
	elapsedTime = AbsoluteTime[] - Key["start"]@Last@$tictocStack;
	msg = Key["msg"]@Last@$tictocStack;
	t1 = ToString@N@Quantity[elapsedTime, "Seconds"];
	t2 = DateString@*TimeObject@@QuantityMagnitude@UnitConvert[
		Quantity[elapsedTime, "Seconds"],
		MixedUnit@{"Hours", "Minutes", "Seconds"}
	];
	Off[General::stop];
	Message[Toc::message, msg, t1, t2];
	On[General::stop];
	$tictocStack = Most@$tictocStack;
];

$tictocStack = {}; (* initial value *)

Tic::message = "Evaluation (`1`) started at `2`.";
Toc::message = "Evaluation (`1`) lasted `2` (\[TildeEqual] `3`).";
Toc::noTic = "There was no corresponding call to Tic[].";


(* ::Subsubsection:: *)
(*Cached data calculation and loading*)


(* Saves data (any Mathematica expressions) as Strings into separate (text) files in an archive file.
   Optionally saves a hash code in the archive. *)
SaveDataArchive[expr_List, filename_List, archName_String, OptionsPattern[]] := Module[
	{finaldata, finalnames, elems},
	
	(* Add the hash code as the last element. *)
	finaldata := Append[InputForm /@ expr, OptionValue@"HashCode"];
	finalnames = Append[filename, "_hashcode.txt"];
	
	If[OptionValue@"UseZip",
		(* We save the data (Mathematica expressions) literally, as Strings. *)
		elems = Append["String"] /@ List /@ finalnames;
		(* Rename existing file and export into a zip archive. *)
		RenameFile[archName, (archName <> ".old"), OverwriteTarget -> True];
		Export[archName, finaldata, {"ZIP", elems}], (* alternatively e.g. {"GZIP", "TAR", elems} *)
		(* Else: save into a directory of the same name. *)
		Quiet[CreateDirectory[archName], CreateDirectory::filex];
		MapThread[Export[archName<>"/"<>#2, #1, "String"] &, {finaldata, finalnames}]
	];
	
	(* We return the expressions unmodified. *)
	expr
];

Options[SaveDataArchive] = {
	"HashCode" -> Undefined,
	"UseZip" -> True
};


(* Loads data (Mathematica expressions saved as strings) from separate files in an archive file.
   Optionally checks the hash code in the archive against the given hash code. *)
LoadDataArchive[archName_String, filename_List, OptionsPattern[]] := Module[
	{hash, elems},
	
	(* Check the hash and issue a message if it doesn't match. *)
	hash = If[OptionValue@"UseZip",
		ToExpression@Import[archName, {"_hashcode.txt", "String"}],
		ToExpression@Import[archName<>"/_hashcode.txt", "String"]
	];
	If[hash =!= OptionValue@"HashCode", Message[CacheDataCalculation::wrongCache]];
	
	(* Load the archive files (as Strings) in original order, sans the hash code. *)
	If[OptionValue@"UseZip",
		ToExpression@Import[archName, {#, "String"}] & /@ Most@Import[archName, "FileNames"],
		ToExpression@Import[archName<>"/"<>#, "String"] & /@ filename
	]
];

Options[LoadDataArchive] = {
	"HashCode" -> Undefined,
	"UseZip" -> True
};


(* Calculate data or load it from an archive file.
   Optionally tests a given hash code against the one saved with the data. *)
CacheDataCalculation[archName_String, filename_List, OptionsPattern[], expr_List] := Module[
	{},
	If[$recalcAll == False && OptionValue["RecalculateCache"] == False,
		(* Load old data. *)
		LoadDataArchive[archName, filename, "HashCode" -> OptionValue@"HashCode",
			"UseZip" -> OptionValue@"UseZip"],
		(* Calculate new data. *)
		SaveDataArchive[expr, filename, archName, "HashCode" -> OptionValue@"HashCode",
			"UseZip" -> OptionValue@"UseZip"]
	]
];

CacheDataCalculation::wrongCache = "Input hash code did not match the data file hash code.";
SetAttributes[CacheDataCalculation, HoldRest];
Options[CacheDataCalculation] = {
	"RecalculateCache" -> False,
	"HashCode" -> Undefined,
	"UseZip" -> True
};


(* Sets the global override for CacheDataCalculation. *)
SetRecalculateAll[x_] := If[x == True, $recalcAll = True, $recalcAll = False];

$recalcAll = False; (* Private variable for track-keeping; initial value. *)


(* ::Subsubsection:: *)
(*End*)


(* Protect all exported symbols. *)
Protect["Util`*"];

End[]; (* End private context. *)

Block[{$ContextPath}, EndPackage[]];
