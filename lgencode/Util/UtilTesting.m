(* ::Package:: *)

(* ::Subsubsection:: *)
(*Begin*)


(* Unit tests for Util.wl *)

SetDirectory@NotebookDirectory[];
Get["Util.wl"];
$ContextPath = DeleteDuplicates@Prepend[$ContextPath, "Util`"];

Begin["Util`Testing`"];

(* Initial test list. *)
tests = {};


(* ::Subsubsection:: *)
(*Timing functions*)


tests = tests // Append@VerificationTest[
	Tic["hello"]; Toc[],
	Null,
	{Tic::message, Toc::message},
	TestID -> "tictoc"
];

tests = tests // Append@VerificationTest[
	Tic[]; Tic[]; Toc[]; Toc[],
	Null,
	{Tic::message, Tic::message, Toc::message, Toc::message},
	TestID -> "nestedTictoc"
];

tests = tests // Append@VerificationTest[
	Toc[],
	Null,
	{Toc::noTic},
	TestID -> "noTic"
];


(* ::Subsubsection:: *)
(*Cached data calculation (zip archive)*)


tests = tests // Append@VerificationTest[
	Module[{f, fc, a, b, c, d, e, x, cached, file1, file2, subfiles, del},
		fc = 0;
		f[] := (fc++; <|"k1" -> 3275, "k2" -> 7546|>);
		cached[recalc_] := CacheDataCalculation[
			"test.zip",
			{"num.txt", "sinx", "str.dat", "array.tmp", "5"},
			"HashCode" -> 158158,
			"RecalculateCache" -> recalc,
			{123, Sin[x], "test string", Array[x, {2, 2, 2}], f[]}
		];
		{a, b, c, d, e} = cached[True];
		Clear[a, b, c, d, e];
		{a, b, c, d, e} = cached[False];
		file1 = FileExistsQ["test.zip"];
		file2 = FileExistsQ["test.zip.old"];
		subfiles = Import["test.zip", "FileNames"];
		del = DeleteFile["test.zip"];
		{a, b, c, d, e, fc, file1, file2, subfiles, del} === {
			123,
			Sin[x],
			"test string",
			Array[x, {2, 2, 2}],
			<|"k1" -> 3275, "k2" -> 7546|>,
			1,
			True,
			False,
			{"num.txt", "sinx", "str.dat", "array.tmp", "5", "_hashcode.txt"},
			Null
		}
	],
	TestID -> "cacheData"
];

tests = tests // Append@VerificationTest[
	Module[{cached},
		cached[recalc_, hash_] := CacheDataCalculation[
			"test.zip",
			{"test.txt"},
			"HashCode" -> hash,
			"RecalculateCache" -> recalc,
			{1}
		];
		cached[True, 123];
		cached[False, 321];
		DeleteFile["test.zip"]
	],
	Null,
	{CacheDataCalculation::wrongCache},
	TestID -> "cacheDataWrongHash"
];


(* ::Subsubsection:: *)
(*Combined cache + timing (zip archive)*)


tests = tests // Append@VerificationTest[
	Module[{f, out, cached, file1, file2, del},
		f[] := Module[{o},
			Tic["test"];
			out = 12357;
			Toc[];
			out
		];
		cached[recalc_] := CacheDataCalculation[
			"test.zip",
			{"num.dat"},
			"RecalculateCache" -> recalc,
			{f[]}
		];
		{out} = cached[True];
		Clear[out];
		{out} = cached[False];
		file1 = FileExistsQ["test.zip"];
		file2 = FileExistsQ["test.zip.old"];
		del = DeleteFile["test.zip"];
		{out, file1, file2, del} === {
			12357,
			True,
			False,
			Null
		}
	],
	True,
	{Tic::message, Toc::message},
	TestID -> "cacheDataAndTiming"
];


(* ::Subsubsection:: *)
(*Cached data calculation (directory)*)


tests = tests // Append@VerificationTest[
	Module[{f, fc, a, b, c, d, e, x, cached, dir, del1, del2},
		fc = 0;
		f[] := (fc++; <|"k1" -> 3275, "k2" -> 7546|>);
		cached[recalc_] := CacheDataCalculation[
			"testdir",
			{"num.txt", "sinx", "str.dat", "array.tmp", "5"},
			"HashCode" -> 158158,
			"RecalculateCache" -> recalc,
			"UseZip" -> False,
			{123, Sin[x], "test string", Array[x, {2, 2, 2}], f[]}
		];
		{a, b, c, d, e} = cached[True];
		Clear[a, b, c, d, e];
		{a, b, c, d, e} = cached[False];
		dir = FileExistsQ["testdir"];
		Pause[1];
		del1 = DeleteFile["testdir/"<># & /@ {"num.txt", "sinx", "str.dat", "array.tmp", "5", "_hashcode.txt"}];
		Pause[1];
		del2 = DeleteDirectory["testdir"];
		{a, b, c, d, e, fc, dir, del1, del2} === {
			123,
			Sin[x],
			"test string",
			Array[x, {2, 2, 2}],
			<|"k1" -> 3275, "k2" -> 7546|>,
			1,
			True,
			Null,
			Null
		}
	],
	TestID -> "cacheDataDir"
];

tests = tests // Append@VerificationTest[
	Module[{cached},
		cached[recalc_, hash_] := CacheDataCalculation[
			"testdir",
			{"test.txt"},
			"HashCode" -> hash,
			"RecalculateCache" -> recalc,
			"UseZip" -> False,
			{1}
		];
		cached[True, 123];
		cached[False, 321];
		Pause[1];
		DeleteFile[{"testdir/test.txt", "testdir/_hashcode.txt"}];
		Pause[1];
		DeleteDirectory["testdir"]
	],
	Null,
	{CacheDataCalculation::wrongCache},
	TestID -> "cacheDataDirWrongHash"
];


(* ::Subsubsection:: *)
(*End*)


TestReport@tests

End[];
$ContextPath = DeleteCases[$ContextPath, "Util`"];
