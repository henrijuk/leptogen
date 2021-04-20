(* ::Package:: *)

(* ::Subsubsection:: *)
(*Begin package*)


(* Tools for making self-energy interpolation functions for LeptoGen. *)
(* Author: Henri Jukkala *)
(* For copyright and license information see included LICENSE.txt *)

BeginPackage["SelfEnergyInterpolation`"];

Unprotect["SelfEnergyInterpolation`*"];
ClearAll["SelfEnergyInterpolation`*"];

(* External dependencies. *)
`Private`$pkgDir = DirectoryName@$InputFileName;
Get["SelfEnergyFunctions`", Path -> ParentDirectory[`Private`$pkgDir, 1]];
Get["Util`", Path -> ParentDirectory[`Private`$pkgDir, 1]];

(* Exports. *)
CreateInterpolationFunctions;


(* ::Subsubsection:: *)
(*Begin private*)


Begin["`Private`"]; (* Begin private context. *)


(* ::Subsubsection:: *)
(*Grid and interpolation functions*)


(* Wrapper for making the interpolation functions.
   Returns an Association which contains the InterpolatingFunctions.
   The function arguments are (lz, ly, x), and self-energies are evaluated at y0 = Sqrt[y^2 + (z*x)^2]. *)
CreateInterpolationFunctions[filename_String, lz_, ly_, OptionsPattern[]] := Module[
	{lzCoords, lyCoords, dOper, \[CapitalSigma]eqNum, \[Delta]\[CapitalSigma]Num},
	
	lzCoords = Subdivide[N@lz[[1]], N@lz[[2]], lz[[3]] - 1];
	lyCoords = Subdivide[N@ly[[1]], N@ly[[2]], ly[[3]] - 1];
	
	{\[CapitalSigma]eqNum, \[Delta]\[CapitalSigma]Num} = CacheGridData[filename, lzCoords, lyCoords, OptionValue@"Recalculate",
		OptionValue@"SelfEnergyCacheUseZip"];
	dOper = CreateDerivativeOperators[lzCoords, lyCoords, OptionValue@"FiniteDiffSettings"];
	
	(* Output. *)
	CreateInterpolationFunctionsCore[lzCoords, lyCoords, \[CapitalSigma]eqNum, \[Delta]\[CapitalSigma]Num, dOper, OptionValue@"InterpSettings"]
];

(* Messages. *)
CreateInterpolationFunctions::wrongCache = "Wrong coordinate vectors for self-energy: data archive does not match."

(* Default options. *)
Options[CreateInterpolationFunctions] = {
	"Recalculate" -> True,
	"InterpSettings" -> {InterpolationOrder -> 3, Method -> "Spline"},
	"FiniteDiffSettings" -> {"DifferenceOrder" -> 4},
	"SelfEnergyCacheUseZip" -> True
};


(* Cached calculation of the grid data to/from specified file. *)
CacheGridData[filename_String, lzCoords_, lyCoords_, recalc:(True|False), usezip:(True|False)] := Module[
	{calc, out},
	
	calc := Util`CacheDataCalculation[
		filename,
		{"lzGridCoords.txt", "lyGridCoords.txt", "SigmaGridData.txt"},
		"RecalculateCache" -> recalc,
		"HashCode" -> Hash[{lzCoords, lyCoords}],
		"UseZip" -> usezip,
		{lzCoords, lyCoords, CalculateGridData[lzCoords, lyCoords]}
	];
	
	out = Check[calc,
		Message[CreateInterpolationFunctions::wrongCache]; Abort[],
		Util`CacheDataCalculation::wrongCache
	];
	
	out[[3]]
];


(* Calculates values of a^A, b^A etc. in (z,y)-grid evaluated at y0 = Sqrt[y^2 + z^2].
   Returns a list {\[CapitalSigma]eq, \[Delta]\[CapitalSigma]} of Associations containing the numerical data. *)
CalculateGridData[lzCoords_, lyCoords_] := Module[
	{zyGrid, zy, \[CapitalSigma]eqNum = <||>, \[Delta]\[CapitalSigma]Num = <||>},
	
	zyGrid = Developer`ToPackedArray@N@Tuples[10^{lzCoords, lyCoords}];
	
	ParallelEvaluate[Off[General::munfl]]; (* Set machine underflow warnings off. *)
	
	(* Calculate aAeq[\[Omega], y] and bAeq[\[Omega], y]. *)
	Util`Tic["aA and bA (\[CapitalSigma]eq) numerical integration"];
	{\[CapitalSigma]eqNum["aAeq"], \[CapitalSigma]eqNum["bAeq"]} = Transpose@ParallelTable[
		SelfEnergyFunctions`abAeqThermal[zy[[1]], zy[[2]]],
		{zy, zyGrid},
		DistributedContexts -> "SelfEnergyFunctions`"
	];
	Util`Toc[];
	
	(* Calculate a<\[Delta][\[Omega], y] and b<\[Delta][\[Omega], y]. *)
	Util`Tic["a< and b< (\[Delta]\[CapitalSigma]) numerical integration"];
	{\[Delta]\[CapitalSigma]Num["aS\[Delta]"], \[Delta]\[CapitalSigma]Num["bS\[Delta]"]} = Transpose@ParallelTable[
		SelfEnergyFunctions`abS\[Delta]MassShell[zy[[1]], zy[[2]]],
		{zy, zyGrid},
		DistributedContexts -> "SelfEnergyFunctions`"
	];
	Util`Toc[];
	
	(* Calculate aA\[Delta][\[Omega], y] and bA\[Delta][\[Omega], y]. *)
	Util`Tic["aA and bA (\[Delta]\[CapitalSigma]) numerical integration"];
	{\[Delta]\[CapitalSigma]Num["aA\[Delta]"], \[Delta]\[CapitalSigma]Num["bA\[Delta]"]} = Transpose@ParallelTable[
		SelfEnergyFunctions`abA\[Delta]MassShell[zy[[1]], zy[[2]]],
		{zy, zyGrid},
		DistributedContexts -> "SelfEnergyFunctions`"
	];
	Util`Toc[];
	
	ParallelEvaluate[On[General::munfl]];
	
	{\[CapitalSigma]eqNum, \[Delta]\[CapitalSigma]Num}
];


(* Returns an Association of operators which calculate derivatives from the numerical grid data. *)
CreateDerivativeOperators[lzCoords_, lyCoords_, optns_] := Module[
	{zyGrid, z, y, lzD, y0D},
	
	(* These calculate lz-derivatives on the lz,ly grid. *)
	lzD[1] = NDSolve`FiniteDifferenceDerivative[Derivative[1, 0], {lzCoords, lyCoords}, optns];
	lzD[2] = NDSolve`FiniteDifferenceDerivative[Derivative[2, 0], {lzCoords, lyCoords}, optns];
	
	zyGrid = Tuples[10^{lzCoords, lyCoords}];
	z = zyGrid[[All, 1]];
	y = zyGrid[[All, 2]];
	
	(* These calculate y0-derivatives using the lz-derivatives. *)
	y0D[1] = Sqrt[z^2 + y^2]/z^2*lzD[1]@#/Log[10] &;
	y0D[2] = (z^2 + y^2)/z^4*lzD[2]@#/Log[10]^2 - (z^2 + 2*y^2)/z^4*lzD[1]@#/Log[10] &;
	
	AssociationThread[
		{"id", "lzD1", "lzD2", "y0D1", "y0D2"},
		{Identity, lzD[1], lzD[2], y0D[1], y0D[2]}
	]
];


(* Interpolations for a^A, b^A etc. and their derivatives as functions of (lz,ly).
   Returns an Association containing the InterpolatingFunctions. *)
CreateInterpolationFunctionsCore[lzCoords_, lyCoords_, \[CapitalSigma]eqNum_, \[Delta]\[CapitalSigma]Num_, dOperators_, optns_] := Module[
	{lzlyGrid, interp, \[CapitalSigma]eqInterp, \[Delta]\[CapitalSigma]Interp},
	
	(* Set up the grid points and interpolation routine. *)
	lzlyGrid = Tuples[{lzCoords, lyCoords}];
	interp[data_] := Interpolation[Transpose@{lzlyGrid, data}, optns];
	
	(* Create the interpolation functions. *)
	\[CapitalSigma]eqInterp = Map[Function[{f}, interp@f[#]], dOperators] & /@ \[CapitalSigma]eqNum;
	\[Delta]\[CapitalSigma]Interp = interp /@ \[Delta]\[CapitalSigma]Num;
	
	(* Wrap in the dimensionless mass argument x (third argument). *)
	\[CapitalSigma]eqInterp = Map[Function[{f}, f[#1 + Log10@#3, #2] &], \[CapitalSigma]eqInterp, {2}];
	\[Delta]\[CapitalSigma]Interp = Map[Function[{f}, f[#1 + Log10@#3, #2] &], \[Delta]\[CapitalSigma]Interp, {1}];
	
	(* Output. *)
	Join[\[CapitalSigma]eqInterp, \[Delta]\[CapitalSigma]Interp]
];


(* ::Subsubsection:: *)
(*End*)


(* Protect all exported symbols. *)
Protect["SelfEnergyInterpolation`*"];

End[]; (* End private context. *)

Block[{$ContextPath}, EndPackage[]];
