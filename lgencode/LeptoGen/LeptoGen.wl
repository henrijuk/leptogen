(* ::Package:: *)

(* ::Subsection:: *)
(*LeptoGen*)


(* Wolfram Language package *)

(* Title: LeptoGen *)
(* Summary: Functions for solving neutrino \[Delta]f and the lepton asymmetry. *)
(* Package Version: 1.1 *)
(* Mathematica Version: 12.0 *)
(* Author: Henri Jukkala *)
(* For copyright and license information see included LICENSE.txt *)

(* Last changed 22. Jun 2021 *)

(* Note: this program assumes the default option (Heads -> False) for Map, Apply, Cases and Replace. *)


(* ::Subsubsection:: *)
(*Begin package*)


BeginPackage["LeptoGen`"];

Unprotect["LeptoGen`*"];
ClearAll["LeptoGen`*"];

(* Get the package directory. *)
`Private`$pkgDir = DirectoryName@$InputFileName;

(* External packages. *)
Get["SelfEnergyInterpolation`", Path -> ParentDirectory[`Private`$pkgDir, 1]];
Get["Util`", Path -> ParentDirectory[`Private`$pkgDir, 1]];


(* ::Subsubsection:: *)
(*Exported symbols*)


InitialSetup::usage = "Launches parallel kernels and creates self-energy interpolation functions.";
GridSetup::usage = "Creates the numerical z,y-grid and related functions.";
PhysicsParams::usage = "Calculates all relevant physics parameters.";
ControlSettings::usage = "Wraps program control flags and settings.";

PreCalculation::usage = "Calculates numerical functions needed for the main QKE solver.";
NeutrinoSolve::usage = "Solves the neutrino part of the main QKE.";
LeptonSolve::usage = "Solves the lepton asymmetry part of the main QKE.";

BEPreCalculation::usage = "Calculates numerical functions needed for the momentum-dependent BE solver.";
BENeutrinoSolve::usage = "Solves the neutrino part of the BE.";
BELeptonSolve::usage = "Solves the lepton asymmetry part of the BE.";

BESolveIntegrated::usage = "Solves integrated semiclassical Boltzmann equations for the neutrinos and lepton asymmetry.";

AllInput::usage = "Wraps together all used input parameters.";
ResultInfo::usage = "Wraps together some relevant data from the results.";


(* ::Subsubsection:: *)
(*Begin private*)


Begin["`Private`"]; (* Begin private context. *)


(* ::Subsubsection:: *)
(*Auxiliary functions (not exported)*)


(* Makes coordinate vectors according to specifications. To get nonuniform spacing,
   specify a distribution whose CDF and inverse CDF are used for scaling. *)
coordVectorScaled[min_, max_, n_] := Subdivide[min, max, n - 1];
coordVectorScaled[min_, max_, n_, dist_] := (InverseCDF@dist)@coordVectorScaled[
	(CDF@dist)@min, (CDF@dist)@max, n
];

(* For example, to use the y = (u/(1 - u))^p substitution for y = k/T, define
   mydist /: InverseCDF[mydist[p_]] := Log10 @* ((#/(1 - #))^p &);
   mydist /: CDF[mydist[p_]] := (#^(1/p)/(1 + #^(1/p)) &) @* (10^# &);
   and use e.g. coordVectorScaled[min, max, n, mydist[2]].
   (In this example we assumed that the original variable is logarithmic, log10(y).)
*)


(* Merge default options with given option List. *)
mergeOptions[f_, opt_List] := Merge[{Flatten@opt, Options@f}, First]


(* ::Subsubsection:: *)
(*Initial setup*)


(* Does initial (once-only) setup and returns an Association of self-energy interpolation functions. *)
InitialSetup[opt:OptionsPattern[]] := Module[
	{\[CapitalSigma]Int},
	
	(* Launch parallel computation kernels, if going to recalculate. *)
	If[OptionValue@SelfEnergyRecalculate,
		LaunchKernels[OptionValue@ParallelKernels - $KernelCount]
	];
	
	(* Create the self-energy interpolation functions for the specified grid and settings. *)
	\[CapitalSigma]Int = SelfEnergyInterpolation`CreateInterpolationFunctions[
		OptionValue@SelfEnergyCacheName,
		OptionValue@SelfEnergylzCoordSpecs,
		OptionValue@SelfEnergylyCoordSpecs,
		"Recalculate" -> OptionValue@SelfEnergyRecalculate,
		"InterpSettings" -> OptionValue@SelfEnergyInterpSettings,
		"FiniteDiffSettings" -> OptionValue@SelfEnergyFiniteDiffSettings,
		"SelfEnergyCacheUseZip" -> OptionValue@SelfEnergyCacheUseZip
	];
	
	(* Output. *)
	<|
		"SigmaThermalInterp" -> \[CapitalSigma]Int, (* T-dependent parts of \[CapitalSigma]eq and \[Delta]\[CapitalSigma] only *)
		"Input" -> mergeOptions[InitialSetup, {opt}]
	|> 
];

(* Default input. *)
Options[InitialSetup] = {
	"ParallelKernels" -> $ProcessorCount,
	"SelfEnergyInterpSettings" -> {InterpolationOrder -> 3, Method -> "Spline"},
	"SelfEnergyFiniteDiffSettings" -> {"DifferenceOrder" -> 8},
	"SelfEnergyCacheUseZip" -> False, (* whether to use a zip archive or not *)
	"SelfEnergyCacheName" -> "selfenergycache",
	"SelfEnergyRecalculate" -> True,
	"SelfEnergylzCoordSpecs" -> {-4., 4., 100},
	"SelfEnergylyCoordSpecs" -> {-4., 7., 120}
};


(* ::Subsubsection:: *)
(*Grid setup*)


(* Sets up the (lz,ly)-grid and returns an Association of grid variables and functions. *)
GridSetup[opt:OptionsPattern[]] := Module[
	{pack = Developer`ToPackedArray, lzCoords, lyCoords, lzlyGrid, lzlyTuples, lzD, interp},
	
	(* Create the Log_10(z) and Log_10(y) coordinate vectors and grid points. *)
	lzCoords = coordVectorScaled @@ OptionValue@lzCoordSpecs // pack@*N;
	lyCoords = coordVectorScaled @@ OptionValue@lyCoordSpecs // pack@*N;
	
	(* Create the (lz,ly) grid points. *)
	lzlyGrid = Outer[List, lzCoords, lyCoords] // pack; (* grid configuration *)
	lzlyTuples = Tuples[{lzCoords, lyCoords}] // pack; (* equivalent to Flatten[lzlyGrid, 1] *)
	
	(* Finite difference operator for calculating the lz-derivative on the (lz,ly) grid.
	   Note: can be applied to both grid and tuples (flat configuration) of values. *)
	With[{opts = OptionValue@FiniteDiffSettings},
		lzD = NDSolve`FiniteDifferenceDerivative[Derivative[1, 0], {lzCoords, lyCoords}, opts];
	];
	
	(* Interpolation operators for creating InterpolatingFunctions with the given settings. *)
	(* #1: list of argument values (can be lists). #2: list of function values (can be arrays). *)
	With[{opts1 = OptionValue@InterpSettings1, opts2 = OptionValue@InterpSettings2},
		interp[1] = Interpolation[Transpose@{#1, #2}, opts1] &;
		interp[2] = Interpolation[Transpose@{#1, #2}, opts2] &;
	];
	
	(* Output. *)
	<|
		"lzCoords" -> lzCoords,
		"lyCoords" -> lyCoords,
		"lzlyTuples" -> lzlyTuples,
		"lzlyGrid" -> lzlyGrid,
		"lzGrid" -> lzlyGrid[[All, All, 1]],
		"lyGrid" -> lzlyGrid[[All, All, 2]],
		"lzDerivativeOperator" -> lzD, (* used in precalc *)
		"Interpolator1" -> interp[1], (* interpolator for step 1 (neutrino DE) *)
		"Interpolator2" -> interp[2], (* interpolator for step 2 (lepton DE) *)
		"Input" -> mergeOptions[GridSetup, {opt}]
	|>
];

(* Default input. *)
Options[GridSetup] = {
	"lzCoordSpecs" -> {-2., 2., 200},
	"lyCoordSpecs" -> {-2., 4., 100},
	"FiniteDiffSettings" -> {"DifferenceOrder" -> 8}, (* precalc *)
	"InterpSettings1" -> {InterpolationOrder -> 3, Method -> "Spline"}, (* neutrino DE*)
	"InterpSettings2" -> {InterpolationOrder -> 1} (* lepton DE*)
};


(* ::Subsubsection:: *)
(*Physics parameters*)


(* Physics parameters, returned in an Association. *)
PhysicsParams[opt:OptionsPattern[]] := Module[
	{Y, \[Theta], m, massRatio, g\[FivePointedStar], mPlanck, Hubble1, entropyCoeff, \[CapitalGamma]vac, K},
	
	(* Shorthands for input arguments. *)
	{Y[1], Y[2], \[Theta][1,2], m[1], massRatio, g\[FivePointedStar]} = N@OptionValue@{
		"Y1", "Y2", "\[Theta]12", "m1", "m2/m1", "g*"
	};
	
	m[2] = m[1]*massRatio;
	mPlanck = 1.2209*^19; (* Planck mass \[Sqrt](\[HBar]c/G), in GeVs *)
	Hubble1 = Sqrt[4*\[Pi]^3/45*g\[FivePointedStar]]*m[1]^2/mPlanck; (* H(T) at T = m1 *)
	entropyCoeff = 2*\[Pi]^2/45*g\[FivePointedStar]; (* coeff of s = scoeff*T^3 *)
	
	\[CapitalGamma]vac = AssociationMap[m[#]*Y[#]^2/(8\[Pi]) &, {1, 2}]; (* vacuum decay widths *)
	K = AssociationMap[\[CapitalGamma]vac[#]/Hubble1 &, {1, 2}]; (* washout strength parameters *)
	
	(* Output. *)
	<|
		"Y1" -> Y[1],
		"Y2" -> Y[2],
		"\[Theta]12" -> \[Theta][1,2],
		"m1" -> m[1],
		"x1" -> 1., (* dimensionless mass parameter *)
		"x2" -> massRatio, (* dimensionless mass parameter *)
		"\[CapitalDelta]x2" -> massRatio^2 - 1, (* relative mass-squared difference *)
		"H1" -> Hubble1,
		"scoeff" -> entropyCoeff,
		"\[CapitalGamma]vac1" -> \[CapitalGamma]vac[1],
		"\[CapitalGamma]vac2" -> \[CapitalGamma]vac[2],
		"K1" -> K[1],
		"K2" -> K[2],
		"Input" -> mergeOptions[PhysicsParams, {opt}]
	|>
];

(* Default input. *)
Options[PhysicsParams] = {
	"Y1" -> 0.06, (* absolute value of Yukawa coupling 1 *)
	"Y2" -> 0.1, (* absolute value of Yukawa coupling 2 *)
	"\[Theta]12" -> \[Pi]/4., (* angle in radians between the two complex Yukawas *)
	"m1" -> 1.*^13, (* physical mass, in GeVs *)
	"m2/m1" -> Sqrt[1. + (0.06^2 + 0.1^2)/(16\[Pi])], (* mass ratio *)
	"g*" -> 110. (* effective relativistic degrees of freedom  *)
};


(* ::Subsubsection:: *)
(*Program control settings*)


(* Program control flags and other settings, returned in an Association. *)
ControlSettings[opt:OptionsPattern[]] := mergeOptions[ControlSettings, {opt}];

(* Default input. *)
Options[ControlSettings] = {
	"TreeLevelAdSolution" -> True, (* use free S_ad, setting to False is experimental *)
	"ThermalInitialCondition" -> False, (* switch between vacuum/thermal *)
	"DiscardNeutrinoSourceTerm" -> False, (* removes the -d/dz(S^<_ad) source term *)
	"DiscardWashoutA" -> False, (* sets W_A to zero *)
	"HelicityEvenOnly" -> False, (* the helicity-symmetric approximation *)
	"DecouplingApproximation" -> False, (* the decoupling limit *)
	"AttractorApproximation" -> False, (* even more simplified approximation for S_CP *)
	"UseWightmanGreater" -> False, (* use S^> instead of S^< for the Majorana neutrinos *)
	"NeutrinoDESettings" -> {AccuracyGoal -> 12, PrecisionGoal -> 12, InterpolationOrder -> All},
	"LeptonDEUseNeutrinoGrid" -> False, (* True: uses the \[Delta]f function grid for S_CP etc. *)
	"LeptonDESettings" -> {AccuracyGoal -> 20, PrecisionGoal -> 12, InterpolationOrder -> All,
		StartingStepSize -> 1.*^-4, MaxStepSize -> 0.01},
	"BoltzmannDESettings" -> {AccuracyGoal -> 20, PrecisionGoal -> 12, InterpolationOrder -> All,
		StartingStepSize -> 1.*^-4, MaxStepSize -> 0.01},
	"BoltzmannDiscardBackreaction" -> True (* removes lepton BR-term in neutrino rate eq. *),
	"BoltzmannCPAsymmetry" -> "Mixed" (* specifies the regulator for Boltzmann CP-asymmetry *),
	"BoltzmannAttractorApprox" -> False (* True: uses the late time limit for \[Delta]f_i *)
};


(* ::Subsubsection:: *)
(*Main quantum kinetic equation solver*)


Get["MainQKE.m", Path -> $pkgDir];

PreCalculation = `MainQKE`Precalc;
NeutrinoSolve = `MainQKE`NeutrinoEqSolve;
LeptonSolve = `MainQKE`LeptonEqSolve;


(* ::Subsubsection:: *)
(*Semiclassical Boltzmann equation solvers*)


Get["BoltzmannCP.m", Path -> $pkgDir];
Get["BoltzmannEqs.m", Path -> $pkgDir];
Get["BoltzmannRateEqs.m", Path -> $pkgDir];

(* Momentum-dependent equations *)
BEPreCalculation = `BoltzmannEqs`Precalc;
BENeutrinoSolve = `BoltzmannEqs`NeutrinoEqSolve;
BELeptonSolve = `BoltzmannEqs`LeptonEqSolve;

(* Rate equations *)
BESolveIntegrated = `BoltzmannRateEqs`BESolve;


(* ::Subsubsection:: *)
(*All input arguments*)


(* Combines all used input arguments of the setup functions. *)
AllInput[init_, grid_, phys_, sett_] := <|
	"Init" -> init["Input"],
	"Grid" -> grid["Input"],
	"Physics" -> phys["Input"],
	"Settings" -> sett
|>;


(* ::Subsubsection:: *)
(*Result info*)


(* Collects some result and parameter values to an Association. *)
ResultInfo[grid_, phys_, settings_, leptonsol_, BEleptonsol_, Intsol_] := Module[
	{lzMin, lzMax},
	
	{lzMin, lzMax} = MinMax@grid@"lzCoords";
	
	<|
		"Y_L" -> leptonsol["Y_L"][lzMax],
		"Y_L BE" -> BEleptonsol["Y_L"][lzMax],
		"Y_L Int" -> Intsol["Y_L"][lzMax],
		"H(m1)" -> phys@"H1",
		"T0" -> phys@"m1"/10^lzMin,
		"\[CapitalGamma]vac1" -> phys@"\[CapitalGamma]vac1",
		"\[CapitalGamma]vac2" -> phys@"\[CapitalGamma]vac2",
		"K1" -> phys@"K1",
		"K2" -> phys@"K2",
		"\[CapitalDelta]x2" -> phys@"\[CapitalDelta]x2"
	|>
];


(* ::Subsubsection:: *)
(*End package*)


End[]; (* End private context. *)

Protect["LeptoGen`*"];

Block[{$ContextPath}, EndPackage[]];
