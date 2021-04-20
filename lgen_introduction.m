(* ::Package:: *)

(* ::Section:: *)
(*Leptogenesis package introduction*)


(* ::Text:: *)
(*Start by running this file (by using "Evaluation -> Evaluate notebook" or "Run All Code"  in the Mathematica front end).*)


(* ::Subsection:: *)
(*Packages*)


(* ::Text:: *)
(*First we load the packages.*)


SetDirectory@NotebookDirectory[];
Get["lgencode/LeptoGen/LeptoGen.wl"];
Get["lgencode/LeptoGen/LeptoGenTools.wl"];


(* ::Text:: *)
(*LeptoGen.wl is the main package which contains routines for setting up the self-energy interpolation functions (InitialSetup), for setting up the logarithmic (z, k/T)-grid (GridSetup), for setting the physics parameters (PhysicsParams) and program control settings (ControlSettings), for pre-calculating relevant quantities (PreCalculation) before solving the main equations, and the main leptogenesis equation solvers (NeutrinoSolve, LeptonSolve) themselves.*)


(* ::Text:: *)
(*LeptoGenTools.wl contains a handy wrapper (SolveAll) for setting up and solving all the leptogenesis equations with arguments for most-used parameters. NOTE: SolveAll takes as optional arguments all options of ControlSettings of the main package. This package also includes some (plotting) functions for displaying the leptogenesis results.*)


(* ::Text:: *)
(*All options (given as key-value pairs) are listed in the end of this file (after successfully evaluating the notebook/m-file).*)


(* ::Subsection:: *)
(*Initialization*)


(* ::Text:: *)
(*The main package uses self-energy interpolation functions. The data for these is either loaded from a pre-calculated cache (with "SelfEnergyRecalculate" -> False), or calculated from scratch and saved to a cache (with "SelfEnergyRecalculate" -> True). In both cases the name of the cache directory is specified with the option "SelfEnergyCacheName". The option "SelfEnergyInterpSettings" is used to specify options for the interpolations (which use the built-in Interpolation function). The last three options shown here are used to specify the amount of Mathematica's parallel kernels  (used only when recalculating the cache) and the lz = log10(z) and ly = log10(k/T) grids.*)


init = LeptoGen`InitialSetup[
	"SelfEnergyCacheName" -> "selfenergycache",
	"SelfEnergyInterpSettings" -> {InterpolationOrder -> 3, Method -> "Spline"},
	"SelfEnergyRecalculate" -> False,
	"ParallelKernels" -> $ProcessorCount,
	"SelfEnergylzCoordSpecs" -> {-4., 4., 100},
	"SelfEnergylyCoordSpecs" -> {-4., 7., 120}
];


(* ::Subsection:: *)
(*Data calculation*)


(* ::Text:: *)
(*Here we do a resonant leptogenesis calculation with the parameters y1 = 0.06, y2 = 0.1, \[Theta]12 = \[Pi]/4, m1 = 10^13 GeV and (m2^2 - m1^2)/m1^2 = (0.06^2 + 0.1^2)/(16\[Pi]). The lz = log10(z) and ly = log10(k/T) coordinate vector specifications are given as lists {min, max, amount of points}. Options are given as key-value pairs. Here we show the most relevant options (with their default values) for controlling the initial condition and the approximations for solving the equations.*)


Util`Tic["example calculation"];
data = LeptoGenTools`SolveAll[init,
	0.06, 0.1, \[Pi]/4., 1.*^13, (0.06^2 + 0.1^2)/(16\[Pi]), {-2., 2., 200}, {-2., 4., 100},
	"ThermalInitialCondition" -> False,
	"DiscardNeutrinoSourceTerm" -> False,
	"DiscardWashoutA" -> False,
	"HelicityEvenOnly" -> False,
	"DecouplingApproximation" -> False,
	"AttractorApproximation" -> False,
	"LeptonDEUseNeutrinoGrid" -> False,
	"BoltzmannDiscardBackreaction" -> True,
	"BoltzmannCPAsymmetry" -> "Mixed",
	"OnlyPrecalc" -> False
];
Util`Toc[];


(* ::Text:: *)
(*The results are saved into a nested Association; use Keys to see what is available.*)


data // Keys // InputForm


(* ::Text:: *)
(*Some examples:*)


data["FullNeutrino"] // Keys // InputForm


data["FullNeutrino", "\[Delta]f"]


(* Show the (types of) data saved in the key "FullLepton".*)
Head /@ data["FullLepton"] // Dataset


(* ::Section:: *)
(*Reference*)


(* ::Subsection:: *)
(*Data structure*)


(* ::Text:: *)
(*This displays the full structure of the output of SolveAll (only the Heads of individual data elements are shown here):*)


Dataset /@ Map[Head, data, {2}] // Normal // Column


(* ::Subsection:: *)
(*Options and their default values*)


(* ::Text:: *)
(*Main package functions.*)


Options[LeptoGen`InitialSetup] // Dataset


Options[LeptoGen`GridSetup] // Dataset


Options[LeptoGen`PhysicsParams] // Dataset


Options[LeptoGen`ControlSettings] // Dataset


(* ::Text:: *)
(*Tool package functions.*)


Options[LeptoGenTools`SolveAll] // Dataset


Options[LeptoGenTools`PrintResultInfo] // Dataset
