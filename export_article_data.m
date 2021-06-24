(* ::Package:: *)

(* ::Section:: *)
(*Data export script*)


(*
Calculates and exports ALL numerical data used in the article
    "Flavour mixing transport theory and resonant leptogenesis",
    H. Jukkala, K. Kainulainen, P. M. Rahkila (2021).
Can be run in the Mathematica front end (use "Run All Code" or "Evaluation -> Evaluate Notebook").
Warning: can take a long time to run (e.g. ~1 hour on a quad-core workstation with 16 GB memory).
*)


(* ::Subsection:: *)
(*Begin context*)


Begin["lgenExport`"]; (* namespace for this script. *)


(* Directory for the output data files. *)
exportDirName = "export";


(* ::Subsection:: *)
(*Initialization*)


SetDirectory@NotebookDirectory[];
Get["lgencode/LeptoGen/LeptoGen.wl"];
Get["lgencode/LeptoGen/LeptoGenTools.wl"];


(* Uses the precalculated self-energy data cache. (Re-calculation can take several minutes.) *)
Util`Tic["initial setup"];
init = LeptoGen`InitialSetup[
	"SelfEnergyCacheName" -> "selfenergycache",
	"SelfEnergyInterpSettings" -> {InterpolationOrder -> 3, Method -> "Spline"},
	"SelfEnergyRecalculate" -> False, (* True: recalculates the cache *)
	"ParallelKernels" -> $ProcessorCount,
	"SelfEnergylzCoordSpecs" -> {-4., 4., 100},
	"SelfEnergylyCoordSpecs" -> {-4., 7., 120}
];
Util`Toc[];


(* ::Subsection:: *)
(*Data calculation*)


(* ::Subsubsection:: *)
(*Fixed mass difference*)


Util`Tic["fixed mass diff. calculations"];


(* Benchmark parameters (Y1 = 0.06). Vacuum initial conditions. *)
data["benchmark, vacuum"] = LeptoGenTools`SolveAll[init,
	0.06, 0.1, \[Pi]/4., 1.*^13, (0.06^2 + 0.1^2)/(16\[Pi]), {-2., 2., 200}, {-2., 4., 100},
	"LeptonDEUseNeutrinoGrid" -> True
];

(* Benchmark parameters (Y1 = 0.06). Thermal initial conditions. *)
data["benchmark, thermal"] = LeptoGenTools`SolveAll[init,
	0.06, 0.1, \[Pi]/4., 1.*^13, (0.06^2 + 0.1^2)/(16\[Pi]), {-2., 2., 200}, {-2., 4., 100},
	"ThermalInitialCondition" -> True,
	"LeptonDEUseNeutrinoGrid" -> True
];

(* Y1 = 0.01. Vacuum initial conditions. *)
data["Y1=0.01, vacuum"] = LeptoGenTools`SolveAll[init,
	0.01, 0.1, \[Pi]/4., 1.*^13, (0.06^2 + 0.1^2)/(16\[Pi]), {-2., 2., 200}, {-2., 4., 100},
	"LeptonDEUseNeutrinoGrid" -> True
];

(* Y1 = 0.01. Thermal initial conditions. *)
data["Y1=0.01, thermal"] = LeptoGenTools`SolveAll[init,
	0.01, 0.1, \[Pi]/4., 1.*^13, (0.06^2 + 0.1^2)/(16\[Pi]), {-2., 2., 200}, {-2., 4., 100},
	"ThermalInitialCondition" -> True,
	"LeptonDEUseNeutrinoGrid" -> True
];


(* Benchmark parameters (Y1 = 0.06). Vacuum initial conditions. Very large grid. *)
data["benchmark, vacuum, large"] = LeptoGenTools`SolveAll[init,
	0.06, 0.1, \[Pi]/4., 1.*^13, (0.06^2 + 0.1^2)/(16\[Pi]), {-2., 2., 300}, {-2., 4., 450},
	"LeptonDEUseNeutrinoGrid" -> True
];


(* Mass diff. parameter \[CapitalDelta]x2 = 0.02 and Y1 = 0.06. Vacuum initial conditions. Large grid. *)
data["\[CapitalDelta]x2 = 0.02, vacuum, large"] = LeptoGenTools`SolveAll[init,
	0.06, 0.1, \[Pi]/4., 1.*^13, 0.02, {-2., 2., 200}, {-2., 4., 200},
	"BoltzmannCPAsymmetry" -> "Sum",
	"LeptonDEUseNeutrinoGrid" -> True
];


(* Benchmark parameters (Y1 = 0.06). Vacuum initial conditions. Sum regulator for Boltzmann. *)
data["benchmark, vacuum, BE-Sum"] = LeptoGenTools`SolveAll[init,
	0.06, 0.1, \[Pi]/4., 1.*^13, (0.06^2 + 0.1^2)/(16\[Pi]), {-2., 2., 200}, {-2., 4., 100},
	"BoltzmannCPAsymmetry" -> "Sum",
	"LeptonDEUseNeutrinoGrid" -> True
];

(* Benchmark parameters (Y1 = 0.06). Vacuum initial conditions. Helicity-even-only approximation. *)
data["benchmark, vacuum, hEvenOnly"] = LeptoGenTools`SolveAll[init,
	0.06, 0.1, \[Pi]/4., 1.*^13, (0.06^2 + 0.1^2)/(16\[Pi]), {-2., 2., 200}, {-2., 4., 100},
	"HelicityEvenOnly" -> True,
	"LeptonDEUseNeutrinoGrid" -> True
];

(* Benchmark parameters (Y1 = 0.06). Vacuum initial conditions. Decoupling approximation. *)
data["benchmark, vacuum, decoupling"] = LeptoGenTools`SolveAll[init,
	0.06, 0.1, \[Pi]/4., 1.*^13, (0.06^2 + 0.1^2)/(16\[Pi]), {-2., 2., 200}, {-2., 4., 100},
	"DecouplingApproximation" -> True,
	"LeptonDEUseNeutrinoGrid" -> True
];


(* Y1 = 0.01, Y2 = 0.02. Vacuum initial conditions. Sum regulator for Boltzmann. *)
data["Y1=0.01, Y2=0.02, vacuum, BE-Sum"] = LeptoGenTools`SolveAll[init,
	0.01, 0.02, \[Pi]/4., 1.*^13, (0.01^2 + 0.02^2)/(16\[Pi]), {-2., 2., 200}, {-2., 4., 100},
	"BoltzmannCPAsymmetry" -> "Sum",
	"LeptonDEUseNeutrinoGrid" -> True
];

(* Y1 = 0.01, Y2 = 0.02. Vacuum initial conditions. Effective sum regulator for Boltzmann. *)
data["Y1=0.01, Y2=0.02, vacuum, BE-EffSum"] = LeptoGenTools`SolveAll[init,
	0.01, 0.02, \[Pi]/4., 1.*^13, (0.01^2 + 0.02^2)/(16\[Pi]), {-2., 2., 200}, {-2., 4., 100},
	"BoltzmannCPAsymmetry" -> "EffectiveSum",
	"LeptonDEUseNeutrinoGrid" -> True
];


Util`Toc[];


(* ::Subsubsection:: *)
(*Varying mass difference*)


(* ::Text:: *)
(*First set*)


Util`Tic["varying mass diff. calculations 1"];

(* Varying mass difference, with Y1 = 0.01. Vacuum initial conditions. *)
data["varying \[CapitalDelta]x2, Y1=0.01"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.01, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with mixed regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.01, BE-Mixed"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.01, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Mixed",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with difference regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.01, BE-Difference"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.01, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Difference",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with sum regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.01, BE-Sum"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.01, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Sum",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with the effective sum regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.01, BE-EffSum"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.01, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "EffectiveSum",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

Util`Toc[];


(* ::Text:: *)
(*Second set*)


Util`Tic["varying mass diff. calculations 2"];

(* Varying mass difference, with Y1 = 0.06. Vacuum initial conditions. *)
data["varying \[CapitalDelta]x2, Y1=0.06"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.06, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with mixed regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.06, BE-Mixed"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.06, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Mixed",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with difference regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.06, BE-Difference"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.06, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Difference",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with sum regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.06, BE-Sum"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.06, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Sum",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with the effective sum regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.06, BE-EffSum"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.06, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "EffectiveSum",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

Util`Toc[];


(* ::Text:: *)
(*Third set*)


Util`Tic["varying mass diff. calculations 3"];

(* Varying mass difference, with Y1 = 0.09. Vacuum initial conditions. *)
data["varying \[CapitalDelta]x2, Y1=0.09"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.09, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with mixed regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.09, BE-Mixed"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.09, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Mixed",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with difference regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.09, BE-Difference"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.09, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Difference",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with sum regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.09, BE-Sum"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.09, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Sum",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with the effective sum regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.09, BE-EffSum"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.09, 0.1, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "EffectiveSum",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

Util`Toc[];


(* ::Text:: *)
(*Fourth set*)


Util`Tic["varying mass diff. calculations 4"];

(* Varying mass difference, with Y1 = 0.01, Y2 = 0.02. Vacuum initial conditions. *)
(* Note that we use a lower \[CapitalDelta]m^2 range here. *)
data["varying \[CapitalDelta]x2, Y1=0.01, Y2=0.02"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.01, 0.02, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-3], 30 - 1]; (* lower range *)
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with mixed regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.01, Y2=0.02, BE-Mixed"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.01, 0.02, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Mixed",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with difference regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.01, Y2=0.02, BE-Difference"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.01, 0.02, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Difference",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with sum regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.01, Y2=0.02, BE-Sum"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.01, 0.02, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "Sum",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

(* Boltzmann only with the effective sum regulator. *)
data["varying \[CapitalDelta]x2, Y1=0.01, Y2=0.02, BE-EffSum"] = Module[{f, \[CapitalDelta]x2vec},
	f = LeptoGenTools`SolveAll[init,
		0.01, 0.02, \[Pi]/4., 1.*^13, #, {-2., 2., 200}, {-2., 4., 100},
		"BoltzmannCPAsymmetry" -> "EffectiveSum",
		"OnlyBE" -> True,
		"LeptonDEUseNeutrinoGrid" -> True
	]["Info"] &;
	\[CapitalDelta]x2vec = 10^Subdivide[Log10[1.*^-7], Log10[1.*^-2], 30 - 1];
	f /@ \[CapitalDelta]x2vec
];

Util`Toc[];


(* ::Subsection:: *)
(*Data exporting*)


Quiet[
	CreateDirectory[exportDirName<>"/p1data"];
	CreateDirectory[exportDirName<>"/p2data"];
	CreateDirectory[exportDirName<>"/p3data"];
	CreateDirectory[exportDirName<>"/p4data"];
	CreateDirectory[exportDirName<>"/p5data"];
	CreateDirectory[exportDirName<>"/p6data"],
	CreateDirectory::filex
];

export = LeptoGenTools`ExportTable;


(* ::Subsubsection:: *)
(*YL(z) data*)


exportYLz[data_, str_String] := Module[
	{path = exportDirName<>"/p1data/", YL, YLBE, YLRate, lz1, lz2, lz3},
	
	YL = data["FullLepton"]["Y_L"]; lz1 = First@YL["Coordinates"];
	YLBE = data["BELepton"]["Y_L"]; lz2 = First@YLBE["Coordinates"];
	YLRate = data["IntSol"]["Y_L"]; lz3 = First@YLRate["Coordinates"];
	
	export[path<>"YL_vs_z"<>str<>".txt", Transpose@{10^lz1, YL[lz1]}];
	export[path<>"YL-Boltzmann_vs_z"<>str<>".txt", Transpose@{10^lz2, YLBE[lz2]}];
	export[path<>"YL-Rate_vs_z"<>str<>".txt", Transpose@{10^lz3, YLRate[lz3]}];
];

exportYLz[data["benchmark, vacuum"], "_benchmark"];
exportYLz[data["benchmark, thermal"], "_benchmark_thermal"];
exportYLz[data["Y1=0.01, vacuum"], "_with_Y1_001"];
exportYLz[data["Y1=0.01, thermal"], "_with_Y1_001_thermal"];
exportYLz[data["Y1=0.01, Y2=0.02, vacuum, BE-Sum"], "_with_Y1_001_Y2_002_Sum"];
exportYLz[data["Y1=0.01, Y2=0.02, vacuum, BE-EffSum"], "_with_Y1_001_Y2_002_EffSum"];


(* ::Subsubsection:: *)
(*SCP(z) and W(z) data*)


exportSCPWz[data_, str_String] := Module[
	{path = exportDirName<>"/p2data/", SCP, SCPBE, SCPRate, WA, WABE, WB, WBBE, WBRate, lz},
	
	(* Note: we remove the log(10)*z scalings from SCP and W. *)
	
	SCP = data["FullLepton"]["SCP"]; lz[1] = First@SCP["Coordinates"];
	SCPBE = data["BELepton"]["SCP"]; lz[2] = First@SCPBE["Coordinates"];
	SCPRate = data["IntSol"]["SCP"]; lz[3] = data["Grid"]["lzCoords"];
	
	export[path<>"SCP_vs_z"<>str<>".txt", Transpose@{10^lz[1], SCP[lz[1]]/(Log[10]*10^lz[1])}];
	export[path<>"SCP-Boltzmann_vs_z"<>str<>".txt", Transpose@{10^lz[2], SCPBE[lz[2]]/(Log[10]*10^lz[2])}];
	export[path<>"SCP-Rate_vs_z"<>str<>".txt", Transpose@{10^lz[3], (SCPRate /@ lz[3])/(Log[10]*10^lz[3])}];
	
	WB = data["FullLepton"]["WB"]; lz[1] = First@WB["Coordinates"];
	WBBE = data["BELepton"]["WB"]; lz[2] = First@WBBE["Coordinates"];
	WBRate = data["IntSol"]["WB"]; lz[3] = data["Grid"]["lzCoords"];
	
	export[path<>"WB_vs_z"<>str<>".txt", Transpose@{10^lz[1], WB[lz[1]]/(Log[10]*10^lz[1])}];
	export[path<>"WB-Boltzmann_vs_z"<>str<>".txt", Transpose@{10^lz[2], WBBE[lz[2]]/(Log[10]*10^lz[2])}];
	export[path<>"WB-Rate_vs_z"<>str<>".txt", Transpose@{10^lz[3], (WBRate /@ lz[3])/(Log[10]*10^lz[3])}];
	
	WA = data["FullLepton"]["WA"]; lz[1] = First@WA["Coordinates"];
	WABE = data["BELepton"]["WA"]; lz[2] = First@WABE["Coordinates"];
	
	export[path<>"WA_vs_z"<>str<>".txt", Transpose@{10^lz[1], WA[lz[1]]/(Log[10]*10^lz[1])}];
	export[path<>"WA-Boltzmann_vs_z"<>str<>".txt", Transpose@{10^lz[2], WABE[lz[2]]/(Log[10]*10^lz[2])}];
];

exportSCPWz[data["benchmark, vacuum"], "_benchmark"];


(* ::Subsubsection:: *)
(*Heatmap data (neutrino functions)*)


exportHeatmaps[data_, str_String] := Module[
	{path = exportDirName<>"/p3data/", lzc, lyc, log10z},
	
	lzc = data["Grid"]["lzCoords"];
	lyc = data["Grid"]["lyCoords"];
	log10z = Log[10]*10^data["Grid", "lzGrid"]; (* scaling log(10)*z to be removed *)
	
	(* Export z and y coordinate vectors. *)
	export[path<>"z_coordinates"<>str<>".txt", 10^lzc];
	export[path<>"y_coordinates"<>str<>".txt", 10^lyc];
	
	(* Export \[Delta]f data. *)
	export[path<>"neutrino_deltaf_1"<>str<>".txt", data["FullNeutrino"]["\[Delta]f"][lzc][[All, All, 1]]];
	export[path<>"neutrino_deltaf_5"<>str<>".txt", data["FullNeutrino"]["\[Delta]f"][lzc][[All, All, 5]]];
	export[path<>"neutrino_deltaf_4"<>str<>".txt", data["FullNeutrino"]["\[Delta]f"][lzc][[All, All, 4]]];
	export[path<>"neutrino_deltaf_8"<>str<>".txt", data["FullNeutrino"]["\[Delta]f"][lzc][[All, All, 8]]];
	
	(* Export fAd and DfAd data. *)
	export[path<>"neutrino_fAd_1"<>str<>".txt", data["FullPrecalc"]["fAd"][[All, All, 1]]];
	export[path<>"neutrino_source_1"<>str<>".txt", data["FullNeutrino"]["SourceTerm"][lzc][[All, All, 1]]/log10z];
	
	(* Export Coll data. *)
	export[path<>"neutrino_Coll_1_1"<>str<>".txt", data["FullPrecalc"]["Coll"][[All, All, 1, 1]]/log10z];
	export[path<>"neutrino_Coll_1_8"<>str<>".txt", data["FullPrecalc"]["Coll"][[All, All, 1, 8]]/log10z];
];

exportHeatmaps[data["benchmark, vacuum, large"], ""];


(* ::Subsubsection:: *)
(*Final YL data (YL as function of (m2^2 - m1^2)/m1^2)*)


exportFinalYLMassDiff[data_, str_String, onlyBE:(_?BooleanQ):False] := Module[
	{path = exportDirName<>"/p4data/", \[CapitalDelta]x2vec, YLvec, YLBEvec, YLRatevec},
	
	\[CapitalDelta]x2vec = #["\[CapitalDelta]x2"] & /@ data;
	If[Not@onlyBE, YLvec = #["Y_L"] & /@ data];
	YLBEvec = #["Y_L BE"] & /@ data;
	YLRatevec = #["Y_L Int"] & /@ data;
	
	If[Not@onlyBE,
		export[path<>"YL-final_vs_massdiff"<>str<>".txt", Transpose@{\[CapitalDelta]x2vec, YLvec}],
		(* Else: *)
		export[path<>"YL-Boltzmann-final_vs_massdiff"<>str<>".txt", Transpose@{\[CapitalDelta]x2vec, YLBEvec}];
		export[path<>"YL-Rate-final_vs_massdiff"<>str<>".txt", Transpose@{\[CapitalDelta]x2vec, YLRatevec}];
	];
];

exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.01"], "_with_Y1_001"];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.06"], "_with_Y1_006"];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.09"], "_with_Y1_009"];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.01, Y2=0.02"], "_with_Y1_001_Y2_002"];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.01, BE-Mixed"], "_with_Y1_001_Mix", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.06, BE-Mixed"], "_with_Y1_006_Mix", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.09, BE-Mixed"], "_with_Y1_009_Mix", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.01, Y2=0.02, BE-Mixed"], "_with_Y1_001_Y2_002_Mix", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.01, BE-Difference"], "_with_Y1_001_Diff", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.06, BE-Difference"], "_with_Y1_006_Diff", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.09, BE-Difference"], "_with_Y1_009_Diff", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.01, Y2=0.02, BE-Difference"], "_with_Y1_001_Y2_002_Diff", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.01, BE-Sum"], "_with_Y1_001_Sum", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.06, BE-Sum"], "_with_Y1_006_Sum", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.09, BE-Sum"], "_with_Y1_009_Sum", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.01, Y2=0.02, BE-Sum"], "_with_Y1_001_Y2_002_Sum", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.01, BE-EffSum"], "_with_Y1_001_EffSum", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.06, BE-EffSum"], "_with_Y1_006_EffSum", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.09, BE-EffSum"], "_with_Y1_009_EffSum", True];
exportFinalYLMassDiff[data["varying \[CapitalDelta]x2, Y1=0.01, Y2=0.02, BE-EffSum"], "_with_Y1_001_Y2_002_EffSum", True];


(* ::Subsubsection:: *)
(*Oscillation data*)


exportOscillation[data_, str_String] := Module[
	{path = exportDirName<>"/p5data/", lyidx, ly, lzc, lzcGrid, \[Delta]f, SCPk, SCP, SCPBE, SCPBEk},
	
	lyidx = LeptoGenTools`NearestLyIndex[data["Grid"]][Log10[3.]];
	ly = data["Grid"]["lyCoords"][[lyidx]];
	lzc = First@data["FullNeutrino"]["\[Delta]f"]["Coordinates"];
	lzcGrid = data["Grid"]["lzCoords"];
	
	\[Delta]f = data["FullNeutrino"]["\[Delta]f"][lzc][[All, lyidx, 4]];
	SCPk = data["FullLepton"]["SCPdata"][[lyidx, All]];
	SCP = data["FullLepton"]["SCP"][lzcGrid];
	SCPBE = data["BELepton"]["SCP"][lzcGrid];
	SCPBEk = data["BELepton"]["SCPData"][[lyidx, All]];
	
	(* We remove the log(10)*z scaling for SCP here. We also need to remove the log(10)*k scaling, and we also opt
	   to factor out the k^2/(2\[Pi]^2) scaling (from d^3k/(2\[Pi])^3) of the SCP integrand *)
	export[path<>"neutrino_deltaf_4_vs_z"<>str<>".txt", Transpose@{10^lzc, \[Delta]f}];
	export[path<>"SCP_k_mode_3_vs_z"<>str<>".txt", Transpose@{10^lzc, SCPk/(Log[10]*10^lzc*Log[10]*(10^ly)^3/(2*\[Pi]^2))}];
	export[path<>"SCP_vs_z"<>str<>".txt", Transpose@{10^lzcGrid, SCP/(Log[10]*10^lzcGrid)}];
	export[path<>"SCPBE_vs_z"<>str<>".txt", Transpose@{10^lzcGrid, SCPBE/(Log[10]*10^lzcGrid)}];
	export[path<>"SCPBE_k_mode_3_vs_z"<>str<>".txt", Transpose@{10^lzcGrid, SCPBEk/(Log[10]*10^lzcGrid*Log[10]*(10^ly)^3/(2*\[Pi]^2))}];
];

exportOscillation[data["\[CapitalDelta]x2 = 0.02, vacuum, large"], "_with_large_massdiff"];


(* ::Subsubsection:: *)
(*Approximation comparison data*)


exportApproxComp[data_, str_String, onlyBE:(_?BooleanQ):False] := Module[
	{path = exportDirName<>"/p6data/", YL, YLBE, YLRate, SCP, SCPBE, SCPRate, lz},
	
	YL = data["FullLepton"]["Y_L"]; lz[1] = First@YL["Coordinates"];
	YLBE = data["BELepton"]["Y_L"]; lz[2] = First@YLBE["Coordinates"];
	YLRate = data["IntSol"]["Y_L"]; lz[3] = First@YLRate["Coordinates"];
	
	If[Not@onlyBE,
		export[path<>"YL_vs_z"<>str<>".txt", Transpose@{10^lz[1], YL[lz[1]]}],
		(* Else: *)
		export[path<>"YL-Boltzmann_vs_z"<>str<>".txt", Transpose@{10^lz[2], YLBE[lz[2]]}];
		export[path<>"YL-Rate_vs_z"<>str<>".txt", Transpose@{10^lz[3], YLRate[lz[3]]}];
	];
	
	SCP = data["FullLepton"]["SCP"]; lz[1] = First@SCP["Coordinates"];
	SCPBE = data["BELepton"]["SCP"]; lz[2] = First@SCPBE["Coordinates"];
	SCPRate = data["IntSol"]["SCP"]; lz[3] = data["Grid"]["lzCoords"];
	
	(* We remove the log(10)*z scaling for SCP here. *)
	If[Not@onlyBE,
		export[path<>"SCP_vs_z"<>str<>".txt", Transpose@{10^lz[1], SCP[lz[1]]/(Log[10]*10^lz[1])}],
		(* Else: *)
		export[path<>"SCP-Boltzmann_vs_z"<>str<>".txt", Transpose@{10^lz[2], SCPBE[lz[2]]/(Log[10]*10^lz[2])}];
		export[path<>"SCP-Rate_vs_z"<>str<>".txt", Transpose@{10^lz[3], (SCPRate /@ lz[3])/(Log[10]*10^lz[3])}];
	];
	
];

exportApproxComp[data["benchmark, vacuum, BE-Sum"], "_benchmark"];
exportApproxComp[data["benchmark, vacuum, hEvenOnly"], "_benchmark_hEvenOnly"];
exportApproxComp[data["benchmark, vacuum, decoupling"], "_benchmark_decoupling"];
exportApproxComp[data["benchmark, vacuum, BE-Sum"], "_benchmark_Sum", True];


(* ::Subsection:: *)
(*End context*)


End[]; (* lgenExport` *)
