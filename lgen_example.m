(* ::Package:: *)

(* ::Section:: *)
(*Leptogenesis example*)


(* ::Subsection:: *)
(*Calculation*)


SetDirectory@NotebookDirectory[];
Get["lgencode/LeptoGen/LeptoGen.wl"];
Get["lgencode/LeptoGen/LeptoGenTools.wl"];


init = LeptoGen`InitialSetup[
	"SelfEnergyCacheName" -> "selfenergycache",
	"SelfEnergyInterpSettings" -> {InterpolationOrder -> 3, Method -> "Spline"},
	"SelfEnergyRecalculate" -> False,
	"ParallelKernels" -> $ProcessorCount,
	"SelfEnergylzCoordSpecs" -> {-4., 4., 100},
	"SelfEnergylyCoordSpecs" -> {-4., 7., 120}
];


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
	"BoltzmannAttractorApprox" -> False,
	"OnlyPrecalc" -> False
];
Util`Toc[];

LeptoGenTools`PrintResultInfo[data["Info"], "digits" -> 4]


(* ::Subsection:: *)
(*Plots*)


LeptoGenTools`Plot\[Delta]f[data, 4, 2.]
LeptoGenTools`PlotAsymm[data]
LeptoGenTools`LogPlotAsymm[data]
LeptoGenTools`PlotSCP[data]
LeptoGenTools`LogPlotSCP[data]
LeptoGenTools`PlotWB[data]
LeptoGenTools`LogPlotWB[data]
LeptoGenTools`PlotWA[data]
LeptoGenTools`LogPlotWA[data]


LeptoGenTools`CollHeatmap[data, 1, 8]
LeptoGenTools`\[Delta]fHeatmap[data, 8]
LeptoGenTools`fAdHeatmap[data, 1]
LeptoGenTools`SourceHeatmap[data, 1]
