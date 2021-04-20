(* ::Package:: *)

(* ::Subsection:: *)
(*LeptoGen tools*)


(* Tool package for LeptoGen. *)
(* Author: Henri Jukkala *)
(* For copyright and license information see included LICENSE.txt *)

(* Last changed 18. Apr 2021 *)


(* ::Subsubsection:: *)
(*Begin package*)


BeginPackage["LeptoGenTools`"];

Unprotect["LeptoGenTools`*"];
ClearAll["LeptoGenTools`*"];

(* Exported symbols. *)
SolveAll;
PrintResultInfo;

Plot\[Delta]f;
PlotAsymm;
PlotSCP;
PlotWB;
PlotWA;
LogPlotAsymm;
LogPlotSCP;
LogPlotWB;
LogPlotWA;

\[Delta]fHeatmap;
fAdHeatmap;
SourceHeatmap;
CollHeatmap;

NearestLy;
NearestLyIndex;
DataIsPacked;
DataIsMachineNumber;
ExportTable;


(* ::Subsubsection:: *)
(*Begin private*)


Begin["`Private`"]; (* Begin private context. *)


(* ::Subsubsection:: *)
(*Solve all*)


(* Solves all LeptoGen equations, with explicit input arguments for the most relevant
   parameters. Requires initialization with init = LeptoGen`InitialSetup[...].
   Returns all results in an Association.*)
SolveAll[init_, Y1_, Y2_, \[Theta]12_, m1_, \[CapitalDelta]x2_, lzSpec_List, lySpec_List, opt:OptionsPattern[{SolveAll, LeptoGen`ControlSettings}]
] := Module[
	{grid, phys, controlFlags, sett, prec, nsol, lsol, BEprec, BEnsol, BElsol, Intsol},
	
	Off[General::munfl]; (* Set machine underflow warnings off. *)
	
	grid = LeptoGen`GridSetup[
		"lzCoordSpecs" -> lzSpec,
		"lyCoordSpecs" -> lySpec
	];
	
	phys = LeptoGen`PhysicsParams[
		"Y1" -> Y1,
		"Y2" -> Y2,
		"\[Theta]12" -> \[Theta]12,
		"m1" -> m1,
		"m2/m1" -> Sqrt[1. + \[CapitalDelta]x2]
	];
	
	(* Select options of LeptoGen`ControlSettings from input options. *)
	controlFlags = Sequence@@Select[{opt}, KeyExistsQ[Options[LeptoGen`ControlSettings], First@#] &];
	
	sett = LeptoGen`ControlSettings[
		Evaluate@controlFlags
	];
	
	(* Solve the main QKEs. *)
	Which[OptionValue@"OnlyBE",
		prec = nsol = lsol = Undefined,
		OptionValue@"OnlyPrecalc",
		prec = LeptoGen`PreCalculation[init, grid, phys, sett];
		nsol = lsol = Undefined,
		True,
		prec = LeptoGen`PreCalculation[init, grid, phys, sett];
		nsol = LeptoGen`NeutrinoSolve[grid, prec, sett];
		lsol = LeptoGen`LeptonSolve[grid, prec, sett, nsol["\[Delta]f"]];
	];
	
	(* Solve the Boltzmann equations (momentum-dependent and integrated). *)
	Which[OptionValue@"OnlyPrecalc",
		BEprec = LeptoGen`BEPreCalculation[init, grid, phys, sett];
		BEnsol = BElsol = Intsol = Undefined,
		True,
		BEprec = LeptoGen`BEPreCalculation[init, grid, phys, sett];
		BEnsol = LeptoGen`BENeutrinoSolve[grid, BEprec, sett];
		BElsol = LeptoGen`BELeptonSolve[grid, BEprec, sett, BEnsol["\[Delta]f"]];
		Intsol = LeptoGen`BESolveIntegrated[grid, phys, sett];
	];
	
	On[General::munfl];
	
	(* Output. *)
	<|
		"Grid" -> grid,
		"Params" -> phys,
		"Settings" -> sett,
		"FullPrecalc" -> prec,
		"FullNeutrino" -> nsol,
		"FullLepton" -> lsol,
		"BEPrecalc" -> BEprec,
		"BENeutrino" -> BEnsol,
		"BELepton" -> BElsol,
		"IntSol" -> Intsol,
		"Input" -> LeptoGen`AllInput[init, grid, phys, sett],
		"Info" -> LeptoGen`ResultInfo[grid, phys, sett, lsol, BElsol, Intsol]
	|>
];

Options[SolveAll] = {
	"OnlyBE" -> False (* solve only Boltzmann and rate equations *),
	"OnlyPrecalc" -> False (* calculate the precalc parts only *)
};


(* ::Subsubsection:: *)
(*Result info*)


(* Prints (to $Output) some relevant numerical parameters from info = LeptoGen`ResultInfo[...] *)
PrintResultInfo[info_, OptionsPattern[]] := Module[{sf},
	sf = ScientificForm[#, OptionValue@"digits"] &;
	
	Print["Final lepton asymmetry: \*SubscriptBox[Y, \[CapitalDelta]\[ScriptL]] = ",
		info["Y_L"] // sf];
	Print["Final lepton asymmetry (Boltzmann): \*SubscriptBox[Y, \[CapitalDelta]\[ScriptL]] = ",
		info["Y_L BE"] // sf];
	Print["Final lepton asymmetry (Rate): \*SubscriptBox[Y, \[CapitalDelta]\[ScriptL]] = ",
		info["Y_L Int"] // sf];
	Print["Hubble parameter: H(\*SubscriptBox[m, 1]) = ",
		info["H(m1)"] // sf, " GeV"];
	Print["Initial temperature: \*SubscriptBox[T, 0] = ",
		info["T0"] // sf, " GeV"];
	Print["Vacuum decay width: \*SubscriptBox[\[CapitalGamma], 1] = ",
		info["\[CapitalGamma]vac1"] // sf, " GeV"];
	Print["Vacuum decay width: \*SubscriptBox[\[CapitalGamma], 2] = ",
		info["\[CapitalGamma]vac2"] // sf, " GeV"];
	Print["Washout strength parameter: \*SubscriptBox[K, 1] = \*SubscriptBox[\[CapitalGamma], 1]/H(\*SubscriptBox[m, 1]) = ",
		info["K1"] // sf];
	Print["Washout strength parameter: \*SubscriptBox[K, 2] = \*SubscriptBox[\[CapitalGamma], 2]/H(\*SubscriptBox[m, 1]) = ",
		info["K2"] // sf];
];

(* Default options. *)
Options[PrintResultInfo] = {
	"digits" -> 4
};


(* ::Subsubsection:: *)
(*Neutrino \[Delta]f plotting*)


(* ::Text:: *)
(*The "data" argument below is the output of the SolveAll function (above).*)


(* Plots \[Delta]f, scaled by (k/T)^2. *)
Plot\[Delta]f[data_, i_, y_, opt:OptionsPattern[]] := LogLinearPlot[
	data["FullNeutrino"]["\[Delta]f"][Log10[z]][[NearestLyIndex[data@"Grid"][Log10@y], i]],
	{z, 10^Min@data["Grid"]["lzCoords"], 10^Max@data["Grid"]["lzCoords"]},
	Evaluate@opt,
	PlotRange -> All,
	FrameLabel -> {"z"},
	PlotLabel -> "\[Delta]f (scaled by \*SuperscriptBox[\[Kappa], 2]), component "
		<>ToString[i]<>", \[Kappa] \[Congruent] k/T = "
		<>ToString@(10^# &)@NearestLy[data@"Grid"][Log10@y],
	PlotLegends -> {"\[Delta]f"},
	PlotStyle -> {{Joined, Hue[0.6]}},
	PlotTheme -> "Scientific",
	ImageSize -> 500
]


(* ::Subsubsection:: *)
(*Lepton function plotting (log-linear)*)


(* Lepton function comparison plot (log-linear). *)
leptonPlot[data_, f_String, opt:OptionsPattern[]] := LogLinearPlot[
	{
		(* We remove the Log[10]*z scaling here for SCP and W. *)
		data["FullLepton"][f][Log10[z]]/If[f == "Y_L", 1, (Log[10]*z)],
		data["BELepton"][f][Log10[z]]/If[f == "Y_L", 1, (Log[10]*z)],
		data["IntSol"][f][Log10[z]]/If[f == "Y_L", 1, (Log[10]*z)]
	},
	{z, 10^Min@data["Grid"]["lzCoords"], 10^Max@data["Grid"]["lzCoords"]},
	Evaluate@opt,
	PlotRange -> All,
	PlotLegends -> {"Main QKE", "Boltzmann", "Rate"},
	PlotStyle -> {{Joined, Hue[0.6]}, {Dashed, Hue[0.35]}, {Dotted, GrayLevel[0.4]}},
	PlotTheme -> "Scientific",
	ImageSize -> 500
];


(* Lepton Y_L plot. *)
PlotAsymm[data_, opt:OptionsPattern[]] := leptonPlot[data, "Y_L", Evaluate@opt,
	PlotLabel -> "Lepton asymmetry",
	FrameLabel -> {"z", "\*SubscriptBox[Y, L]"}
];


(* Lepton S_CP plot. *)
PlotSCP[data_, opt:OptionsPattern[]] := leptonPlot[data, "SCP", Evaluate@opt,
	PlotLabel -> "Lepton source",
	FrameLabel -> {"z", "\*SubscriptBox[S, CP]"}
];


(* Lepton W_B plot. *)
PlotWB[data_, opt:OptionsPattern[]] := leptonPlot[data, "WB", Evaluate@opt,
	PlotLabel -> "Lepton washout B",
	FrameLabel -> {"z", "\*SubscriptBox[W, B]"}
];


(* Lepton W_A plot. *)
PlotWA[data_, opt:OptionsPattern[]] := leptonPlot[data, "WA", Evaluate@opt,
	PlotLabel -> "Lepton washout A",
	FrameLabel -> {"z", "\*SubscriptBox[W, A]"}
];


(* ::Subsubsection:: *)
(*Lepton function plotting (log-log)*)


(* Lepton function comparison plot (log-log). *)
pos[f_] := If[f[#] > 0, f[#]] &;
neg[f_] := If[f[#] < 0, Abs@f[#]] &;

leptonLogPlot[data_, f_String, opt:OptionsPattern[]] := LogLogPlot[
	{
		(* We remove the Log[10]*z scaling here for SCP and W. *)
		pos[data["FullLepton"][f]][Log10[z]]/If[f == "Y_L", 1, (Log[10]*z)],
		pos[data["BELepton"][f]][Log10[z]]/If[f == "Y_L", 1, (Log[10]*z)],
		pos[data["IntSol"][f]][Log10[z]]/If[f == "Y_L", 1, (Log[10]*z)],
		neg[data["FullLepton"][f]][Log10[z]]/If[f == "Y_L", 1, (Log[10]*z)],
		neg[data["BELepton"][f]][Log10[z]]/If[f == "Y_L", 1, (Log[10]*z)],
		neg[data["IntSol"][f]][Log10[z]]/If[f == "Y_L", 1, (Log[10]*z)]
	},
	{z, 10^Min@data["Grid"]["lzCoords"], 10^Max@data["Grid"]["lzCoords"]},
	Evaluate@opt,
	PlotRange -> All,
	PlotLegends -> {
		"Main QKE > 0", "Boltzmann > 0", "Rate > 0",
		"Main QKE < 0", "Boltzmann < 0", "Rate < 0"
	},
	PlotStyle -> {
		{Joined, Hue[0.9]}, {Dashed, Hue[0.9]}, {Dotted, Hue[0.9]},
		{Joined, Hue[0.6]}, {Dashed, Hue[0.6]}, {Dotted, Hue[0.6]}
	},
	PlotTheme -> "Scientific",
	ImageSize -> 500
];


(* Lepton Y_L log-log plot. *)
LogPlotAsymm[data_, opt:OptionsPattern[]] := leptonLogPlot[data, "Y_L", Evaluate@opt,
	PlotLabel -> "Lepton asymmetry",
	FrameLabel -> {"z", "\*SubscriptBox[Y, L]"}
];


(* Lepton S_CP log-log plot. *)
LogPlotSCP[data_, opt:OptionsPattern[]] := leptonLogPlot[data, "SCP", Evaluate@opt,
	PlotLabel -> "Lepton source",
	FrameLabel -> {"z", "\*SubscriptBox[S, CP]"}
];


(* Lepton W_B log-log plot. *)
LogPlotWB[data_, opt:OptionsPattern[]] := leptonLogPlot[data, "WB", Evaluate@opt,
	PlotLabel -> "Lepton washout B",
	FrameLabel -> {"z", "\*SubscriptBox[W, B]"}
];


(* Lepton W_A log-log plot. *)
LogPlotWA[data_, opt:OptionsPattern[]] := leptonLogPlot[data, "WA", Evaluate@opt,
	PlotLabel -> "Lepton washout A",
	FrameLabel -> {"z", "\*SubscriptBox[W, A]"}
];


(* ::Subsubsection:: *)
(*Grid function plotting (heatmaps)*)


(* Grid function plotting. *)
gridPlot[data_, fGrid_, opt:OptionsPattern[]] := ListDensityPlot[
	Transpose@{10^Flatten@data["Grid", "lzGrid"], 10^Flatten@data["Grid", "lyGrid"], Flatten@fGrid},
	Evaluate@opt,
	PlotRange -> All,
	ScalingFunctions -> {"Log", "Log", "Linear"},
	FrameLabel -> {"z", "k/T"},
	PlotLegends -> Automatic,
	PlotTheme -> "Scientific",
	ImageSize -> 500
];


(* Neutrino \[Delta]f heatmap, scaled by (k/T)^2. *)
\[Delta]fHeatmap[data_, i_, opt:OptionsPattern[]] := gridPlot[data,
	data["FullNeutrino", "\[Delta]f"][data["Grid", "lzCoords"]][[All, All, i]],
	Evaluate@opt,
	PlotLabel -> "Neutrino \[Delta]f (scaled by \*SuperscriptBox[\[Kappa], 2]), component "<>ToString[i]
];


(* Neutrino fAd heatmap, scaled by (k/T)^2. *)
fAdHeatmap[data_, i_, opt:OptionsPattern[]] := gridPlot[data,
	data["FullPrecalc", "fAd"][[All, All, i]],
	Evaluate@opt,
	PlotLabel -> "Neutrino \*SubscriptBox[f, ad] (scaled by \*SuperscriptBox[\[Kappa], 2]), component "<>ToString[i]
];


(* Neutrino source term heatmap, scaled by (k/T)^2. *)
SourceHeatmap[data_, i_, opt:OptionsPattern[]] := gridPlot[data,
	(* We remove the Log[10]*z scaling here. *)
	data["FullNeutrino", "SourceTerm"][data["Grid", "lzCoords"]][[All, All, i]]/(Log[10]*10^data["Grid", "lzGrid"]),
	Evaluate@opt,
	PlotLabel -> "Neutrino source term -\*SubscriptBox[d, z]\*SubscriptBox[f, ad] (scaled by \*SuperscriptBox[\[Kappa], 2]), component "<>ToString[i]
];


(* Collision term coefficient heatmap. *)
CollHeatmap[data_, i_, j_, opt:OptionsPattern[]] := gridPlot[data,
	(* We remove the Log[10]*z scaling here. *)
	data["FullPrecalc", "Coll"][[All, All, i, j]]/(Log[10]*10^data["Grid", "lzGrid"]),
	Evaluate@opt,
	PlotLabel -> "Neutrino collision term coefficient ("<>ToString[i]<>","<>ToString[j]<>")"
];


(* ::Subsubsection:: *)
(*Miscellaneous*)


(* Return the nearest ly-value (and its index) from a given LeptoGen grid. *)
NearestLy[grid_][ly_] := First@Nearest[grid@"lyCoords" -> "Element", ly]
NearestLyIndex[grid_][ly_] := Nearest[grid@"lyCoords" -> {"Element", "Index"}, ly][[1, 2]]


(* Check that precalc data are machine numbers and packed arrays. *)
DataIsPacked[precalc_] := Developer`PackedArrayQ /@ precalc;
DataIsMachineNumber[precalc_] := Flatten /* Map[MachineNumberQ] /* DeleteDuplicates /* First /@ precalc;


(* Export data (e.g. a List) to a file in table format. *)
ExportTable[filename_, data_] := Export[filename, data,
	"Table", "FieldSeparators" -> " ",
	CharacterEncoding -> "UTF8"
];


(* ::Subsubsection:: *)
(*End package*)


End[]; (* End private context. *)

Protect["LeptoGenTools`*"];

Block[{$ContextPath}, EndPackage[]];
