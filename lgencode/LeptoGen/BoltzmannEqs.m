(* ::Package:: *)

(* ::Subsection:: *)
(*Semiclassical Boltzmann equations*)


Begin["`BoltzmannEqs`"];


(* ::Subsubsection:: *)
(*Pre-calculation*)


(* ::Text:: *)
(*Auxiliary functions and variables*)


omega[k_, m_] := Sqrt[k^2 + m^2];
fFD[y0_] := If[y0 > 700., 0., 1./(Exp[y0] + 1.)];
SetAttributes[fFD, Listable];


(* Transposes the dimensions of a rectangular array cyclically n (default : 1) places to the right. *)
rotateDims[s_?ArrayQ, n:(_?IntegerQ):1] := Transpose[s, RotateLeft[Range@ArrayDepth@s, n]];


(* ::Text:: *)
(*Pre-calculation*)


(* Returns an Association of precalculated data on the grid for all needed functions. *)
Precalc[init_, grid_, phys_, settings_] := Module[
	{Y, \[Theta], x = <||>, m1, H1, sc, lz, ly, z, y, gridDepth,
	i, m, \[CapitalGamma]0, \[Epsilon]CP, zx, w, feq, Dfeq, Ii, Ji1, Ji2, neutrino, lepton},
	
	(* Input argument unpacking and other shorthands. *)
	{Y[1], Y[2], \[Theta][1, 2], x[1], x[2], m1, H1, sc} =
		{#Y1, #Y2, #\[Theta]12, #x1, #x2, #m1, #H1, #scoeff} &[phys];
	{lz, ly} = {#lzGrid, #lyGrid} &[grid];
	{z, y} = {10^lz, 10^ly};
	
	gridDepth = ArrayDepth@lz;
	If[ArrayDepth@lz =!= ArrayDepth@ly, Message[Precalc::gridError]];
	i = {1, 2}; (* neutrino flavor *)
	
	(* Constant quantities. *)
	m[i_] := x[i]*m1;
	\[CapitalGamma]0[i_] := Y[i]^2*m[i]/(8*\[Pi]);
	\[Epsilon]CP = LeptoGen`Private`BoltzmannCP`\[Epsilon][m, \[CapitalGamma]0, \[Theta], settings];
	
	(* Evaluate some grid functions numerically once-and-for-all (for each flavor). *)
	zx = AssociationMap[x[#]*z &, i];
	w = AssociationMap[omega[y, x[#]*z] &, i];
	feq = AssociationMap[fFD@omega[y, x[#]*z] &, i];
	Dfeq = -feq*(1 - feq); (* y0-derivative of feq(y0) *)
	Ii = AssociationMap[1/y*Log[(Exp[w[#] + y] - 1)/(Exp[w[#]] - Exp[y])] &, i];
	Ji1 = AssociationMap[1/y*(fFD[(w[#] - y)/2] - fFD[(w[#] + y)/2]) &, i];
	Ji2 = feq*(1 - feq)*Ii;
	
	(* Evaluate neutrino functions numerically on the grid. *)
	neutrino = <|
		"feq" -> List @@ (y^2*feq),
		"Dfeq" -> List @@ AssociationMap[(Log[10]*z)*y^2*x[#]*zx[#]/w[#]*Dfeq[#] &, i], (* lz-derivative of feq *)
		"Coll" -> List @@ AssociationMap[(Log[10]*z)*(\[CapitalGamma]0[#]/H1*z)*zx[#]/w[#]*Ii[#] &, i]
	|>;
	
	(* Evaluate lepton functions numerically on the grid. *)
	lepton = <|
		"SCPCoeff" -> List@@AssociationMap[(Log[10]*z)*(\[Epsilon]CP[#]*\[CapitalGamma]0[#]/H1*z)*2/sc*(Log[10]*y/(2*\[Pi]^2))*zx[#]/w[#]*Ii[#] &, i],
		"WACoeff" -> List@@AssociationMap[-(Log[10]*z)*(\[CapitalGamma]0[#]/H1*z)*6*(Log[10]*y/(2*\[Pi]^2))*zx[#]/w[#]*Ji1[#] &, i],
		"WBInt" -> Plus@@AssociationMap[-(Log[10]*z)*(\[CapitalGamma]0[#]/H1*z)*6*(Log[10]*y^3/(2*\[Pi]^2))*zx[#]/w[#]*Ji2[#] &, i]
	|>;
	
	(* Rotate the grid point dimensions from last to first. *)
	neutrino = rotateDims[#, gridDepth] & /@ neutrino;
	lepton = rotateDims[#, gridDepth] & /@ lepton;
	
	(* Initial value for neutrino \[Delta]f. *)
	neutrino["\[Delta]fInitialValue"] = If[settings@"ThermalInitialCondition",
		0*First@neutrino["feq"], (* thermal initial value *)
		First[-neutrino["feq"]] (* vacuum initial value *)
	];
	
	(* Output numerical data. *)
	Join[neutrino, lepton]
];

(* Messages. *)
Precalc::gridError = "The z and y grid depths don't match.";


(* ::Subsubsection:: *)
(*Neutrino DE solver*)


(* Solves the neutrino DE. *)
NeutrinoEqSolve[grid_, precalc_, settings_] := Module[
	{lzc, lzinterp, lincoeff, s, source, initial, linear, \[Delta]f, lz, \[Delta]fsol},
	
	lzc = grid@"lzCoords";
	lzinterp = grid["Interpolator1"][lzc, #] &; (* uses interpolator no.1 *)
	
	lincoeff = lzinterp[-precalc@"Coll"];
	s = If[settings@"DiscardNeutrinoSourceTerm", 0, 1];
	source = lzinterp[-s*precalc@"Dfeq"];
	initial = precalc@"\[Delta]fInitialValue";
	
	\[Delta]fsol = NDSolveValue[
		{\[Delta]f'[lz] == lincoeff[lz]*\[Delta]f[lz] + source[lz], \[Delta]f[Min@lzc] == initial},
		\[Delta]f, {lz, Min@lzc, Max@lzc},
		settings@"NeutrinoDESettings", WorkingPrecision -> MachinePrecision
	];
	
	(* Output interpolation functions. *)
	<|
		"\[Delta]f" -> \[Delta]fsol,
		"LinearTermCoeff" -> lincoeff,
		"SourceTerm" -> source,
		"InitialValue" -> initial
	|>
];


(* ::Subsubsection:: *)
(*Lepton asymmetry DE solver*)


(* Solves the lepton asymmetry DE for a given neutrino \[Delta]f function. *)
LeptonEqSolve[grid_, precalc_, settings_, \[Delta]f_] := Module[
	{lzc, lyc, data = <||>, lyinterp, lyint, lzinterp, scp, wa, wb, YL, la, YLsol},
	
	lzc = grid@"lzCoords";
	lyc = grid@"lyCoords";
	lzinterp = grid["Interpolator2"][lzc, #] &; (* uses interpolator no.2 *)
	
	data["\[Delta]f"] = Transpose@\[Delta]f@lzc; (* transpose (lz,ly) into (ly,lz) *)
	data["SCPCoeff"] = Transpose@precalc["SCPCoeff"];
	data["WACoeff"] = Transpose@precalc["WACoeff"];
	data["WBInt"] = Transpose@precalc["WBInt"];
	
	(* Vectorized calculation of the (SCP or WA).\[Delta]f (vector.vector) product. *)
	data["SCPInt"] = Apply[Dot, Transpose[{data["SCPCoeff"], data["\[Delta]f"]}, {3, 1, 2, 4}], {2}];
	data["WAInt"] = Apply[Dot, Transpose[{data["WACoeff"], data["\[Delta]f"]}, {3, 1, 2, 4}], {2}];
	If[settings@"DiscardWashoutA", data["WAInt"] *= 0];
	
	(* Functions for performing the numerical integration over ly. *)
	lyinterp = Interpolation[Transpose@{lyc, #}, InterpolationOrder -> 1] &;
	lyint = Integrate[#[ly], {ly, Min@lyc, Max@lyc}] &;
	
	{scp, wa, wb} = lzinterp@*lyint@*lyinterp /@ {data@"SCPInt", data@"WAInt", data@"WBInt"};
	
	YLsol = NDSolveValue[
		{YL'[lz] == (wa[lz] + wb[lz])*YL[lz] + scp[lz], YL[Min@lzc] == 0.},
		YL, {lz, Min@lzc, Max@lzc},
		settings@"LeptonDESettings", WorkingPrecision -> MachinePrecision
	];
	
	(* Output interpolation functions . *)
	<|
		"Y_L" -> YLsol, (* entropy-normalized lepton asymmetry *)
		"SCP" -> scp,
		"WA" -> wa,
		"WB" -> wb,
		"SCPData" -> data["SCPInt"] (* SCP-integrand *)
	|>
];


End[];
