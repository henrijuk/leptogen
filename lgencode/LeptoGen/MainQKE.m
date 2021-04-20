(* ::Package:: *)

(* ::Subsection:: *)
(*Main quantum kinetic equations*)


Begin["`MainQKE`"];

(* Load subfiles. *)
Get["Neutrino.m", Path -> LeptoGen`Private`$pkgDir];
Get["Lepton.m", Path -> LeptoGen`Private`$pkgDir];
Get["Components.m", Path -> LeptoGen`Private`$pkgDir];


(* ::Subsubsection:: *)
(*Pre-calculation*)


(* ::Text:: *)
(*Auxiliary functions and variables*)


cw = 2; (* lepton SU(2)-doublet multiplicity factor *)
omega[k_, m_] := Sqrt[k^2 + m^2];
fermiDirac[y0_] := If[y0 > 700., 0., 1./(Exp[y0] + 1.)];
SetAttributes[fermiDirac, Listable];


(* Creates a nested Association from a function and lists of keys. *)
nestedAssocMap[f_, keys__List] := GroupBy[Tuples@{keys}, Map[Extract, Range@Length@{keys}], Apply[f]@*First];


(* Transposes the dimensions of a rectangular array cyclically n (default : 1) places to the right. *)
rotateDims[s_?ArrayQ, n:(_?IntegerQ):1] := Transpose[s, RotateLeft[Range@ArrayDepth@s, n]];


(* ::Text:: *)
(*Pre-calculation*)


(* Returns an Association of precalculated data on the grid for all needed functions. *)
Precalc[init_, grid_, phys_, settings_] := Module[
	{Y, \[Theta], x, m1, sc, H1, \[CapitalSigma]Int, lz, ly, z, y, gridDepth, vec, mat,
	i, k, m, \[Omega], feq, Dfeq, \[CapitalSigma]eq, D\[CapitalSigma]eq, \[Delta]\[CapitalSigma], SG, neutrino, lepton},
	
	(* Input argument unpacking and other shorthands. *)
	{Y[1], Y[2], \[Theta][1, 2], x[1], x[2], m1, H1, sc} =
		{#Y1, #Y2, #\[Theta]12, #x1, #x2, #m1, #H1, #scoeff} &[phys];
	\[CapitalSigma]Int = init["SigmaThermalInterp"];
	{lz, ly} = {#lzGrid, #lyGrid} &[grid];
	{z, y} = {10^lz, 10^ly};
	
	gridDepth = ArrayDepth@lz;
	If[ArrayDepth@lz =!= ArrayDepth@ly, Message[Precalc::gridError]];
	i = {1, 2}; (* neutrino flavors *)
	
	(* Evaluate some (dimensionless) grid variables once-and-for-all (for each flavor). *)
	k = y;
	m = AssociationMap[x[#]*z &, i];
	\[Omega] = AssociationMap[omega[y, x[#]*z] & , i];
	feq = AssociationMap[fermiDirac@omega[y, x[#]*z] &, i];
	Dfeq = -feq*(1 - feq); (* y0-derivative of feq(y0) *)
	\[CapitalSigma]eq = nestedAssocMap[\[CapitalSigma]Int[#2<>#1<>"eq"]["id"][lz, ly, x[#3]] &, {"A"}, {"a", "b"}, i];
	D\[CapitalSigma]eq = nestedAssocMap[\[CapitalSigma]Int[#2<>#1<>"eq"]["y0D1"][lz, ly, x[#3]] &, {"A"}, {"a", "b"}, i];
	\[Delta]\[CapitalSigma] = nestedAssocMap[\[CapitalSigma]Int[#2<>#1<>"\[Delta]"][lz, ly, x[#3]] &, {"S", "A"}, {"a", "b"}, i];
	
	(* Conversion functions for the vector form of the neutrino equation. *)
	vec = Which[
		settings@"AttractorApproximation", `Components`vmfVector4,
		settings@"DecouplingApproximation", `Components`vmfVector3,
		settings@"HelicityEvenOnly", `Components`vmfVector2,
		True, `Components`vmfVector
	];
	mat = Which[
		settings@"AttractorApproximation", `Components`vmfMatrix4,
		settings@"DecouplingApproximation", `Components`vmfMatrix3,
		settings@"HelicityEvenOnly", `Components`vmfMatrix2,
		True, `Components`vmfMatrix
	];
	
	SG = If[settings@"UseWightmanGreater", ">", "<"];
	
	(* Explanation of the scalings used below:
	   - y^2 and 1/(2\[Pi]^2) for the isotropic d^3y integration.
	     - Note that y^2 is already included in fAd. (and hence also in \[Delta]f)
	   - z/H1 from d/dt -> d/dz.
	   - T from scaling of the Hamilton, Coll.
	   - T^4 from scaling of SCP, W.
	   - 1/scoeff from entropy normalisation (s = scoeff*T^3, for SCP only).
	     - Together these make T*z/H1 = m1/H1.
	   - Log[10]*z from d/dz -> d/d(log10 z) (in the DE).
	   - Log[10]*y from dy -> d(log10 y) (in the y-integral implementation).
	   - Factor of 6/cw from \[Mu] -> n_ell - nbar_ell.
	   The scalings are given in the vector/matrix conversion functions below. *)
	
	(* Evaluate neutrino functions numerically on the grid. Conversion to vector/matrix form, with scaling. *)
	neutrino = <|
		"fAd" -> If[settings@"TreeLevelAdSolution",
			vec[y^2]@`Neutrino`fAdTree[SG][feq],
			vec[y^2]@`Neutrino`fAd[SG, Y, \[Theta]][k, m, \[Omega], feq, Dfeq, \[CapitalSigma]eq, D\[CapitalSigma]eq]
			],
		"fAdVac" -> vec[y^2]@`Neutrino`fAdVac[SG],
		"Hamilton" -> mat[m1/H1*Log[10]*z]@`Neutrino`Hamilton[\[Omega]],
		"Coll" -> mat[m1/H1*Log[10]*z]@`Neutrino`Coll[Y, \[Theta]][k, m, \[Omega], feq, \[CapitalSigma]eq]
	|>;
	
	(* Calculate the lz-derivative of fAd. *)
	neutrino["DfAd"] = If[settings@"TreeLevelAdSolution",
		vec[Log[10]*z*y^2]@`Neutrino`DtfAdTree[SG][m, x, \[Omega], Dfeq],
		grid["lzDerivativeOperator"] /@ neutrino["fAd"] (* numerical derivative *)
	];
	
	(* Evaluate lepton functions numerically on the grid. *)
	lepton = <|
		"SCPCoeff" -> If[settings@"AttractorApproximation",
			vec[Log[10]*z*m1/H1/sc*Log[10]*y/(2*\[Pi]^2)]@`Lepton`SCPCoeffAttractor[Y, \[Theta]][k, m, \[Omega], \[CapitalSigma]eq],
			vec[Log[10]*z*m1/H1/sc*Log[10]*y/(2*\[Pi]^2)]@`Lepton`SCPCoeff[Y, \[Theta]][k, m, \[Omega], \[CapitalSigma]eq]
			],
		"WACoeff" -> If[settings@"AttractorApproximation",
			vec[Log[10]*z*m1/H1*6/cw*Log[10]*y/(2*\[Pi]^2)]@`Lepton`WACoeffAttractor[Y, \[Theta]][k, m, \[Omega], \[Delta]\[CapitalSigma]],
			vec[Log[10]*z*m1/H1*6/cw*Log[10]*y/(2*\[Pi]^2)]@`Lepton`WACoeff[Y, \[Theta]][k, m, \[Omega], \[Delta]\[CapitalSigma]]
			],
		"WBInt" -> (Log[10]*z*m1/H1*6/cw*Log[10]*y^3/(2*\[Pi]^2))*`Lepton`WBInt[Y][k, \[Omega], feq, \[Delta]\[CapitalSigma]]
	|>;
	
	(* Rotate the grid point dimensions from last to first. *)
	neutrino = rotateDims[#, gridDepth] & /@ neutrino;
	lepton = rotateDims[#, gridDepth] & /@ lepton;
	
	(* Initial value for neutrino \[Delta]f. *)
	neutrino["\[Delta]fInitialValue"] = If[settings@"ThermalInitialCondition",
		0*First@neutrino["fAd"], (* thermal initial value *)
		First[neutrino["fAdVac"] - neutrino["fAd"]] (* vacuum initial value *)
	];
	
	(* Output numerical data. *)
	Join[neutrino, lepton]
];

(* Messages. *)
Precalc::gridError = "The z and y grid depths don't match.";


(* ::Subsubsection:: *)
(*Neutrino DE solver*)


(* Solves the neutrino DE in the helicity-even-odd (hEO) form. *)
NeutrinoEqSolve[grid_, precalc_, settings_] := Module[
	{lzc, lzinterp, lincoeff, s, source, initial, linear, \[Delta]f, lz, \[Delta]fsol},
	
	lzc = grid@"lzCoords";
	lzinterp = grid["Interpolator1"][lzc, #] &; (* uses interpolator no.1 *)
	
	lincoeff = lzinterp[precalc@"Hamilton" - precalc@"Coll"];
	s = If[settings@"DiscardNeutrinoSourceTerm", 0, 1];
	source = lzinterp[-s*precalc@"DfAd"];
	initial = precalc@"\[Delta]fInitialValue";
	
	(* Vectorized (in ly) calculation of the lincoeff.\[Delta]f (matrix.vector) product. *)
	linear[lz_, f_?(ArrayQ[#, _, NumberQ] &)] := Apply[Dot, Transpose@{lincoeff[lz], f}, {1}];
	
	\[Delta]fsol = NDSolveValue[
		{\[Delta]f'[lz] == linear[lz, \[Delta]f[lz]] + source[lz], \[Delta]f[Min@lzc] == initial},
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


(* Solves the lepton asymmetry DE for a given neutrino \[Delta]f function (in hEO-form). *)
LeptonEqSolve[grid_, precalc_, settings_, \[Delta]f_] := Module[
	{lzc, lyc, data = <||>, osc, sSG, lyinterp, lyint, lzinterp, scp, wa, wb, YL, la, YLsol,
	lzGridInterp, SCPInterp, WAInterp, WBInterp},
	
	lyc = grid@"lyCoords";
	lzc = If[settings@"LeptonDEUseNeutrinoGrid",
		First@\[Delta]f["Coordinates"], (* optional, use \[Delta]f lz-grid *)
		grid@"lzCoords"]; (* default to the precalc lz-grid *)
	
	sSG = If[settings@"UseWightmanGreater", -1, 1];
	data["\[Delta]f"] = sSG*Transpose@\[Delta]f@lzc; (* transpose (lz,ly) into (ly,lz) *)
	
	{data["SCPCoeff"], data["WACoeff"], data["WBInt"]} = If[settings@"LeptonDEUseNeutrinoGrid",
		(* Optionally use \[Delta]f grid for SCP and W interpolation. *)
		lzGridInterp = Interpolation[Transpose@{grid@"lzCoords", #}, InterpolationOrder -> 1] &;
		SCPInterp = lzGridInterp@precalc["SCPCoeff"];
		WAInterp = lzGridInterp@precalc["WACoeff"];
		WBInterp = lzGridInterp@precalc["WBInt"];
		{Transpose@SCPInterp[lzc], Transpose@WAInterp[lzc], Transpose@WBInterp[lzc]},
		(* Default to the precalc lz-grid. *)
		{Transpose@precalc["SCPCoeff"], Transpose@precalc["WACoeff"], Transpose@precalc@"WBInt"}
	];
	
	(* Vectorized calculation of the (SCP or WA).\[Delta]f (vector.vector) product. *)
	data["SCPInt"] = Apply[Dot, Transpose[{data["SCPCoeff"], data["\[Delta]f"]}, {3, 1, 2, 4}], {2}];
	data["WAInt"] = Apply[Dot, Transpose[{data["WACoeff"], data["\[Delta]f"]}, {3, 1, 2, 4}], {2}];
	If[settings@"DiscardWashoutA", data["WAInt"] *= 0];
	
	(* Functions for performing the numerical integration over ly. *)
	lyinterp = Interpolation[Transpose@{lyc, #}, InterpolationOrder -> 1] &;
	lyint = Integrate[#[ly], {ly, Min@lyc, Max@lyc}] &;
	(* Function for making the final lz-interpolation functions. *)
	lzinterp = grid["Interpolator2"][lzc, #] &; (* uses interpolator no.2 *)
	
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
		"SCPdata" -> data["SCPInt"] (* SCP-integrand *)
	|>
];


End[];
