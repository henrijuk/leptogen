(* ::Package:: *)

(* ::Subsection:: *)
(*Semiclassical Boltzmann rate equations*)


Begin["`BoltzmannRateEqs`"];


Yeq[z_, xi_, scoeff_] := (z*xi)^2*BesselK[2, z*xi]/(scoeff*\[Pi]^2);
DYeq[z_, xi_, scoeff_] := -xi*(z*xi)^2*BesselK[1, z*xi]/(scoeff*\[Pi]^2); (* z-derivative *)
Dfun[z_, xi_, \[CapitalGamma]i_, H1_] := \[CapitalGamma]i/H1*zbesselratio[z*xi]/xi;
Wfun[z_, xi_, \[CapitalGamma]i_, H1_] := 1/4*\[CapitalGamma]i/H1*z*(z*xi)^2*BesselK[1, z*xi];
zbesselratio[z_] := If[z > 200., z - 3/2 + 15/8*(1/z - 1/z^2), z*BesselK[1, z]/BesselK[2, z]];


BESolve[grid_, phys_, settings_] := Module[
	{Y, \[Theta], x, m, m1, H1, sc, \[CapitalGamma]0, \[CapitalGamma], \[Epsilon]CP, f, S, lzc, iv, \[Delta]Y1sol, \[Delta]Y2sol, YLsol},
	
	(* Input argument unpacking. *)
	{Y[1], Y[2], \[Theta][1,2], x[1], x[2], m1, H1, sc} =
		{#Y1, #Y2, #\[Theta]12, #x1, #x2, #m1, #H1, #scoeff} &[phys];
	
	m[i_] := x[i]*m1;
	\[CapitalGamma]0[i_] := Y[i]^2*m[i]/(8*\[Pi]);
	\[Epsilon]CP = LeptoGen`Private`BoltzmannCP`\[Epsilon][m, \[CapitalGamma]0, \[Theta], settings];
	
	(* Source terms. *)
	S["1"][lz_] := -(Log[10]*10^lz)*DYeq[10^lz, x[1], sc];
	S["2"][lz_] := -(Log[10]*10^lz)*DYeq[10^lz, x[2], sc];
	S["L"][lz_] := 0;
	
	(* Linear term coefficient functions. *)
	f["1,1"][lz_] := -(Log[10]*10^lz)*Dfun[10^lz, x[1], \[CapitalGamma]0[1], H1];
	f["1,2"][lz_] := 0;
	f["1,L"][lz_] := -(Log[10]*10^lz)*\[Epsilon]CP[1]*Wfun[10^lz, x[1], \[CapitalGamma]0[1], H1];
	
	f["2,1"][lz_] := 0;
	f["2,2"][lz_] := -(Log[10]*10^lz)*Dfun[10^lz, x[2], \[CapitalGamma]0[2], H1];
	f["2,L"][lz_] := -(Log[10]*10^lz)*\[Epsilon]CP[2]*Wfun[10^lz, x[2], \[CapitalGamma]0[2], H1];
	
	f["L,1"][lz_] := (Log[10]*10^lz)*\[Epsilon]CP[1]*Dfun[10^lz, x[1], \[CapitalGamma]0[1], H1];
	f["L,2"][lz_] := (Log[10]*10^lz)*\[Epsilon]CP[2]*Dfun[10^lz, x[2], \[CapitalGamma]0[2], H1];
	f["L,L"][lz_] := -(Log[10]*10^lz)*(
		Wfun[10^lz, x[1], \[CapitalGamma]0[1], H1] + Wfun[10^lz, x[2], \[CapitalGamma]0[2], H1]
	);
	
	(* Drop lepton backreaction term in the neutrino equations. *)
	If[settings@"BoltzmannDiscardBackreaction",
		f["1,L"][lz_] := 0;
		f["2,L"][lz_] := 0
	];
	
	(* Drop neutrino equation source term. *)
	If[settings@"DiscardNeutrinoSourceTerm",
		S["1"][lz_] := 0;
		S["2"][lz_] := 0
	];
	
	lzc = grid@"lzCoords";
	
	(* Initial values. *)
	iv = If[settings@"ThermalInitialCondition",
		{0., 0., 0.}, (* thermal initial value for Majorana neutrinos *)
		{-Yeq[10^(Min@lzc), x[1], sc], -Yeq[10^(Min@lzc), x[2], sc], 0.} (* vacuum iv *)
	];
	
	{\[Delta]Y1sol, \[Delta]Y2sol, YLsol} = NDSolveValue[
		{
			\[Delta]Y1'[lz] == S["1"][lz] + f["1,1"][lz]*\[Delta]Y1[lz] + f["1,L"][lz]*YL[lz],
			\[Delta]Y2'[lz] == S["2"][lz] + f["2,2"][lz]*\[Delta]Y2[lz] + f["2,L"][lz]*YL[lz],
			YL'[lz] == f["L,1"][lz]*\[Delta]Y1[lz] + f["L,2"][lz]*\[Delta]Y2[lz] + f["L,L"][lz]*YL[lz],
			\[Delta]Y1[Min@lzc] == iv[[1]],
			\[Delta]Y2[Min@lzc] == iv[[2]],
			YL[Min@lzc] == iv[[3]]
		},
		{\[Delta]Y1, \[Delta]Y2, YL},
		{lz, Min@lzc, Max@lzc},
		settings@"BoltzmannDESettings", WorkingPrecision -> MachinePrecision
	];
	
	<|
		"\[Delta]Y1" -> \[Delta]Y1sol,
		"\[Delta]Y2" -> \[Delta]Y2sol,
		"Y_L" -> YLsol,
		"SCP" -> (f["L,1"][#]*\[Delta]Y1sol[#] + f["L,2"][#]*\[Delta]Y2sol[#] &),
		"WB" -> (f["L,L"][#] &)
	|>
];


End[];
