(* ::Package:: *)

(* ::Subsubsection:: *)
(*Begin package*)


(* Self-energy functions for LeptoGen. *)
(* Author: Henri Jukkala *)
(* For copyright and license information see included LICENSE.txt *)

BeginPackage["SelfEnergyFunctions`"];

Unprotect["SelfEnergyFunctions`*"];
ClearAll["SelfEnergyFunctions`*"];

(* Exported symbols. *)
abAeqThermal;
abS\[Delta]MassShell;
abA\[Delta]MassShell;

(* Notation used below: z = m/T, y = k/T, y0 = k0/T. *)


(* ::Subsubsection:: *)
(*Begin private*)


Begin["`Private`"]; (* Begin private context. *)


(* ::Subsubsection:: *)
(*Numerical functions (dimensionless, arguments scaled by T)*)


(* ::Text:: *)
(*Common functions and variables*)


pi = N[Pi, $MachinePrecision];
expm1 = Internal`Expm1; (* exp(#) - 1 but avoids underflow near zero *)


(* exp with underflow and overflow catching *)
exp = Which[
	RealAbs[#] < 700., Exp[#],
	Negative[#], 0.,
	Positive[#], Overflow[]
] &;

fFD = If[# < 700., 1./(exp[#] + 1.), 0.] &; (* Fermi-Dirac distribution *)
fBE = If[# < 700., 1./expm1[#], 0.] &; (* Bose-Einstein distribution *)


(* Auxiliary function: returns y0, yp, ym. *)
y0ypym[z_, y_] := Module[{y0, yp, ym},
	y0 = Sqrt[y^2 + z^2]; (* mass shell energy*)
	yp = (y0 + y)/2;
	ym = z^2/(4*yp); (* equals (y0 - y)/2 but avoids loss of precision *)
	{y0, yp, ym}
];


(* ::Text:: *)
(*\[CapitalSigma]^A_eq (divided to vacuum and thermal parts)*)


(* a^A_eq[y0, y] and b^A_eq[y0, y] evaluated at the mass shell y0 = Sqrt[y^2 + z^2]. *)
abAeqMassShell[z_?NumericQ, y_?NumericQ] := abAeqVac[z, y] + abAeqThermal[z, y];
SetAttributes[abAeqMassShell, Listable];


abAeqVac[z_?NumericQ, y_?NumericQ] := Module[{y0, afun, bfun},
	y0 = First@y0ypym[z, y];
	afun = 1./(32*pi)*y0;
	bfun = -1./(32*pi)*y;
	Re@{afun, bfun}
];
SetAttributes[abAeqVac, Listable];


abAeqThermal[z_?NumericQ, y_?NumericQ] := Module[
	{y0, yp, ym, thres, opt, afun, bfun, aint, bint},
	
	{y0, yp, ym} = y0ypym[z, y];
	thres = 0.9 + UnitStep[y - 1000.]*(0.1 - 100./y);
	opt = {
		Method -> {
			"GlobalAdaptive",
			Method -> {"GaussKronrodRule", "Points" -> Automatic},
			"SingularityHandler" -> "DoubleExponential",
			"SingularityDepth" -> Automatic,
			"SymbolicProcessing" -> None
		},
		AccuracyGoal -> 14,
		PrecisionGoal -> 11,
		WorkingPrecision -> $MachinePrecision,
		MaxRecursion -> Automatic
	};
	
	(* Note here -fFD[x] instead of fFD[-x] because it's thermal part only! *)
	afun = 1./(16*pi)*NIntegrate[
		(ym + y*t)*(-fFD[ym + y*t] + fBE[ym + y*(1 - t)]), {t, 0, thres, 1}, Evaluate@opt
	];
	bfun = -1./(16*pi)*NIntegrate[
		(-ym + y0*t)*(-fFD[ym + y*t] + fBE[ym + y*(1 - t)]), {t, 0, thres, 1}, Evaluate@opt
	];
	
	Re@{afun, bfun}
];
SetAttributes[abAeqThermal, Listable];


(* ::Text:: *)
(*\[Delta]\[CapitalSigma]^< (1. order correction in lepton \[Mu], vanishes in vacuum)*)


(* a^<_\[Delta][y0, y] and b^<_\[Delta][y0, y] evaluated at the mass shell y0 = Sqrt[y^2 + z^2]. *)
abS\[Delta]MassShell[z_?NumericQ, y_?NumericQ] := Module[
	{y0, yp, ym, thres, opt, afun, bfun, aint, bint},
	
	{y0, yp, ym} = y0ypym[z, y];
	thres = 0.9 + UnitStep[y - 1000.]*(0.1 - 100./y);
	opt = {
		Method -> {
			"GlobalAdaptive",
			Method -> {"GaussKronrodRule", "Points" -> Automatic},
			"SingularityHandler" -> "DoubleExponential",
			"SingularityDepth" -> Automatic,
			"SymbolicProcessing" -> None
		},
		AccuracyGoal -> 14,
		PrecisionGoal -> 11,
		WorkingPrecision -> $MachinePrecision,
		MaxRecursion -> Automatic
	};
	
	afun = 1./(8*pi)*NIntegrate[
		(ym + y*t)*fFD[ym + y*t]*(1 - fFD[ym + y*t])*fBE[ym + y*(1 - t)], {t, 0, thres, 1},
		Evaluate@opt
	];
	bfun = -1./(8*pi)*NIntegrate[
		(-ym + y0*t)*fFD[ym + y*t]*(1 - fFD[ym + y*t])*fBE[ym + y*(1 - t)], {t, 0, thres, 1},
		Evaluate@opt
	];
	
	Re@{afun, bfun}
];
SetAttributes[abS\[Delta]MassShell, Listable];


(* ::Text:: *)
(*\[Delta]\[CapitalSigma]^A (1. order correction in lepton \[Mu], vanishes in vacuum)*)


(* a^A_\[Delta][y0, y] and b^A_\[Delta][y0, y] evaluated at the mass shell y0 = Sqrt[y^2 + z^2]. *)
abA\[Delta]MassShell[z_?NumericQ, y_?NumericQ] := Module[
	{y0, yp, ym, thres, opt, afun, bfun, aint, bint},
	
	{y0, yp, ym} = y0ypym[z, y];
	thres = 0.9 + UnitStep[y - 1000.]*(0.1 - 100./y);
	opt = {
		Method -> {
			"GlobalAdaptive",
			Method -> {"GaussKronrodRule", "Points" -> Automatic},
			"SingularityHandler" -> "DoubleExponential",
			"SingularityDepth" -> Automatic,
			"SymbolicProcessing" -> None
		},
		AccuracyGoal -> 14,
		PrecisionGoal -> 11,
		WorkingPrecision -> $MachinePrecision,
		MaxRecursion -> Automatic
	};
	
	afun = 1./(16*pi)*NIntegrate[
		-(ym + y*t)*fFD[ym + y*t]*(1 - fFD[ym + y*t]), {t, 0, thres, 1}, Evaluate@opt
	];
	bfun = -1./(16*pi)*NIntegrate[
		(ym - y0*t)*fFD[ym + y*t]*(1 - fFD[ym + y*t]), {t, 0, thres, 1}, Evaluate@opt
	];
	
	Re@{afun, bfun}
];
SetAttributes[abA\[Delta]MassShell, Listable];


(* ::Subsubsection:: *)
(*End*)


(* Protect all exported symbols. *)
Protect["SelfEnergyFunctions`*"];

End[]; (* End private context. *)

Block[{$ContextPath}, EndPackage[]];
