(* ::Package:: *)

(* ::Subsubsection:: *)
(*Lepton equation functions*)


(* Notes:
   - SCP and WA coefficients will be multiplied with \[Delta]f.
   - "E" and "O" refer to the helicity even/odd part of the corresponding \[Delta]f component.
   - SCP and WA coefficients of Re(\[Delta]f_12) and Im(\[Delta]f_12) are given with(out) an explicit imaginary
     unit \[ImaginaryI], which only specifies whether this contributes to the Re or Im part.
   - Externally given self-energy functions (\[CapitalSigma]eq and \[Delta]\[CapitalSigma]) contain T-dependent parts only.
   - The vacuum part of Sigma^A_eq is hardcoded to avoid loss of precision due to cancellations.
*)

Begin["`Lepton`"];


cw = 2; (* lepton SU(2)-doublet multiplicity factor *)
cvac = 1/(32*\[Pi]); (* coefficient of Sigma^A_vac *)


(* Coefficients of \[Delta]f in the integrand of SCP. Integration measure not included. *)
SCPCoeff[Y_, \[Theta]_][k_, m_, \[Omega]_, \[CapitalSigma]eq_] := Module[
	{coeff12, scp = <||>},
	
	coeff12 = 1/Sqrt[2*\[Omega][1]*\[Omega][2]*(-k^2 + m[1]*m[2] + \[Omega][1]*\[Omega][2])];
	
	scp["E"] = <|
		"1,1" -> 0,
		"2,2" -> 0,
		"1,2" -> -I*2*coeff12*Sin[\[Theta][1, 2]]*Y[1]*Y[2]*(
			(m[1]*\[Omega][2] + m[2]*\[Omega][1])*(\[CapitalSigma]eq["A"]["a"][1] + \[CapitalSigma]eq["A"]["a"][2]) +
			k*(m[1] + m[2])*(\[CapitalSigma]eq["A"]["b"][1] + \[CapitalSigma]eq["A"]["b"][2]) +
			cvac*(m[1] + m[2])*(-k^2 + m[1]*m[2] + \[Omega][1]*\[Omega][2])
		)
	|>;
	
	(* There is no vacuum part for 1,1 and 2,2 here (hence the explicit 0). *)
	scp["O"] = <|
		"1,1" -> 2*Y[1]^2*(\[CapitalSigma]eq["A"]["b"][1] + k/\[Omega][1]*\[CapitalSigma]eq["A"]["a"][1] + cvac*0),
		"2,2" -> 2*Y[2]^2*(\[CapitalSigma]eq["A"]["b"][2] + k/\[Omega][2]*\[CapitalSigma]eq["A"]["a"][2] + cvac*0),
		"1,2" -> 2*coeff12*Cos[\[Theta][1, 2]]*Y[1]*Y[2]*(
			(m[1]*\[Omega][2] + m[2]*\[Omega][1])*(\[CapitalSigma]eq["A"]["b"][1] + \[CapitalSigma]eq["A"]["b"][2]) +
			k*(m[1] + m[2])*(\[CapitalSigma]eq["A"]["a"][1] + \[CapitalSigma]eq["A"]["a"][2]) +
			cvac*k*(m[1] - m[2])*(\[Omega][1] - \[Omega][2])
		)
	|>;
	
	2*cw*scp (* additional 2 is needed due to hEO definition *)
];


(* Coefficients of \[Delta]f in the integrand of WA. Integration measure not included. *)
WACoeff[Y_, \[Theta]_][k_, m_, \[Omega]_, \[Delta]\[CapitalSigma]_] := Module[
	{coeff12, wa = <||>},
	
	coeff12 = 1/Sqrt[2*\[Omega][1]*\[Omega][2]*(-k^2 + m[1]*m[2] + \[Omega][1]*\[Omega][2])];
	
	wa["E"] = <|
		"1,1" -> 2*Y[1]^2*(\[Delta]\[CapitalSigma]["A"]["a"][1] + k/\[Omega][1]*\[Delta]\[CapitalSigma]["A"]["b"][1]),
		"2,2" -> 2*Y[2]^2*(\[Delta]\[CapitalSigma]["A"]["a"][2] + k/\[Omega][2]*\[Delta]\[CapitalSigma]["A"]["b"][2]),
		"1,2" -> 2*coeff12*Cos[\[Theta][1, 2]]*Y[1]*Y[2]*(
			(m[1]*\[Omega][2] + m[2]*\[Omega][1])*(\[Delta]\[CapitalSigma]["A"]["a"][1] + \[Delta]\[CapitalSigma]["A"]["a"][2]) +
			k*(m[1] + m[2])*(\[Delta]\[CapitalSigma]["A"]["b"][1] + \[Delta]\[CapitalSigma]["A"]["b"][2])
		)
	|>;
	
	wa["O"] = <|
		"1,1" -> 0,
		"2,2" -> 0,
		"1,2" -> -I*2*coeff12*Sin[\[Theta][1, 2]]*Y[1]*Y[2]*(
			(m[1]*\[Omega][2] + m[2]*\[Omega][1])*(\[Delta]\[CapitalSigma]["A"]["b"][1] + \[Delta]\[CapitalSigma]["A"]["b"][2]) +
			k*(m[1] + m[2])*(\[Delta]\[CapitalSigma]["A"]["a"][1] + \[Delta]\[CapitalSigma]["A"]["a"][2])
		)
	|>;
	
	2*cw*wa (* additional 2 is needed due to hEO definition *)
];


(* Integrand of WB. Integration measure not included. *)
WBInt[Y_][k_,\[Omega]_, feq_, \[Delta]\[CapitalSigma]_] := cw*(
	2*Y[1]^2*(
		2*feq[1]*(\[Delta]\[CapitalSigma]["A"]["a"][1] + k/\[Omega][1]*\[Delta]\[CapitalSigma]["A"]["b"][1]) -
		(\[Delta]\[CapitalSigma]["S"]["a"][1] + k/\[Omega][1]*\[Delta]\[CapitalSigma]["S"]["b"][1])
	) +
	2*Y[2]^2*(
		2*feq[2]*(\[Delta]\[CapitalSigma]["A"]["a"][2] + k/\[Omega][2]*\[Delta]\[CapitalSigma]["A"]["b"][2]) -
		(\[Delta]\[CapitalSigma]["S"]["a"][2] + k/\[Omega][2]*\[Delta]\[CapitalSigma]["S"]["b"][2])
	)
);


(* Variant implementing the attractor solution of \[Delta]f12 in the helicity-symmetric decoupling limit. *)
SCPCoeffAttractor[Y_, \[Theta]_][k_, m_, \[Omega]_, \[CapitalSigma]eq_] := Module[
	{c12, \[CapitalGamma]bar12, common, scp = <||>},
	
	c12 = 1/(\[Omega][1]*\[Omega][2]*(-k^2 + m[1]*m[2] + \[Omega][1]*\[Omega][2]));
	\[CapitalGamma]bar12 = cw*(
		Y[1]^2*(\[CapitalSigma]eq["A"]["a"][1] + k/\[Omega][1]*\[CapitalSigma]eq["A"]["b"][1] + cvac*m[1]^2/\[Omega][1]) +
		Y[2]^2*(\[CapitalSigma]eq["A"]["a"][2] + k/\[Omega][2]*\[CapitalSigma]eq["A"]["b"][2] + cvac*m[2]^2/\[Omega][2])
	);
	common = cw^2*c12*Sin[2*\[Theta][1, 2]]*Y[1]^2*Y[2]^2*
		(\[Omega][2] - \[Omega][1])/((\[Omega][2] - \[Omega][1])^2 + (\[CapitalGamma]bar12)^2)*(
			(m[1]*\[Omega][2] + m[2]*\[Omega][1])*(\[CapitalSigma]eq["A"]["a"][1] + \[CapitalSigma]eq["A"]["a"][2]) +
			k*(m[1] + m[2])*(\[CapitalSigma]eq["A"]["b"][1] + \[CapitalSigma]eq["A"]["b"][2]) +
			cvac*(m[1] + m[2])*(-k^2 + m[1]*m[2] + \[Omega][1]*\[Omega][2])
		);
	
	(* only E-part needed *)
	scp["E"] = <|
		"1,1" -> common*(
				(m[1]*\[Omega][2] + m[2]*\[Omega][1])*(\[CapitalSigma]eq["A"]["a"][1]) +
				k*(m[1] + m[2])*(\[CapitalSigma]eq["A"]["b"][1]) +
				cvac*(m[1])*(-k^2 + m[1]*m[2] + \[Omega][1]*\[Omega][2])
			),
		"2,2" -> common*(
				(m[1]*\[Omega][2] + m[2]*\[Omega][1])*(\[CapitalSigma]eq["A"]["a"][2]) +
				k*(m[1] + m[2])*(\[CapitalSigma]eq["A"]["b"][2]) +
				cvac*(m[2])*(-k^2 + m[1]*m[2] + \[Omega][1]*\[Omega][2])
			)
	|>;
	
	scp
];


(* Variant implementing the attractor solution of \[Delta]f12 in the helicity-symmetric decoupling limit. *)
WACoeffAttractor[Y_, \[Theta]_][k_, m_, \[Omega]_, \[Delta]\[CapitalSigma]_] := Module[
	{wa = <||>},
	
	(* Placeholder, this contribution is currently dropped. *)
	wa["E"] = <|
		"1,1" -> 0,
		"2,2" -> 0
	|>;
	
	wa
];


End[];
