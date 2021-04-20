(* ::Package:: *)

(* ::Subsubsection:: *)
(*Neutrino equation functions*)


(* Notes:
   - The functions are given in the flavour-component form.
   - "E" and "O" refer to helicity even and odd parts, respectively.
   - All externally given self-energy functions (\[CapitalSigma]eq and \[Delta]\[CapitalSigma]) contain T-dependent parts only.
   - The vacuum part of Sigma^A_eq is hardcoded to avoid loss of precision due to cancellations.
*)

Begin["`Neutrino`"];


cw = 2; (* lepton SU(2)-doublet multiplicity factor *)
cvac = 1/(32*\[Pi]); (* coefficient of Sigma^A_vac *)


(* PLACEHOLDER, not implemented yet. f_ad with corrections from the interactions. *)
fAd["<", Y_, \[Theta]_][k_, m_, \[Omega]_, feq_, Dfeq_, \[CapitalSigma]eq_, D\[CapitalSigma]eq_] := Module[
	{fAd = <||>},
	
	fAd["E"] = <|
		"1,1" -> feq[1],
		"2,2" -> feq[2],
		"1,2" -> 0
	|>;
	
	fAd["O"] = <|
		"1,1" -> 0,
		"2,2" -> 0,
		"1,2" -> 0
	|>;
	
	fAd
];

fAd[">", Y_, \[Theta]_][k_, m_, \[Omega]_, feq_, Dfeq_, \[CapitalSigma]eq_, D\[CapitalSigma]eq_] :=
fAd["<", Y, \[Theta]][k, m, \[Omega], 1 - feq, -Dfeq, \[CapitalSigma]eq, D\[CapitalSigma]eq];


fAdTree["<"][feq_] := <|
	"E" -> <|
		"1,1" -> feq[1],
		"2,2" -> feq[2],
		"1,2" -> 0
	|>,
	"O" -> <|
		"1,1" -> 0,
		"2,2" -> 0,
		"1,2" -> 0
	|>
|>;

fAdTree[">"][feq_] := fAdTree["<"][1 - feq];


DtfAdTree["<"][m_, mdot_, \[Omega]_, Dfeq_] := <|
	"E" -> <|
		"1,1" -> m[1]*mdot[1]/\[Omega][1]*Dfeq[1],
		"2,2" -> m[2]*mdot[2]/\[Omega][2]*Dfeq[2],
		"1,2" -> 0
	|>,
	"O" -> <|
		"1,1" -> 0,
		"2,2" -> 0,
		"1,2" -> 0
	|>
|>;

DtfAdTree[">"][m_, mdot_, \[Omega]_, Dfeq_] := -DtfAdTree["<"][m, mdot, \[Omega], Dfeq];


fAdVac["<"] := <|
	"E" -> <|"1,1" -> 0, "2,2" -> 0, "1,2" -> 0|>,
	"O" -> <|"1,1" -> 0, "2,2" -> 0, "1,2" -> 0|>
|>;

fAdVac[">"] := <|
	"E" -> <|"1,1" -> 1, "2,2" -> 1, "1,2" -> 0|>,
	"O" -> <|"1,1" -> 0, "2,2" -> 0, "1,2" -> 0|>
|>;


Coll[Y_, \[Theta]_][k_, m_, \[Omega]_, feq_, \[CapitalSigma]eq_] := Module[
	{coeff12, Coll = <||>},
	
	coeff12 = 1/Sqrt[2*\[Omega][1]*\[Omega][2]*(-k^2 + m[1]*m[2] + \[Omega][1]*\[Omega][2])];
	
	Coll["E"] = <|
		"1,1" -> cw*Y[1]^2*(
			\[CapitalSigma]eq["A"]["a"][1] + k/\[Omega][1]*\[CapitalSigma]eq["A"]["b"][1] + cvac*m[1]^2/\[Omega][1]
		),
		"2,2" -> cw*Y[2]^2*(
			\[CapitalSigma]eq["A"]["a"][2] + k/\[Omega][2]*\[CapitalSigma]eq["A"]["b"][2] + cvac*m[2]^2/\[Omega][2]
		),
		"1,2" -> cw*Y[1]*Y[2]*Cos[\[Theta][1, 2]]*coeff12*(
			(m[2]*\[Omega][1] + m[1]*\[Omega][2])*\[CapitalSigma]eq["A"]["a"][2] + k*(m[1] + m[2])*\[CapitalSigma]eq["A"]["b"][2] +
			cvac*m[2]*(-k^2 + m[1]*m[2] + \[Omega][1]*\[Omega][2])
		),
		"2,1" -> cw*Y[1]*Y[2]*Cos[\[Theta][1, 2]]*coeff12*(
			(m[2]*\[Omega][1] + m[1]*\[Omega][2])*\[CapitalSigma]eq["A"]["a"][1] + k*(m[1] + m[2])*\[CapitalSigma]eq["A"]["b"][1] +
			cvac*m[1]*(-k^2 + m[1]*m[2] + \[Omega][1]*\[Omega][2])
		)
	|>;
	
	Coll["O"] = <|
		"1,1" -> 0,
		"2,2" -> 0,
		"1,2" -> cw*Y[1]*Y[2]*Sin[\[Theta][1, 2]]*coeff12*(
			-I*( (m[2]*\[Omega][1] + m[1]*\[Omega][2])*\[CapitalSigma]eq["A"]["b"][2] + k*(m[1] + m[2])*\[CapitalSigma]eq["A"]["a"][2] +
			cvac*k*m[2]*(\[Omega][2] - \[Omega][1]) )
		),
		"2,1" -> -cw*Y[1]*Y[2]*Sin[\[Theta][1, 2]]*coeff12*(
			-I*( (m[2]*\[Omega][1] + m[1]*\[Omega][2])*\[CapitalSigma]eq["A"]["b"][1] + k*(m[1] + m[2])*\[CapitalSigma]eq["A"]["a"][1] +
			cvac*k*m[1]*(\[Omega][1] - \[Omega][2]) )
		)
	|>;
	
	Coll
];


Hamilton[\[Omega]_] := <|
	"E" -> <|
		"1,1" -> -I*\[Omega][1],
		"2,2" -> -I*\[Omega][2],
		"1,2" -> 0,
		"2,1" -> 0
	|>,
	"O" -> <|
		"1,1" -> 0,
		"2,2" -> 0,
		"1,2" -> 0,
		"2,1" -> 0
	|>
|>;


End[];
