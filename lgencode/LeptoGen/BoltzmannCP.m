(* ::Package:: *)

(* ::Subsubsection:: *)
(*CP-asymmetry parameters*)


(* The CP-asymmetry parameters for semiclassical Boltzmann equations of RL.
   Self-energy contribution only. *)

Begin["`BoltzmannCP`"];


\[Epsilon][m_, \[CapitalGamma]_, \[Theta]_, settings_] := Switch[settings@"BoltzmannCPAsymmetry",
	"Mixed", \[Epsilon]MixRegulator[m, \[CapitalGamma], \[Theta]],
	"Difference", \[Epsilon]DiffRegulator[m, \[CapitalGamma], \[Theta]],
	"Sum", \[Epsilon]SumRegulator[m, \[CapitalGamma], \[Theta]],
	"EffectiveSum", \[Epsilon]EffSumRegulator[m, \[CapitalGamma], \[Theta]]
];


(* From [arXiv:hep-ph/9707235] and [arXiv:hep-ph/0309342]. *)
\[Epsilon]MixRegulator[m_, \[CapitalGamma]_, \[Theta]_] := Module[{\[Epsilon]},
	\[Epsilon][1] = Sin[2*\[Theta][1,2]]*(m[2]^2 - m[1]^2)*m[1]*\[CapitalGamma][2]/(
		(m[2]^2 - m[1]^2)^2 + (m[1]*\[CapitalGamma][2])^2
	);
	\[Epsilon][2] = Sin[2*\[Theta][1,2]]*(m[2]^2 - m[1]^2)*m[2]*\[CapitalGamma][1]/(
		(m[2]^2 - m[1]^2)^2 + (m[2]*\[CapitalGamma][1])^2
	);
	\[Epsilon]
];

(* From [arXiv:hep-ph/9710460] and [arXiv:hep-ph/0511248]. *)
\[Epsilon]DiffRegulator[m_, \[CapitalGamma]_, \[Theta]_] := Module[{\[Epsilon]},
	\[Epsilon][1] = Sin[2*\[Theta][1,2]]*(m[2]^2 - m[1]^2)*m[1]*\[CapitalGamma][2]/(
		(m[2]^2 - m[1]^2)^2 + (m[2]*\[CapitalGamma][2] - m[1]*\[CapitalGamma][1])^2
	);
	\[Epsilon][2] = Sin[2*\[Theta][1,2]]*(m[2]^2 - m[1]^2)*m[2]*\[CapitalGamma][1]/(
		(m[2]^2 - m[1]^2)^2 + (m[2]*\[CapitalGamma][2] - m[1]*\[CapitalGamma][1])^2
	);
	\[Epsilon]
];

(* From [arXiv:1112.6428] and [arXiv:1312.7680]. *)
\[Epsilon]SumRegulator[m_, \[CapitalGamma]_, \[Theta]_] := Module[{\[Epsilon]},
	\[Epsilon][1] = Sin[2*\[Theta][1,2]]*(m[2]^2 - m[1]^2)*m[1]*\[CapitalGamma][2]/(
		(m[2]^2 - m[1]^2)^2 + (m[2]*\[CapitalGamma][2] + m[1]*\[CapitalGamma][1])^2
	);
	\[Epsilon][2] = Sin[2*\[Theta][1,2]]*(m[2]^2 - m[1]^2)*m[2]*\[CapitalGamma][1]/(
		(m[2]^2 - m[1]^2)^2 + (m[2]*\[CapitalGamma][2] + m[1]*\[CapitalGamma][1])^2
	);
	\[Epsilon]
];

(* From [arXiv:1711.02863]. *)
\[Epsilon]EffSumRegulator[m_, \[CapitalGamma]_, \[Theta]_] := Module[{\[Epsilon]},
	\[Epsilon][1] = Sin[2*\[Theta][1,2]]*(m[2]^2 - m[1]^2)*m[1]*\[CapitalGamma][2]/(
		(m[2]^2 - m[1]^2)^2 + (m[2]*\[CapitalGamma][2] + m[1]*\[CapitalGamma][1])^2*Sin[\[Theta][1,2]]^2
	);
	\[Epsilon][2] = Sin[2*\[Theta][1,2]]*(m[2]^2 - m[1]^2)*m[2]*\[CapitalGamma][1]/(
		(m[2]^2 - m[1]^2)^2 + (m[2]*\[CapitalGamma][2] + m[1]*\[CapitalGamma][1])^2*Sin[\[Theta][1,2]]^2
	);
	\[Epsilon]
];


End[];
