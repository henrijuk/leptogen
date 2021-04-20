(* ::Package:: *)

(* ::Subsubsection:: *)
(*Component definitions for Main QKE*)


Begin["`Components`"];


(* Converts flavor components (fAd, Coll,...) to the vector form of the neutrino equation.
   The vector function is also used with SCP and W1, and the matrix function with the Hamiltonian. *)
fVector = {#"1,1", #"2,2", Re[#"1,2"], Im[#"1,2"]} &;
collMatrix = {
	{2*Re[#"1,1"], 0, 2*Re[#"1,2"],  2*Im[#"1,2"]},
	{0, 2*Re[#"2,2"], 2*Re[#"2,1"], -2*Im[#"2,1"]},
	{ Re[#"2,1"], Re[#"1,2"], Re[#"1,1" + #"2,2"], -Im[#"1,1" - #"2,2"]},
	{-Im[#"2,1"], Im[#"1,2"], Im[#"1,1" - #"2,2"],  Re[#"1,1" + #"2,2"]}
} &;

(* Variant with no off-diagonal backreaction to diagonal components. *)
collMatrixNoBR = {
	{2*Re[#"1,1"], 0, 0, 0},
	{0, 2*Re[#"2,2"], 0, 0},
	{ Re[#"2,1"], Re[#"1,2"], Re[#"1,1" + #"2,2"], -Im[#"1,1" - #"2,2"]},
	{-Im[#"2,1"], Im[#"1,2"], Im[#"1,1" - #"2,2"],  Re[#"1,1" + #"2,2"]}
} &;

(* Variant with only flavour diagonals. *)
fVectorDiag = {#"1,1", #"2,2"} &;
collMatrixDiag = {
	{2*Re[#"1,1"], 0},
	{0, 2*Re[#"2,2"]}
} &;

(* Joins the helicity-even and odd parts to vectors or matrices. *)
hEOVector = Join[#E, #O] &;
hEOMatrix = ArrayFlatten@{{#E, #O}, {#O, #E}} &;

(* Variant for helicity-even part only. *)
hEvenOnly = #E &;


(* Functions for making the final "vector/matrix form" arrays, with scaling for the components. *)
vectorScale[scaling_] := Map[scaling*# &, #, {1}] &;
matrixScale[scaling_] := Map[scaling*# &, #, {2}] &;

vmfVector[s_] := hEOVector[vectorScale[s]@*fVector /@ #] &;
vmfMatrix[s_] := hEOMatrix[matrixScale[s]@*collMatrix /@ #] &;

(* Variant with only the helicity-even part. *)
vmfVector2[s_] := hEvenOnly[vectorScale[s]@*fVector /@ #] &;
vmfMatrix2[s_] := hEvenOnly[matrixScale[s]@*collMatrix /@ #] &;

(* Variant with only the helicity-even part and no off-diagonal backreaction. *)
vmfVector3[s_] := hEvenOnly[vectorScale[s]@*fVector /@ #] &;
vmfMatrix3[s_] := hEvenOnly[matrixScale[s]@*collMatrixNoBR /@ #] &;

(* Variant with the helicity-even part and flavour diagonals only (for the attractor source). *)
vmfVector4[s_] := hEvenOnly[vectorScale[s]@*fVectorDiag /@ #] &;
vmfMatrix4[s_] := hEvenOnly[matrixScale[s]@*collMatrixDiag /@ #] &;


End[];
