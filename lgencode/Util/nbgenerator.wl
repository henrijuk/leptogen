(* ::Package:: *)

(* A simple package for automatic notebook generation from basic Mathematica/WL code files. *)
(* Mathematica Version: 12.0 *)
(* Author: Henri Jukkala *)
(* For copyright and license information see included LICENSE.txt *)

(* Last changed 20. Apr 2021 *)

BeginPackage["nbgenerator`"];

Unprotect["nbgenerator`*"];
ClearAll["nbgenerator`*"];

(* Exported symbols. *)
Create::usage = "Creates a notebook from a Mathematica code (.m or .wl) file .";

Begin["`Private`"];

(* The core notebook generation function. *)
Create[inputFilename_String, notebookFilename_String] := Module[
	{input, nb},
	(* This "just works" for a basic Mathematica m-file. *)
	input = Import[inputFilename, "String"] //
		StringDelete[pkgstring] //
		StringTrim //
		StringReplace["\r\n" -> "\n"] // (* change Windows-style EOLs to unix-style *)
		StringSplit[#, "\n\n\n"] &; (* assuming two blank lines of cell separation *)
	nb = CreateDocument[cellchooser /@ input];
	NotebookSave[nb, NotebookDirectory[]<>notebookFilename]
];

(* Function for choosing an appropriate cell for the input string which is assumed
   to contain a single cell worth of code or text. Defaults to the code cell. *)
(* NOTE: only supports Section, Subsection, Subsubsection, Text and Code cells
   for the input file at the moment. Implementing the rest would be trivial. *)
cellchooser[str_String] := Which[
	StringContainsQ[str, secstring],
	sectioncell[str // StringDelete[secstring | "(*" | "*)"] // StringTrim],
	StringContainsQ[str, subsecstring],
	subsectioncell[str // StringDelete[subsecstring | "(*" | "*)"] // StringTrim],
	StringContainsQ[str, subsubsecstring],
	subsubsectioncell[str // StringDelete[subsubsecstring | "(*" | "*)"] // StringTrim],
	StringContainsQ[str, txtstring],
	textcell[str // StringDelete[txtstring | "(*" | "*)"] // StringTrim],
	True,
	codecell[str]
];

(* Functions for creating different types of Cells. Hardcoded formatting included. *)
sectioncell[str_String] := TextCell[str,
	"Section", FontSize -> 20, FontColor -> fontcolor, FontWeight -> "Plain"
];
subsectioncell[str_String] := TextCell[str,
	"Subsection", FontSize -> 18, FontColor -> fontcolor, FontWeight -> "Plain"
];
subsubsectioncell[str_String] := TextCell[str,
	"Subsubsection", FontSize -> 16, FontColor -> fontcolor, FontWeight -> "Plain"
];
textcell[str_String] := TextCell[str,
	"Text", FontSize -> 14, Background -> Blend[{fontcolor, White}, 0.9]
];
codecell[str_String] := Cell[BoxData@RowBox@{str},
	"Code", FontSize -> 12, 
	InitializationCell -> False,
	Background -> codebgcolor
];
fontcolor = RGBColor[0.1, 0.5, 0.6];
codebgcolor = RGBColor[0.97, 0.97, 0.97];

(* Special strings. *)
pkgstring = "(*"<>" ::Package:: "<>"*)";
secstring = "(*"<>" ::Section:: "<>"*)";
subsecstring = "(*"<>" ::Subsection:: "<>"*)";
subsubsecstring = "(*"<>" ::Subsubsection:: "<>"*)";
txtstring = "(*"<>" ::Text:: "<>"*)";

Protect["nbgenerator`*"];
End[]; (* `Private` *)
Block[{$ContextPath}, EndPackage[]];
