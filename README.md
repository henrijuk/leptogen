LeptoGen
========

LeptoGen is a software package for numerically solving (resonant) leptogenesis equations including
full flavour coherence information for the Majorana neutrinos. The package is written in Wolfram
Language and can be run in the Mathematica front end. (Tested in Mathematica 12.0)

Installation
------------

The package can be loaded by running the code

	SetDirectory@NotebookDirectory[];
	Get["lgencode/LeptoGen/LeptoGen.wl"];
	Get["lgencode/LeptoGen/LeptoGenTools.wl"];

in a Mathematica notebook (make sure the notebook is saved in the same directory as `lgencode`
and `selfenergycache`).

Introduction and examples
-------------------------

Run the file `generate_notebooks.m` in the Mathematica front end. This generates the Mathematica
notebook files

	lgen_example.nb
	lgen_introduction.nb

which contain examples on how to use the package. (You can also use the corresponding m-files
`lgen_example.m` and `lgen_introduction.m` but you can't save any generated results or plots to
them.)

Referencing
-----------

If you use this package, please cite the article "Flavour mixing transport theory and resonant
leptogenesis", H. Jukkala, K. Kainulainen, P. M. Rahkila, (2021),
[arXiv:2104.03998 [hep-ph]](https://arxiv.org/abs/2104.03998).

The data used in the article can be reproduced by running the code `export_article_data.m`.
