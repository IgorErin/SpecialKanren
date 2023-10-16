all:
	dune clean 
	dune build 
	./_build/default/bin/main.exe -var l -par k ./samples/false_and_true.ml
	