build:
	dune build --profile=release

exe: build
	_build/default/bin/main.exe $(ARGS)

watch:
	dune build --profile=release --watch
