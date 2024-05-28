exe: build
	_build/default/bin/main.exe $(ARGS)

build:
	dune build --profile=release

watch:
	dune build --profile=release --watch
