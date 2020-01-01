all: build

build:
	stack build --fast

install:
	stack install --fast
