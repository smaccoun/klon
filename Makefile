all: install

build:
	stack build --fast

install:
	stack install --fast
