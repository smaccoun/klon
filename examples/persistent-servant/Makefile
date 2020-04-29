all: build

br: build run

show-migration:
	stack exec persistent-servant

build:
	stack build --fast

run:
	stack exec persistent-servant

install:
	stack install --fast
