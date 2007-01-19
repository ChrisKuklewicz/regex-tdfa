.PHONY: all configure build install setup clean

CONF=--prefix=/User/chrisk/local/devel/trl --enable-library-profiling
USER=--user

all: setup clean configure build

setup: Setup.hs
	ghc -o setup --make ./Setup.hs

clean:
	-./setup clean

configure:
	./setup configure $(CONF) $(USER)

build:
	./setup build

doc:
	./setup haddock

install:
	./setup install $(USER)
