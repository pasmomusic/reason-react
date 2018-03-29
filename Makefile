all: build

build-deps:
	@esy install

build:
	@esy build

test:
	@esy jbuilder runtest --dev

clean:
	@jbuilder clean

.PHONY: all test build-deps clean
