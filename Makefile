%.cpp.out: %.cpp
	g++ -Wall -o $@ $<

%.hs.out: %.hs
	stack ghc $< -- -o $@
