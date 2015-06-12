all: bin/words bin/makedict bin/makestem bin/makeefil bin/makeewds bin/makeinfl data

bin/words:
	gprbuild -Pwords

bin/makeinfl:
	gprbuild -Pmakeinfl

bin/makedict:
	gprbuild -Pmakedict

bin/makeefil:
	gprbuild -Pmakeefil

bin/makeewds:
	gprbuild -Pmakeewds

bin/makestem:
	gprbuild -Pmakestem

DICTFILE.GEN: DICTLINE.GEN bin/makedict
	bin/makedict $<

EWDSFILE.GEN:
	bin/makeefil

EWDSLIST.GEN: DICTLINE.GEN bin/makeewds
	bin/makeewds $<

INFLECTS.SEC: INFLECTS.LAT bin/makeinfl
	bin/makeinfl $<

STEMFILE.GEN: STEMLIST.GEN bin/makestem
	bin/makestem $<

data: DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSLIST.GEN INFLECTS.SEC

clean:
	rm -f -- obj/* bin/*
	rm -f -- CHECKEWD. 
	rm -f -- DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSLIST.GEN INFLECTS.SEC
