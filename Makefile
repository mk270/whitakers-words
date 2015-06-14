PROGRAMMES := bin/words bin/makedict bin/makestem bin/makeefil bin/makeewds bin/makeinfl 

all: $(PROGRAMMES) data

programmes: $(PROGRAMMES)

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
	echo g | bin/makedict $< > /dev/null

EWDSFILE.GEN:
	bin/makeefil

EWDSLIST.GEN: DICTLINE.GEN bin/makeewds
	echo g | bin/makeewds $< > /dev/null

INFLECTS.SEC: INFLECTS.LAT bin/makeinfl
	bin/makeinfl $<

STEMFILE.GEN: STEMLIST.GEN bin/makestem
	echo g | bin/makestem $< > /dev/null

data: DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSLIST.GEN INFLECTS.SEC

clean:
	rm -f -- obj/* bin/*
	rm -f -- CHECKEWD. 
	rm -f -- DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSLIST.GEN INFLECTS.SEC
