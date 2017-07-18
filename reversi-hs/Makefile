TARGETS=reversi-serv
TARGETC=reversi
SRCS=Play.hs Command.hs

all: $(TARGETS) $(TARGETC)

$(TARGETS): Server.hs $(SRCS)
	ghc -O -XFlexibleContexts --make -o $(TARGETS) Server.hs 

$(TARGETC): Client.hs $(SRCS)
	ghc -O -XFlexibleContexts --make -o $(TARGETC) Client.hs 


clean: 
	rm -f *.o
	rm -f *.hi
	rm -f $(TARGETS)
	rm -f $(TARGETS).exe
	rm -f $(TARGETC)
	rm -f $(TARGETC).exe
