TARGETS=reversi-serv
TARGETC=reversi
SRCS=Play.hs Command.hs

all: $(TARGETS) $(TARGETC)

$(TARGETS): Server.hs $(SRCS)
	stack -v --install-ghc --resolver ghc-8.0.1 ghc -- Server.hs -O -XFlexibleContexts --make -o $(TARGETS)

$(TARGETC): Client.hs $(SRCS)
	stack -v --install-ghc --resolver ghc-8.0.1 ghc -- Client.hs -O -XFlexibleContexts --make -o $(TARGETC) 


clean: 
	rm -f *.o
	rm -f *.hi
	rm -f $(TARGETS)
	rm -f $(TARGETS).exe
	rm -f $(TARGETC)
	rm -f $(TARGETC).exe
