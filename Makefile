#---------------------------------------------------------------
#             CONFIGURE THESE VARIABLES IF NEEDED
#---------------------------------------------------------------

ROOT = ${HOME}/compiladores/root
CDK_INC_DIR = $(ROOT)/usr/include
CDK_LIB_DIR = $(ROOT)/usr/lib
CDK_BIN_DIR = $(ROOT)/usr/bin

LANGUAGE=til

#---------------------------------------------------------------
# PROBABLY, THERE'S NO NEED TO CHANGE ANYTHING BEYOND THIS POINT
#---------------------------------------------------------------

L_NAME=$(LANGUAGE)_scanner
Y_NAME=$(LANGUAGE)_parser

LFLAGS   = 
YFLAGS   = -dtv --debug
CXXFLAGS = -std=c++20 -pedantic -Wall -Wextra -ggdb -I. -I$(CDK_INC_DIR) -Wno-unused-parameter -msse2 -mfpmath=sse
#CXXFLAGS = -std=c++20 -DYYDEBUG=1 -pedantic -Wall -Wextra -ggdb -I. -I$(CDK_INC_DIR) -Wno-unused-parameter
LDFLAGS  = -L$(CDK_LIB_DIR) -lcdk #-lLLVM
COMPILER = $(LANGUAGE)

CDK  = $(CDK_BIN_DIR)/cdk
LEX  = flex
YACC = bison

SRC_CPP = $(shell find ast -name \*.cpp) $(wildcard targets/*.cpp) $(wildcard ./*.cpp)
OFILES  = $(SRC_CPP:%.cpp=%.o)

#---------------------------------------------------------------
#                DO NOT CHANGE AFTER THIS LINE
#---------------------------------------------------------------

all: .auto/all_nodes.h .auto/visitor_decls.h $(COMPILER)

%.tab.o:: %.tab.c
	$(CXX) $(CXXFLAGS) -c $< -o $@ -Wno-class-memaccess

%.o:: %.c
	$(CXX) $(CXXFLAGS) -c $< -o $@

%.o:: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

%.tab.c:: %.y
	$(YACC) $(YFLAGS) -b $* $<

%.tab.h:: %.y
	$(YACC) $(YFLAGS) -b $* $<

$(L_NAME).cpp: $(L_NAME).l
	$(LEX) $(LFLAGS) $<

$(Y_NAME).tab.c: $(Y_NAME).y
$(Y_NAME).tab.h: $(Y_NAME).y

# this is needed to force byacc to run
$(L_NAME).o: $(L_NAME).cpp $(Y_NAME).tab.h

.auto/all_nodes.h: 
	mkdir -p .auto
	$(CDK) ast --decls ast --language $(LANGUAGE) > $@

.auto/visitor_decls.h: 
	mkdir -p .auto
	$(CDK) ast --decls target --language $(LANGUAGE) > $@

$(COMPILER): $(L_NAME).o $(Y_NAME).tab.o $(OFILES)
	$(CXX) -o $@ $^ $(LDFLAGS)

clean:
	$(RM) .auto/all_nodes.h .auto/visitor_decls.h *.tab.[ch] *.o $(OFILES) $(L_NAME).cpp $(Y_NAME).output $(COMPILER)
	$(RM) [A-Z]*-ok.* [A-Z]*-ok

depend: .auto/all_nodes.h
	$(CXX) $(CXXFLAGS) -MM $(SRC_CPP) > .makedeps

-include .makedeps

#---------------------------------------------------------------
#                           THE END
#---------------------------------------------------------------
