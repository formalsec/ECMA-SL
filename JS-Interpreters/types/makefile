ROOT_DIR          := ../../../
TOOL_ECMA_COMP    :=      $(ROOT_DIR)implementation/main.native
TOOL_JS2ECMA_COMP := node $(ROOT_DIR)JS2ECMA-SL/src/index.js

DIR_INT       := interpreter/
DIR_BIN       := bin/
DIR_TESTS     := tests/
DIR_TESTS_IN  := testsuit/tests/
DIR_ASM       := asm/

SRC_INT_ENTRY := $(DIR_INT)EntryPoint.esl
SRC_INT  	  := $(shell find $(DIR_INT)   -type f -name "*.esl")
SRC_TESTS 	  := $(shell find $(DIR_TESTS_IN) -type f -name "*.js")

BIN_INT   := $(DIR_BIN)TypesInterpreter.cesl
BIN_TESTS := $(patsubst $(DIR_TESTS_IN)%.js, $(DIR_BIN)$(DIR_TESTS)%.cesl,   $(SRC_TESTS))
BIN_ASM   := $(patsubst $(DIR_TESTS_IN)%.js, $(DIR_BIN)$(DIR_ASM)%.asm.cesl, $(SRC_TESTS))



.PHONY: default

default: $(BIN_INT) $(BIN_TESTS) $(BIN_ASM)

clean:
	@ rm -rf $(DIR_BIN)



$(BIN_INT): $(SRC_INT_ENTRY) $(SRC_INT)
	@ mkdir -p $(DIR_BIN)
	@ $(TOOL_ECMA_COMP) -mode c -i $< -o $@

$(DIR_BIN)$(DIR_TESTS)%.cesl: $(DIR_TESTS_IN)%.js
	@ mkdir -p $(shell dirname $@)
	@ $(TOOL_JS2ECMA_COMP) -i $< -o $@

$(DIR_BIN)$(DIR_ASM)%.asm.cesl: $(DIR_BIN)$(DIR_TESTS)%.cesl $(BIN_INT)
	@ mkdir -p $(shell dirname $@)
	@ cp $< $@
	@ echo ";" >> $@
	@ cat $(BIN_INT) >> $@