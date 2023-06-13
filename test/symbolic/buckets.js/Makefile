CC=js2cesl

ES6=../../../ECMA-SL/share/interpreter/es6.cesl
BUILD_DIR=_build

SOURCES=$(wildcard ./**/*.js)
TESTS=$(addprefix $(BUILD_DIR)/,$(SOURCES:.js=.cesl))

.PHONY: clean all

default: all

all: $(TESTS)

$(BUILD_DIR)/%.cesl: %.js
	@mkdir -p $(dir $@)
	@echo "$(CC) -interp $(ES6) -o $@ $<"; $(CC) -interp $(ES6) -o $@ $<

clean:
	@echo "$(RM) -r $(BUILD_DIR)"; $(RM) -r $(BUILD_DIR)
