BUILD := bin
RELEASE := $(BUILD)/toml
DBG := $(BUILD)/toml.dbg
TESTER := $(BUILD)/tester

COMMON_FLAGS := -default-ann 'allowOrPats true'
DBG_FLAGS := -const 'Exn.keepHistory true'

SOURCE := src/*.sml src/*.mlb
TESTS := tests/*.sml tests/*.mlb

all: $(RELEASE) $(DBG) $(TESTER)

.PHONY: test

$(RELEASE): $(SOURCE)
	mlton $(COMMON_FLAGS) -output $@ src/toml.mlb

$(DBG): $(SOURCE)
	mlton $(COMMON_FLAGS) $(DBG_FLAGS) -output $@ src/toml.mlb

$(TESTER): $(SOURCE) $(TESTS)
	mlton $(COMMON_FLAGS) $(DBG_FLAGS) -output $@ tests/toml.test.mlb

test: $(TESTER)
	$(TESTER)

clean:
	rm -f $(BUILD)/*

deps:
	jinmori add -r requirements.txt
