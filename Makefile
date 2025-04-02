RELEASE := toml
DBG := toml.dbg
TESTER := tester

BUILD_FLAGS := 
DBG_FLAGS := -const 'Exn.keepHistory true'

SOURCE := src/*.sml src/*.mlb
TESTS := tests/*.sml tests/*.mlb

all: $(RELEASE) $(DBG) $(TESTER)

.PHONY: test

$(RELEASE): $(SOURCE)
	mlton $(BUILD_FLAGS) -output $@ src/toml.mlb

$(DBG): $(SOURCE)
	mlton $(BUILD_FLAGS) $(DBG_FLAGS) -output $@ src/toml.mlb

$(TESTER): $(SOURCE) $(TESTS)
	mlton $(MLTON_FLAGS) $(DBG_FLAGS) -output $@ tests/toml.test.mlb

test: $(TESTER)
	$(TESTER)

clean:
	rm -f $(BIN)/*

deps:
	jinmori add -r requirements.txt
