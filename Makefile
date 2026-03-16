# Paris Metro Route Finder - Makefile
# Copyright (C) 2026 By Ulrik Hørlyk Hjort

# Compiler and flags
GNATMAKE = gnatmake
GNATCLEAN = gnatclean
GNAT_FLAGS = -gnat12 -gnata -gnatwa
GNAT_OPT = -O2

# Directories
SRC_DIR = src
BIN_DIR = bin
DATA_DIR = data
TEST_DIR = tests
OBJ_DIR = obj

# Main programs
MAIN = metro_main
TEST = metro_test

# Targets
.PHONY: all clean test install help

all: $(BIN_DIR)/$(MAIN)
	@cp -r $(DATA_DIR) $(BIN_DIR)/ 2>/dev/null || true
	@cp -r data_enhanced $(BIN_DIR)/ 2>/dev/null || true

$(BIN_DIR)/$(MAIN): $(SRC_DIR)/*.adb $(SRC_DIR)/*.ads | $(BIN_DIR) $(OBJ_DIR)
	cd $(SRC_DIR) && $(GNATMAKE) $(MAIN).adb $(GNAT_FLAGS) $(GNAT_OPT) \
		-D ../$(OBJ_DIR) -o ../$(BIN_DIR)/$(MAIN)
	@cp -r $(DATA_DIR) $(BIN_DIR)/ 2>/dev/null || true
	@cp -r data_enhanced $(BIN_DIR)/ 2>/dev/null || true

test: $(BIN_DIR)/$(TEST)
	@echo "Running tests..."
	cd $(BIN_DIR) && ./$(TEST)

$(BIN_DIR)/$(TEST): $(TEST_DIR)/$(TEST).adb $(SRC_DIR)/metro_network.ad* | $(BIN_DIR) $(OBJ_DIR)
	cd $(TEST_DIR) && $(GNATMAKE) $(TEST).adb $(GNAT_FLAGS) \
		-I../$(SRC_DIR) -D ../$(OBJ_DIR) -o ../$(BIN_DIR)/$(TEST)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

clean:
	rm -rf $(BIN_DIR)/* $(OBJ_DIR)/*
	cd $(SRC_DIR) && $(GNATCLEAN) -c $(MAIN)
	cd $(TEST_DIR) && $(GNATCLEAN) -c $(TEST) 2>/dev/null || true

install: all
	@echo "Metro Route Finder built successfully"
	@echo "Run: ./$(BIN_DIR)/$(MAIN) [start_station] [end_station]"

help:
	@echo "Paris Metro Route Finder - Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all      - Build the main program (default)"
	@echo "  test     - Build and run tests"
	@echo "  clean    - Remove built files"
	@echo "  install  - Build and show usage"
	@echo "  help     - Show this help"
	@echo ""
	@echo "Usage:"
	@echo "  make"
	@echo "  make test"
	@echo "  ./$(BIN_DIR)/$(MAIN) \"Concorde\" \"Bastille\""
