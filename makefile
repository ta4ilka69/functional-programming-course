STACK_GHC = stack ghc --
BUILD_DIR = ./build
OUTPUT_DIR = ./runnable

all: lab1

lab1: lab1/main.hs lab1/Lab1.hs
	$(STACK_GHC) $^ -hidir $(BUILD_DIR) -odir $(BUILD_DIR) -o $(OUTPUT_DIR)/$@

clean:
	rmdir -r $(BUILD_DIR)

.PHONY: all clean
