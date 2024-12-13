STACK_GHC = stack ghc --
BUILD_DIR = ./build
OUTPUT_DIR = ./runnable
HS_FILES = $(wildcard lab*/*.hs)

all: lab1 lab2

lab1: lab1/main.hs lab1/Fibonacci.hs lab1/Powers.hs
	$(STACK_GHC) $^ -hidir $(BUILD_DIR) -odir $(BUILD_DIR) -o $(OUTPUT_DIR)/$@

lab2: lab2/main.hs lab2/AvlBag.hs lab2/UnitTest.hs lab2/PropertyBasedTest.hs
	$(STACK_GHC) $^ -hidir $(BUILD_DIR) -odir $(BUILD_DIR) -o $(OUTPUT_DIR)/$@

lab3: lab3/main.hs lab3/Interpolation.hs lab3/StreamProcessing.hs lab3/CommandLineParser.hs
	$(STACK_GHC) $^ -hidir $(BUILD_DIR) -odir $(BUILD_DIR) -o $(OUTPUT_DIR)/$@

lab3-test: lab3/Spec.hs lab3/Interpolation.hs lab3/StreamProcessing.hs lab3/CommandLineParser.hs
	$(STACK_GHC) $^ -hidir $(BUILD_DIR) -odir $(BUILD_DIR) -o $(OUTPUT_DIR)/$@

clean:
	rm -rf $(BUILD_DIR)
	rm -rf $(OUTPUT_DIR)

format:
	@echo "Checking formatting in labs directory..."
	@for %%f in ($(HS_FILES)) do ( \
		echo Checking %%f... && \
		ormolu --mode inplace %%f || exit 1 \
	)

.PHONY: all clean format lab2 lab1
