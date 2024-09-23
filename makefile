STACK_GHC = stack ghc --
BUILD_DIR = ./build
OUTPUT_DIR = ./runnable
HS_FILES = $(wildcard lab1/*.hs)

all: lab1

lab1: lab1/main.hs lab1/Lab1.hs
	$(STACK_GHC) $^ -hidir $(BUILD_DIR) -odir $(BUILD_DIR) -o $(OUTPUT_DIR)/$@

clean:
	rmdir -r $(BUILD_DIR)

check-format:
	@echo "Checking formatting in labs directory..."
	@for %%f in ($(HS_FILES)) do ( \
		echo Checking %%f... && \
		ormolu --mode inplace %%f || exit 1 \
	)

.PHONY: all clean check-format
