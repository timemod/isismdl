include ../../../tools/makesetincf
include ../../../tools/$(INCLUDE_FILE)

ifndef ISIS
   ISIS = ..$(ds)..$(ds)..$(ds)opt_obj$(ds)$(EXE)
endif

# silent rule to prevent echoing
.SILENT:

# list of Isis-jobs
JOBS := $(basename $(notdir $(wildcard *.isi)))
OUT :=  $(addprefix out/, $(addsuffix .out,$(JOBS)))

out/%.out : %.isi
	echo Running job $<
	-$(ISIS) i=$(<F) logf=log/$*.log o=out/$*.out dbgf=dbg/$*.dbg

all: prerun jobs 

prerun:
	-$(DEL) out$(ds)*.out   log$(ds)*.log $(DEVNUL)

jobs: $(OUT)
