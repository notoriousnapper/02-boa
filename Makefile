######################################################

COURSE=cs131e
ASGN=02
COMPILER=boa
EXT=boa
GROUP=group.txt

######################################################

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

.PHONY: test bin build clean distclean turnin group-not-empty \
	$(ASMS) $(OBJS) $(RUNS) $(RESULTS)

test: clean
	stack test

bin:
	stack install

build:
	stack build

tests/output/%.result: tests/output/%.run
	$< > $@

tests/output/%.run: tests/output/%.o c-bits/main.c
	clang -g -m32 -o $@ c-bits/main.c $<

tests/output/%.o: tests/output/%.s
	nasm -f $(FORMAT) -o $@ $<

tests/output/%.s: tests/input/%.$(EXT) build
	stack exec -- $(COMPILER) $< > $@

clean:
	rm -rf tests/output/*.o tests/output/*.s tests/output/*.dSYM tests/output/*.run tests/output/*.log tests/output/*.result $(ASGN)-$(COMPILER).tgz

distclean: clean
	stack clean
	rm -rf .stack-work

tags:
	hasktags -x -c lib/

group-not-empty:
	test "$$( find $(GROUP) -size +0c )"

turnin: clean $(GROUP) group-not-empty
	./files_to_submit.sh | tar -zcvf ../$(ASGN)-$(COMPILER).tgz -T -
	mv ../$(ASGN)-$(COMPILER).tgz .
	turnin -c $(COURSE) -p $(ASGN) ./$(ASGN)-$(COMPILER).tgz

# aliases

INPUTS  := $(patsubst tests/input/%.boa,%,$(wildcard tests/input/*.boa))
ASMS    := $(patsubst %,%-s,$(INPUTS))
OBJS    := $(patsubst %,%-o,$(INPUTS))
RUNS    := $(patsubst %,%-run,$(INPUTS))
RESULTS := $(patsubst %,%-result,$(INPUTS))

$(ASMS): %-s: tests/output/%.s
	cat $<
$(OBJS): %-o: tests/output/%.o
$(RUNS): %-run: tests/output/%.run
$(RESULTS): %-result: tests/output/%.result
	cat $<
