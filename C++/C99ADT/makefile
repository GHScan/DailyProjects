.PHONY: all build rebuild rel_build rel_rebuild clean update_pch build_actions clean_actions
all: build

include makefile_var

build_type = debug

CC =gcc -std=c99
CFLAGS += -MMD
CFLAGS += -Wall
CFLAGS += $(foreach i,$(macro_defs),-D $(i))
CFLAGS += $(foreach i,$(include_dirs),-I '$(i)')
CFLAGS += $(foreach i,$(lib_dirs),-L '$(i)')
CFLAGS += $(foreach i,$(lib_files),-l '$(i)')
ifeq ($(build_dll),1)
CFLAGS += -shared -fPIC
endif
ifeq ($(build_type),debug)
CFLAGS += -g
else
CFLAGS += -O3 -DNDEBUG
endif
ifeq ($(do_profiling),1) 
CFLAGS += -pg
endif
ifeq ($(test_coverage),1) 
CFLAGS += --coverage
endif

srcs = $(wildcard *.c)
objs = $(srcs:.c=.o)
all_deps = $(srcs:.c=.d)
exist_deps = $(wildcard *.d)
not_exist_deps = $(filter-out $(exist_deps), $(all_deps))
pch_file =pch.h

build: build_actions update_pch main
rebuild: clean
	$(MAKE) build
rel_build: 
	$(MAKE) build -e build_type=release
rel_rebuild: clean
	$(MAKE) rel_build
clean: clean_actions
	rm -f main $(objs) $(all_deps) $(pch_file).gch gmon.out *.gcno *.gcda *.gcov
update_pch: $(pch_file).gch

main: $(objs)
	$(CC) -o $@ $(objs) $(CFLAGS) 

$(pch_file).gch: $(pch_file)
	$(CC) $(filter-out -MMD,$(CFLAGS)) $<

ifneq ($(exist_deps),)
include $(exist_deps)
endif
ifneq ($(not_exist_deps),)
$(not_exist_deps:.d=.o):%.o:%.c
endif
