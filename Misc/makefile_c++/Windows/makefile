.PHONY: all build rebuild rel_build rel_rebuild clean build_actions clean_actions
all: build
include makefile_var

build_type = debug

cxx = cl
cxx_flags += -nologo -W3 -Wp64 -DWIN32 -D_CONSOLE -EHsc -fp:precise -GR -Zc:wchar_t -Zc:forScope -Gd -TP -analyze
cxx_flags += $(foreach i,$(include_dirs),-I"$(subst ?, ,$(i))")
cxx_flags += $(foreach i,$(macro_defs),-D$(i))
ifeq ($(build_type),debug)
cxx_flags += -D_DEBUG -Od -Gm -RTCsu -MDd -ZI
else
cxx_flags += -DNDEBUG -O2 -GL -MD -Zi
endif

pch_cxx_flags += $(if $(filter-out pch.obj,$@),-Yu,-Yc)pch.h

link = link
link_flags += -NOLOGO -DEBUG -MACHINE:X86 -MANIFEST
link_flags += $(lib_files) kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib
link_flags += $(foreach i,$(lib_dirs),-LIBPATH:"$(subst ?, ,$(i))")
ifeq ($(build_type),debug)
link_flags += -INCREMENTAL
else
link_flags += -INCREMENTAL:NO -OPT:REF -OPT:ICF -LTCG

ifeq ($(profile_guide_optimization_step),1)
link_flags+=-LTCG:PGINSTRUMENT
endif
ifeq ($(profile_guide_optimization_step),2)
link_flags+=-LTCG:PGOPTIMIZE
endif

endif

ifeq ($(build_dll),1)
link_flags += -DLL
manifest_type =2
target_file =main.dll
else
manifest_type =1
target_file =main.exe
endif

ifeq ($(use_cpp0x),1)
gpp_flag = -std=c++0x	
endif

depends_gen_cmd = g++ $(gpp_flag) -MM $< | sed "1 s/^\(.\+\)\.o:/\1.obj:/" > $(patsubst %.cpp,%.d,$<)

temp_file_ext = ilk pdb exp lib manifest idb ib_tag pch
temp_files = $(foreach i,$(temp_file_ext),$(wildcard *.$(i)))

srcs = pch.cpp $(filter-out pch.cpp,$(wildcard *.cpp))
objs = $(srcs:.cpp=.obj)
all_deps = $(srcs:.cpp=.d)
exist_deps = $(wildcard *.d)
not_exist_deps = $(filter-out $(exist_deps),$(all_deps))

build: build_actions $(target_file)
rebuild: clean
	$(MAKE) build
rel_build:
	$(MAKE) build -e build_type=release
rel_rebuild: clean
	$(MAKE) rel_build

clean: clean_actions
	rm -f $(objs) $(all_deps)
	rm -f $(temp_files) main.exe main.dll

$(target_file): $(objs)
	$(link) $(link_flags) -OUT:$(target_file) $^
	mt.exe -nologo -manifest $(target_file).manifest "-outputresource:$(target_file);$(manifest_type)"

ifneq ($(exist_deps),)
include $(exist_deps)
endif
ifneq ($(not_exist_deps),)
$(not_exist_deps:.d=.obj):%.obj:%.cpp
endif

$(objs):
	$(depends_gen_cmd)
	$(cxx) $(cxx_flags) $(pch_cxx_flags) -c $<
