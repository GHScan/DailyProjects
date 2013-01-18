define myFunc2
$(if $(strip $(2)), $(foreach i,$(2),$(call myFunc, $(i))), $(1))
endef
define myFunc
$(call myFunc2, $(1), $(wildcard $(1)/*))
endef
ALLFILES = $(call myFunc, .)
run:
	printf "%s\n" $(ALLFILES)
