[+ AutoGen5 template -*- Mode: Makefile -*-
in
+]
[+ dne -D # +]

# Makefile.in for lccc, toplevel



## Variables
prefix = @prefix@
exec_prefix = @exec_prefix@
bindir = @bindir@
libdir = @libdir@
includedir = @includedir@
sbindir = @sbindir@
libexecdir = @libexecdir@
datarootdir = @datarootdir@
datadir = @datadir@
mandir = @mandir@
docdir = @docdir@
infodir = @infodir@
localedir = @localedir@
sysconfdir = @sysconfdir@
localstatedir = @localstatedir@
runstatedir = @runstatedir@
sharedstatedir = @sharedstatedir@
lcccdir = @lcccdir@
xlangplugindir = @xlangplugindir@

builddir = @builddir@
abs_builddir = @abs_builddir@

target = @target@
host = @host@
build = @build@


CC = @CC@
RUSTC = @RUSTC@
CC_FOR_BUILD = @CC_FOR_BUILD@
RUSTC_FOR_BUILD = @RUSTC_FOR_BUILD@

CFLAGS = @CFLAGS@
RUSTFLAGS = @RUSTFLAGS@
CFLAGS_FOR_BUILD = @CFLAGS_FOR_BUILD@
RUSTFLAGS_FOR_BUILD = @RUSTFLAGS_FOR_BUILD@

INSTALL = @INSTALL@
INSTALL_SCRIPT = @INSTALL_SCRIPT@
INSTALL_PROGRAM = @INSTALL_PROGRAM@
MKDIR_P = @MKDIR_P@


## Bootstrapping Variables

lccc_stage = @lccc_stage@

stage0_target = @rustc_build_target@
stage1_target = @stage1_target@
stage2_targets = @stage2_targets@


### Targets to be built during each stage
stage0_goals = @lccc_stage0_targets@
stage1_goals = @lccc_stage1_targets@
stage2_goals = @lccc_stage2_targets@

stage0_RUSTC = $(RUSTC_FOR_BUILD)
stage0_CC = $(CC_FOR_BUILD)
stage0_CFLAGS = $(CFLAGS_FOR_BUILD)
stage0_RUSTFLAGS = $(RUSTFLAGS_FOR_BUILD)


stage1_RUSTC = @stage1_RUSTC@
stage1_CC = @stage1_CC@
stage1_CFLAGS = @stage1_CFLAGS@
stage1_RUSTFLAGS = @stage1_RUSTFLAGS@

stage1_RUSTC_FOR_BUILD = @stage1_RUSTC_FOR_BUILD@
stage1_CC_FOR_BUILD = @stage1_CC_FOR_BUILD@
stage1_CFLAGS_FOR_BUILD = @stage1_CFLAGS_FOR_BUILD@
stage1_RUSTFLAGS_FOR_BUILD = @stage1_RUSTFLAGS_FOR_BUILD@

stage2_RUSTC = @stage2_RUSTC@
stage2_CC = @stage2_CC@
stage2_CFLAGS = @stage2_CFLAGS@
stage2_RUSTFLAGS = @stage2_RUSTFLAGS@


##
# Do not Edit past this point

.DEFAULT_GOAL = all

stage0_builddir = ${builddir}/build
stage0_abs_builddir = ${abs_builddir}/build

stage1_builddir = ${builddir}/
stage1_abs_builddir = ${abs_builddir}/

vendor_STEM = vendor
vendor_SRCDIR = ${srcdir}/vendor
vendor_STAGE0_GOAL = all
vendor_STAGE1_GOAL = all
vendor_STAGE2_GOAL = 
vendor_DEPS = 

steps = vendor @steps@

[+ foreach main +]

[+name+]_STEM = [+subdir+]
[+name+]_SRCDIR = ${srcdir}/[+subdir+]
[+name+]_BUILDDIR = ${abs_builddir}/[+subdir+]
[+name+]_STAGE0_BUILDDIR = ${stage0_builddir}/vendor
[+name+]_STAGE0_GOAL = all
[+name+]_STAGE1_GOAL = all
[+name+]_STAGE2_GOAL = 
[+name+]_DEPS = vendor [+deps+]

[+ endfor +]

[+ foreach codegen +]

[+name+]_STEM = [+subdir+]
[+name+]_SRCDIR = ${srcdir}/[+subdir+]
[+name+]_BUILDDIR = ${abs_builddir}/[+subdir+]
[+name+]_STAGE0_BUILDDIR = ${stage0_builddir}/vendor
[+name+]_STAGE0_GOAL = all
[+name+]_STAGE1_GOAL = all
[+name+]_STAGE2_GOAL = 
[+name+]_DEPS = vendor [+deps+]

[+ endfor +]

[+ foreach frontend +]

[+name+]_STEM = [+subdir+]
[+name+]_STAGE0_GOAL = all
[+name+]_STAGE1_GOAL = @stage1_libs@ all
[+name+]_STAGE2_GOAL = libraries
[+name+]_DEPS = vendor [+deps+]

[+ endfor +]

COMMON_EXPORTS = export INSTALL=$(INSTALL)

STAGE0_EXPORTS = $(COMMON_EXPORTS); export CC=$(stage0_CC); export CFLAGS=$(stage0_CFLAGS); export RUSTC=$(stage0_RUSTC); export RUSTFLAGS=$(stage0_RUSTFLAGS); \
    export CC_FOR_BUILD=$(stage0_CC); export CFLAGS_FOR_BUILD=$(stage0_CFLAGS); export RUSTC_FOR_BUILD=$(stage0_RUSTC); export RUSTFLAGS_FOR_BUILD=$(stage0_RUSTFLAGS)

STAGE0_CONFIGURE = --prefix=$(prefix) --exec-prefix=$(exec_prefix) --bindir=$(bindir) --sbindir=$(sbindir) --libdir=$(libdir) --libexecdir=$(libexecdir) 

[+ define gen_goals +]

.PHONY: [+name+]_[+builddir+]-configure [+name+]_[+builddir+]/all [+name+]_[+builddir+]/install [+name+]_[+builddir+]/install-strip [+name+]_[+builddir+]/clean [+name+]_[+builddir+]/libraries

[+name+]_[+builddir+]-configure: [+srcdir+]/configure $(foreach dep,[+deps], $($(dep)_DEPS)_[+builddir+]/stamp)
    @+$(MKDIR_P) $([+name+]_[+builddir+]); cd [+builddir+]; [+exports+]; [+extra_exports+]; ../[+srcdir+]/configure [+configure+] [+extra_configure+]

$([+name+]_[+builddir+])/: 
    @+[+exports+]; [+extra_exports+]; $(MAKE) [+name+]_[+builddir+]-configure

$([+name+]_[+builddir+])/%: $([+name+]_[+builddir+])/ $(foreach dep,[+deps], $($(dep)_DEPS)_[+builddir+]/stamp)
    @+[+exports+]; [+extra_exports+]; $(MAKE) -C [+name+]_[+builddir+] $*


[+ enddef +]

[+ foreach main +]

[+ endfor +]