AC_INIT([lccc]:[0.1])
AC_CONFIG_SRCDIR([.lccc])

AC_PROG_CXX

AC_PROG_INSTALL


# Get the canonical host and target
# We don't need the build system

AC_CANONICAL_HOST
AC_CANONICAL_TARGET

if [ x$target_alias != x ]
then
    LCCC_DEFAULT_TARGET="$target_alias"
else
    LCCC_DEFAULT_TARGET="$target"
fi

if [ x$host_alias != x]
then
    LCCC_HOST_TARGET="$host_alias"
else
    LCCC_HOST_TARGET="$host"
fi



AC_ARG_ENABLE([library-targets],[
    IFS=',' read -ra noncanon-libtargs <<< "$enableval"
    libtargets=""
    for targ in ${noncanon-libtargs}
    do
        libtargets="libtargets `$ac_srcdir/config.sub $target`"
    done
],[libtargets=${target}])


AC_ARG_ENABLE([hosted-targets],[
    IFS=',' read -ra noncanon-hosttargs <<< "$enableval"
    hosttargs=""
    for targ in ${noncanon-hosttargs}
    do
        libtargets="libtargets `$ac_srcdir/config.sub $target`"
    done
],[
    
])

AC_SUBST(libtargets)

AC_ARG_ENABLE([languages],[languages=${enableval}],)


AC_CONFIG_FILES([Makefile])

AC_CONFIG_SUBDIRS([xlang])

AC_OUTPUT