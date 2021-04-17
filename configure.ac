AC_INIT([lccc]:[0.1])
AC_CONFIG_SRCDIR([.lccc])

AC_PROG_CXX

AC_PROG_INSTALL


# Get the canonical host and target
# We don't need the build system
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

AC_ARG_ENABLE([libtargets],[
    libtargets=${enableval}
],[libtargets=${target}])

AC_SUBST(libtargets)

AC_ARG_ENABLE([languages],[languages=${enableval}],)


AC_CONFIG_FILES([Makefile])

AC_CONFIG_SUBDIRS([xlang])

AC_OUTPUT