
AC_DEFUN([LCRUST_ENABLE_BUILD_STD],[
    AC_REQUIRE([LCRUST_PROG_RUSTC])
    AC_REQUIRE([LCRUST_RUSTC_VERSION])

    AC_ARG_ENABLE([build-std],[
        build_std=$enableval
    ],[
        build_std=no
    ])
    
    if test x$build_std != xno
    then
        AC_MSG_NOTICE([Checking how to build-std with $RUSTC... ])
        if x$rustc_is_lccc != xno
            then 
                # $RUSTC is lccc, do what we want.
                _buildstd_workspace_path="$($RUSTC -Z autotools-hacks --print build-std-workspace-path)"
                _buildstd_features=""
                _buildstd_env=""
            else
                _rustc_sysroot="$($RUSTC --print sysroot || echo no)"
                if test "$_rustc_sysroot" != "no"
                then 
                    _buildstd_workspace_path="$_rustc_sysroot/lib/rustlib/src/rust"
                    if test -d "$_buildstd_workspace_path"
                    then
                        _buildstd_features="backtrace"
                        _buildstd_env="RUSTC_BOOTSTRAP=1"
                    else
                        _buildstd_output="$($RUSTC --print build-std-info || echo no)"

                    fi
                fi
            fi
    fi

])