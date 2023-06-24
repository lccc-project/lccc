
AC_DEFUN([LCRUST_CHECK_EDITION],[
    SAVE_RUSTFLAGS="$RUSTFLAGS"
    RUSTFLAGS="$RUSTFLAGS --edition $1"
    LCRUST_TRY_COMPILE([],[
        rust_edition=$1
    ],
        RUSTFLAGS="$SAVE_RUSTFLAGS"
        rust_edition=no
        $2
    )
])

AC_DEFUN([LCRUST_TRY_COMPILE_WITH_FEATURES],[
    LCRUST_TRY_COMPILE([$1],
        $3
    ,
        [for feature in $2 do
        do
            LCRUST_TRY_COMPILE([#![feature($feature)] $1],[
                try_compile_result=$feature
                feature_$feature=yes
                RUSTFLAGS="$RUSTFLAGS --cfg feature_$feature"
                $3
            ])
            if test x$try_compile_result != xno
            then
                break
            fi
        done
        if test x$try_compile_result \= xno 
        then
            $4
        fi
        ]
    )
])

AC_DEFUN([LCRUST_CHECK_TOOL],[
    LCRUST_TRY_COMPILE([#![$1 :: $2]],[
        tool_$1=yes
        $4
    ],[
        LCRUST_TRY_COMPILE([#![register_tool($1)] $![$1 :: $2]],[
            tool_$1=register
        ])
    ])
])