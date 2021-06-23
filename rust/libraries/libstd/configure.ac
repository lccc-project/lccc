AC_INIT

AC_CONFIG_MACRO_DIRS([m4/])

AC_CANONICAL_HOST

AC_PROG_CC

LCRUST_PROG_RUSTC
LCRUST_RUSTC_VERSION

if test x$rustc_is_lccc != xyes
then
   AC_MSG_ERROR([Cannot compile lcrust libraries except on lccc])
fi 

AC_ARG_WITH([abi-version],[Override the default abi version],[
   case "$enableval" in
      yes )
         abi_version=0
         ;;
      no )
         abi_version=no
         ;;
      * )
         if test "$enableval" -le 0
         then
            abi_version="$enableval"
         else
            AC_MSG_ERROR([Invalid abi-version $enableval. abi-version must be less than or equal to the current version, 0])
         fi
         ;;
],[
   abi_version=0
])

if x$abi_version != xno
then
   RUSTFLAGS="$RUSTFLAGS -Z build-abi=$abi_version --cfg abi_version=\"$abi_version\""
fi 

AC_ARG_ENABLE([randomize-danging],[Randomize the output of NonNull::dangling()],[
   if x$enableval \= xyes
   then 
      RUSTFLAGS="$RUSTFLAGS --cfg feature=\"randomize-dangling\""
   fi
])

AC_ARG_ENABLE([randomize-layout],[Randomize struct layout by passing -Z repr-rust-layout=randomize to rustc],[
   if x$enableval \= xyes
   then
      RUSTFLAGS="$RUSTFLAGS -Z repr-rust-layout=randomize"
   fi
])

AC_TRY_COMPILE([#include <processthreadsapi.h>],[CreateThread(NULL,0,NULL,NULL,0,NULL);],[
   RUSTFLAGS="$RUSTFLAGS --cfg threads=\"win32\""
])


AC_CONFIG_FILES(Makefile)

AC_OUTPUT

