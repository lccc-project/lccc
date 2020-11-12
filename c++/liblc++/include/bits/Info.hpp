#ifndef __LIBLCXX_BITS_INFO_HPP_2020_11_12_06_57_57
#define __LIBLCXX_BITS_INFO_HPP_2020_11_12_06_57_57


#ifdef __cplusplus>=201103L
#define __LCCC_CXX11_CONSTEXPR constexpr
#define __LCCC_CONSTEXPR constexpr
#define __LCCC_CXX11_NOEXCEPT noexcept(true)
#define __LCCC_NOEXCEPT noexcept(true)
#else
#define __LCCC_CXX11_CONSTEXPR
#define __LCCC_CONSTEXPR const
#define __LCCC_CXX11_NOEXCEPT
#define __LCCC_CXX11_NOEXCEPT throw()
#endif

#ifdef __cplusplus>=201402L
#define __LCCC_CXX14_CONSTEXPR constexpr
#define __LCCC_CXX14_NOEXCEPT noexcept(true)
#else
#define __LCCC_CXX14_CONSTEXPR
#define __LCCC_CXX14_NOEXCEPT
#endif

#ifdef __cplusplus>=201703L
#define __LCCC_CXX17_CONSTEXPR constexpr
#define __LCCC_CXX17_INLINE inline
#define __LCCC_CXX17_NOEXCEPT noexcept(true)
#else
#define __LCCC_CXX17_CONSTEXPR
#define __LCCC_CXX17_INLINE
#define __LCCC_CXX17_NOEXCEPT
#endif

#ifdef __cplusplus>=202002L
#define __LCCC_CXX20_CONSTEXPR constexpr
#define __LCCC_CXX20_NOEXCEPT noexcept(true)
#else
#define __LCCC_CXX20_CONSTEXPR
#define __LCCC_CXX20_NOEXCEPT
#endif


#endif /* __LIBLCXX_BITS_INFO_HPP_2020_11_12_06_57_57 */