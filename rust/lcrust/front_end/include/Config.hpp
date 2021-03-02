#ifndef LCCC_LCRUST_CONFIG_HPP_2021_02_09_14_17_30
#define LCCC_LCRUST_CONFIG_HPP_2021_02_09_14_17_30

#ifdef _WIN32
#if defined(_M_IX86)
#define rustcall __fastcall
#elif defined(__i386__)
#define rustcall __fastcall
#else
#define rustcall __cdecl
#endif
#else
#define rustcall 
#endif
#endif