#ifndef LCRUST_LANGITEM_H_2021_05_25_18_04_33
#define LCRUST_LANGITEM_H_2021_05_25_18_04_33

namespace lccc::lcrust
{
    enum class LangItem : std::uint64_t {
        Never = 0,
        Sized = 1,
        PhantomData = 2,
        Unpin = 3,
        Copy = 4,
        StructuralTeq2018 = 5,
        StructuralPeq2018 = 6,
        Freeze = 7,
        TrivialDestruction = 8,
        NonOwning = 9,
        TraitObjectMarker = 10,
        VaListDrop = 11,
        Bool = 12,
        DropInPlace = 13,
        ConstPtr = 14,
        MutPtr = 15,
        Slice = 16,
        Eq = 17,
        PartialOrd = 18,
        IteratorNext = 19,
        Pin = 20,
        PinNewUnchecked = 21,
        Unit = 22,
        Clone = 23,
        Some = 24,
        None = 25,
        Not = 26,
        Deref = 27,
        DerefMut = 28,
        Add = 29,
        Sub = 30,
        Mul = 31,
        Div = 32,
        Reciever = 33,
        Unsize = 34,
        CoerceUnsized = 35,
        UnwrappingDeref = 36,
        PlacementDeref = 37,
        PureAnnotation = 38,
        Drop = 39,
        TypeDestructor = 40,
        FnOnce = 41,
        FnMut = 42,
        Fn = 43,
        Index = 44,
        IndexMut = 45,
        Range = 46,
        ControlFlow = 47,
        Try2018 = 48,
        Try2021 = 49,
        AllocLayout = 50,
        GlobalAlloc = 51,
        Try2018Break = 52,
        Try2018Continue = 53,
        IntoIter = 54,
        TokenStream = 55,
    };
}

#endif /*LCRUST_LANGITEM_H_2021_05_25_18_04_33*/