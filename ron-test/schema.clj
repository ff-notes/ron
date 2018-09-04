(
    (type_alias Char
        RON   String
        view  { C++     (use wchar_t)
                Haskell (use Char)
              }
    )

    (type_alias RgaText
        RON   (RGA Char)
        view  { Haskell (use Text) }
    )

    (struct_lww TestStruct
        RON   { int   SInt64
                text  RgaText
              }
        view  { Haskell create }
    )
)
