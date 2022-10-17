module Internal.IntType exposing (Config)


type alias Config intType =
    { zigZag : intType -> intType
    , zagZig : intType -> intType
    , toUnsigned : intType -> intType
    , fromUnsigned : intType -> intType
    , to7BitList : intType -> List Int
    , from7BitList : List Int -> intType
    }
