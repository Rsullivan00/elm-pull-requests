module Utils exposing (..)

import Dict


insertBoth :
  comparable
  -> appendable
  -> appendable
  -> Dict.Dict comparable appendable
  -> Dict.Dict comparable appendable
insertBoth key leftVal rightVal dict =
  Dict.insert key (leftVal ++ rightVal) dict


mergeDicts :
  Dict.Dict comparable appendable
  -> Dict.Dict comparable appendable
  -> Dict.Dict comparable appendable
mergeDicts d1 d2 =
  Dict.merge Dict.insert insertBoth Dict.insert d1 d2 Dict.empty


tuplesToDicts : List ( comparable, a ) -> List (Dict.Dict comparable (List a))
tuplesToDicts l =
  List.map (\( a, b ) -> Dict.insert a [ b ] Dict.empty) l


listToDictOfLists : List ( comparable, a ) -> Dict.Dict comparable (List a)
listToDictOfLists list =
  let
    dictOfLists l =
      (List.foldl (\a b -> mergeDicts a b) Dict.empty) (tuplesToDicts l)
  in
    dictOfLists list
