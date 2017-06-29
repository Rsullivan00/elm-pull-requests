module Utils exposing (..)

import Dict
import Array exposing (Array)


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


updateAtIndex : Array a -> Int -> (b -> a -> ( a, c )) -> b -> Array a
updateAtIndex models idx updateFn msg =
  case Array.get idx models of
    Nothing ->
      models

    Just model ->
      let
        before =
          Array.slice 0 idx models

        after =
          Array.slice (idx + 1) (Array.length models) models

        ( updatedModel, modelCmd ) =
          updateFn msg model

        updatedModels =
          Array.append (Array.push updatedModel before) after
      in
        updatedModels
