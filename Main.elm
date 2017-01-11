module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Task exposing (Task)
import Json.Decode as Decode exposing ((:=), Decoder)
import Dict
import Repo
import Author


main : Program Never
main =
  Html.program
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }



-- MODEL


type alias Model =
  { repos : List Repo.Model
  , authors : List Author.Model
  , errorDescription : String
  }


init : ( Model, Cmd Msg )
init =
  ( Model [] [] "", fetchStatsCmd )



-- UPDATE


type Msg
  = HttpError Http.Error
  | FetchStats (List Repo.Model)
  | RefreshStats


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HttpError error ->
      ( { model | errorDescription = "Error: " ++ (toString error) }, Cmd.none )

    FetchStats result ->
      ( { model | repos = result, authors = (reposToAuthors result) }, Cmd.none )

    RefreshStats ->
      ( model, fetchStatsCmd )


listToDictOfLists : List ( comparable, a ) -> Dict.Dict comparable (List a)
listToDictOfLists list =
  let
    insertBoth key leftVal rightVal dict =
      Dict.insert key (leftVal ++ rightVal) dict

    mergeDicts d1 d2 =
      Dict.merge Dict.insert insertBoth Dict.insert d1 d2 Dict.empty

    listOfDicts l =
      List.map (\( a, b ) -> Dict.insert a [ b ] Dict.empty) l

    dictOfLists l =
      (List.foldl (\a b -> mergeDicts a b) Dict.empty) (listOfDicts l)
  in
    dictOfLists list


reposToAuthors : List Repo.Model -> List Author.Model
reposToAuthors repos =
  let
    flattenedPRList =
      List.concatMap (\repo -> (List.map (\pr -> ( pr.user, pr )) repo.prs)) repos

    prDict =
      listToDictOfLists flattenedPRList

    authorDict =
      Dict.map (\key value -> Author.Model key value) prDict
  in
    Dict.values authorDict



-- API stuff


api : String
api =
  "http://localhost:3000/"


statsUrl : String
statsUrl =
  api ++ "stats"


fetchStats : Platform.Task Http.Error (List Repo.Model)
fetchStats =
  Http.get (Decode.list Repo.decoder) statsUrl


fetchStatsCmd : Cmd Msg
fetchStatsCmd =
  Task.perform HttpError FetchStats fetchStats



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ h1 [] [ text "Open pull requests" ]
    , h4 [] [ text model.errorDescription ]
    , div [] (List.map Author.view model.authors)
    , button [ class "btn", onClick RefreshStats ]
        [ i [ class "material-icons left" ]
            [ text "loop" ]
        , text "Refresh"
        ]
    ]
