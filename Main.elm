module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Array
import Task exposing (Task)
import Json.Decode as Decode exposing ((:=), Decoder)
import Dict
import Repo
import Author
import Utils exposing (..)


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
  , authors : Array.Array Author.Model
  , errorDescription : String
  }


init : ( Model, Cmd Msg )
init =
  ( Model [] Array.empty "", fetchStatsCmd )



-- UPDATE


type Msg
  = HttpError Http.Error
  | FetchStats (List Repo.Model)
  | RefreshStats
  | AuthorMsg Int Author.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HttpError error ->
      ( { model | errorDescription = "Error: " ++ (toString error) }, Cmd.none )

    FetchStats result ->
      ( { model | repos = result, authors = (reposToAuthors result) }, Cmd.none )

    RefreshStats ->
      ( model, fetchStatsCmd )

    AuthorMsg i authorMessage ->
      case Array.get i model.authors of
        Nothing ->
          ( model, Cmd.none )

        Just author ->
          let
            authors =
              model.authors

            ( updatedAuthor, authorCmd ) =
              Author.update authorMessage author

            beforeAuthors =
              Array.slice 0 i authors

            afterAuthors =
              Array.slice (i + 1) (Array.length authors) authors

            updatedAuthors =
              Array.append (Array.push updatedAuthor beforeAuthors) afterAuthors
          in
            ( { model | authors = updatedAuthors }, Cmd.none )


reposToAuthors : List Repo.Model -> Array.Array Author.Model
reposToAuthors repos =
  let
    flattenedPRList =
      List.concatMap (\repo -> (List.map (\pr -> ( pr.user, pr )) repo.prs)) repos

    prDict =
      listToDictOfLists flattenedPRList

    authorDict =
      Dict.map (\key value -> Author.create key value) prDict
  in
    Array.fromList (Dict.values authorDict)



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
  Task.perform HttpError
    FetchStats
    fetchStats



-- VIEW


viewAuthors : Array.Array Author.Model -> List (Html Msg)
viewAuthors authors =
  Array.toList
    (Array.indexedMap
      (\i author -> Html.map (AuthorMsg i) (Author.view author))
      authors
    )


view : Model -> Html Msg
view model =
  div [ class "app-container" ]
    [ h1 [] [ text "Open pull requests" ]
    , h4 [] [ text model.errorDescription ]
    , div [] (viewAuthors model.authors)
    , button [ class "btn", onClick RefreshStats ]
        [ i [ class "material-icons left" ]
            [ text "loop" ]
        , text "Refresh"
        ]
    ]
