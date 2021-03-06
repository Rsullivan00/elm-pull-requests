module PRCountColumn exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array exposing (Array)
import Author
import Utils exposing (..)


-- MODEL


type alias Model =
  { count : Int
  , authors : Array Author.Model
  }


create : Int -> List Author.Model -> Model
create count authors =
  Model count (Array.fromList authors)



-- UPDATE


type Msg
  = AuthorMsg Int Author.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AuthorMsg i msg ->
      let
        updatedAuthors =
          updateAtIndex model.authors i Author.update msg
      in
        ( { model | authors = updatedAuthors }, Cmd.none )



-- VIEW


viewHeader : Int -> Html Msg
viewHeader idx =
  let
    idx =
      toString idx
  in
    h1 [ class "col-count-header" ] [ text idx ]


viewAuthors : List Author.Model -> List (Html Msg)
viewAuthors authors =
  (List.indexedMap
    (\i author -> Html.map (AuthorMsg i) (Author.view author))
    authors
  )


view : Model -> Html Msg
view model =
  let
    header =
      viewHeader model.count

    authorHtml =
      viewAuthors (Array.toList model.authors)
  in
    div [ class "pr-count-col" ]
      (header :: authorHtml)
