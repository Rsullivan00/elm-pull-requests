module PRCountColumn exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array exposing (Array)
import Author


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
      case Array.get i model.authors of
        Nothing ->
          ( model, Cmd.none )

        Just author ->
          let
            authors =
              model.authors

            ( updatedAuthor, authorCmd ) =
              Author.update msg author

            beforeAuthors =
              Array.slice 0 i authors

            afterAuthors =
              Array.slice (i + 1) (Array.length authors) authors

            updatedAuthors =
              Array.append (Array.push updatedAuthor beforeAuthors) afterAuthors
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
