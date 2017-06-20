module Author exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import PullRequest


-- MODEL


type alias Model =
  { name : String
  , prs : List PullRequest.Model
  , expanded : Bool
  }


create : String -> List PullRequest.Model -> Model
create name prs =
  Model name prs False



-- UPDATE


type Msg
  = ToggleOpen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleOpen ->
      ( { model | expanded = not model.expanded }, Cmd.none )


githubImageLocation : Model -> String
githubImageLocation model =
  "https://invent.focusvision.com/" ++ model.name ++ ".png?size=400"



-- VIEW


viewPRInfo : Model -> Html Msg
viewPRInfo model =
  if model.expanded then
    div [ class "pull-requests" ]
      (List.map PullRequest.view model.prs)
  else
    Html.text ""


view : Model -> Html Msg
view model =
  div [ class ("pr-col pr-col-" ++ toString (List.length model.prs)) ]
    [ img
        [ src (githubImageLocation model)
        , class "user-icon circle"
        , onClick ToggleOpen
        ]
        []
    , viewPRInfo model
    ]
