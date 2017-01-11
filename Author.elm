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
  }



-- UPDATE
-- VIEW


view : Model -> Html msg
view model =
  div []
    [ h3 [] [ text (model.name) ]
    , div [] (List.map PullRequest.view model.prs)
    ]
