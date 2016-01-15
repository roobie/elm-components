{-
A basic authentication component
-}

module Auth where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Env
import Json.Decode as Dec
import Json.Encode as Enc
import Http
import Effects exposing (Effects, Never)
import Task exposing (..)
import Erl exposing (Url)

--====================================================================
-- Model

type alias Username = String
type alias Password = String
type alias AuthToken = String

type alias Model =
  { authToken: Maybe AuthToken
  , username: Username
  , password: Password
  }

init : (Model, Effects Action)
init =
  ( Model Nothing "bjorn" "12345"
  , Effects.none
  )


--====================================================================
-- Update

type Action
  = Verify (Username, Password)
  | Deny
  | SetToken (Maybe AuthToken)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Verify (username, password) ->
      (model, (authenticate username password))
    Deny ->
      -- In order to deny this session
      -- simply forget the authToken
      ((Model Nothing "" ""), Effects.none)
    SetToken maybeToken ->
      ( { model | authToken = maybeToken }
      , Effects.none
      )


body : Username -> Password -> Http.Body
body username password =
  Http.string (Enc.encode 0
                 (Enc.object [ ("username", Enc.string username)
                             , ("password", Enc.string password)
                             ]
                 )
              )

authenticate : Username -> Password -> Effects Action
authenticate u p =
  --Http.post decode (authUrl) (body u p)
  (authRequest u p)
    |> Task.toMaybe
    |> Task.map SetToken
    |> Effects.task


authRequest : Username -> Password -> Task Http.Error String
authRequest u p =
  Http.fromJson decode (Http.send Http.defaultSettings
    { verb = "POST"
    , headers = [ ("Accept", "application/json, text/plain, */*")
                , ("Content-Type", "application/json")]
    , url = authUrl
    , body = (body u p)
    })


authUrl : String
authUrl =
  let
    cfg = Env.config
    apiRoot = cfg.apiRoot
  in
  Erl.toString { apiRoot
               | path = (List.append cfg.authBasePath ["login"])
               } ++ "/"

decode : Dec.Decoder String
decode = Dec.at ["auth_token"] Dec.string


--====================================================================
-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let usernameInput = input [ value model.username
                            , placeholder "Username" ] []
      passwordInput = input [ value model.password
                            , placeholder "Password" ] []
      submit = button
               [ onClick address (Verify (model.username, model.password))]
               [ text "Authenticate" ]
  in
    div []
        [ usernameInput
        , passwordInput
        , submit
        ]
