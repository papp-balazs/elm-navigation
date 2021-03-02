module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (href)
import Url
import Url.Parser as Parser exposing (Parser, oneOf, s, map, top)

-- MAIN

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

-- MODEL

type Route
    = Home
    | Profile
    | NotFound

parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Profile (s "profile")
        ]

fromUrl : Url.Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (Parser.parse parser url)

type alias Model =
    { key : Nav.Key
    , route : Route
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key (fromUrl url), Cmd.none )

-- UPDATE

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = fromUrl url }
            , Cmd.none
            )

-- SUBSRCIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ div []
            [ (viewRouter model)
            , ul []
                [ viewLink "/home"
                , viewLink "/profile"
                ]
            ]
        ]
    }

viewRouter : Model -> Html msg
viewRouter model =
    case model.route of
        Home ->
            viewHome
        Profile ->
            viewProfile
        NotFound ->
            text "404"

viewLink : String -> Html msg
viewLink path =
    li []
        [ a [ href path ] [ text path ] ]

viewHome : Html msg
viewHome =
    text "This is the home"

viewProfile : Html msg
viewProfile =
    text "This is the profile"
