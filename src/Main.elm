module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Html exposing (Html, button, div, text, nav, a)
import Html.Attributes exposing (style, href, target)
import Html.Events exposing (onClick)


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

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , currentTab : Tab
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url Tab1, Cmd.none )

type Tab
    = Tab1
    | Tab2
    | Tab3


-- init : Model
-- init =
--     { currentTab = Tab1 }


-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | SwitchTab Tab


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          let
            path = url.path
            newTab = urlToTab url
          in
          ( { model | currentTab = newTab }
          , Nav.pushUrl model.key path
          )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    SwitchTab tab ->
        let 
            newUrl = tabToUrl tab
        in
        ( { model | currentTab = tab }
        , Nav.pushUrl model.key newUrl
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
        [ div [ style "background-color" "#1a1a1a"
            , style "min-height" "100vh"
            , style "margin" "0"
            , style "color" "#ffffff"
            ]
            [ nav [ style "background-color" "#8b0000"
                , style "padding" "1rem"
                , style "display" "flex"
                , style "gap" "2rem"
                ]
                [ tabLink Tab1 "Tab 1" model.currentTab
                , tabLink Tab2 "Tab 2" model.currentTab
                , tabLink Tab3 "Tab 3" model.currentTab
                ]
            , div [ style "padding" "2rem" ]
                [ pageContent model.currentTab
                ]
            , text "The current URL is: "
                , b [] [ text (Url.toString model.url) ]
                , ul []
                    [ viewLink "/home"
                    , viewLink "/profile"
                    , viewLink "/reviews/the-century-of-the-self"
                    , viewLink "/reviews/public-opinion"
                    , viewLink "/reviews/shah-of-shahs"
                    ]
                ]
            ]
  }

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]

-- view : Model -> Html Msg
-- view model =
--     div [ style "background-color" "#1a1a1a"
--         , style "min-height" "100vh"
--         , style "margin" "0"
--         , style "color" "#ffffff"
--         ]
--         [ nav [ style "background-color" "#8b0000"
--              , style "padding" "1rem"
--              , style "display" "flex"
--              , style "gap" "2rem"
--              ]
--             [ tabLink Tab1 "Tab 1" model.currentTab
--             , tabLink Tab2 "Tab 2" model.currentTab
--             , tabLink Tab3 "Tab 3" model.currentTab
--             ]
--         , div [ style "padding" "2rem" ]
--             [ pageContent model.currentTab
--             ]
--         ]


tabLink : Tab -> String -> Tab -> Html Msg
tabLink tab label currentTab =
    let
        activeStyle =
            if tab == currentTab then
                [ style "border-bottom" "2px solid white" ]
            else
                []
    in
    a ([ onClick (SwitchTab tab)
       , style "color" "#ffffff"
       , style "text-decoration" "none"
       , style "cursor" "pointer"
       , style "padding-bottom" "0.25rem"
       ] ++ activeStyle)
        [ text label ]


pageContent : Tab -> Html Msg
pageContent tab =
    div []
        [ div [ style "margin-bottom" "2rem" ]
            [ case tab of
                Tab1 ->
                    text "Welcome to Page 1! This is some placeholder content."
                Tab2 ->
                    text "This is Page 2 with its own unique content."
                Tab3 ->
                    text "You've reached Page 3. Here's some sample text."
            ]
        , a [ href "https://google.com"
            , style "background-color" "#4a4a4a"
            , style "color" "white"
            , style "padding" "0.5rem 1rem"
            , style "text-decoration" "none"
            , style "border-radius" "4px"
            , target "_blank"
            ]
            [ text "Go to Google" ]
        ]

tabToUrl : Tab -> String
tabToUrl tab =
    case tab of
        Tab1 -> "/tab1"
        Tab2 -> "/tab2"
        Tab3 -> "/tab3"

urlToTab : Url.Url -> Tab
urlToTab url =
    case url.path of
        "/tab2" -> Tab2
        "/tab3" -> Tab3
        _ -> Tab1