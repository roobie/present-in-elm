module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Product =
    { id : Int
    , title : String
    }


listOfProducts =
    [ { id = 1, title = "Product A" }
    , { id = 2, title = "Product A" }
    ]


type alias Model =
    { allProducts : List Product
    , currentGroup : List Product
    , highlightedProduct : Maybe Product
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model listOfProducts [] Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoMsg
    | ClickedAnywhere
    | AddProductToCurrentGroup Product
    | HighlightProduct Product


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        ClickedAnywhere ->
            Debug.log "T" ( model, Cmd.none )

        AddProductToCurrentGroup product ->
            ( { model | currentGroup = product :: model.currentGroup }
            , Cmd.none
            )

        HighlightProduct product ->
            ( { model | highlightedProduct = Just product }
            , Cmd.none
            )



-- SUBSCRIPTIONS


type Key
    = Character Char
    | Control String


type alias KeyEvent =
    { key : Key
    , ctrlKey : Bool
    , shiftKey : Bool
    , altKey : Bool
    }


type KeyEventType
    = KeyPress KeyEvent
    | KeyDown KeyEvent
    | KeyUp KeyEvent


keyEventDecoder : JD.Decoder KeyEvent
keyEventDecoder =
    JD.map4 KeyEvent
        (JD.map toKey (JD.field "key" JD.string))
        (JD.field "ctrlKey" JD.bool)
        (JD.field "shiftKey" JD.bool)
        (JD.field "altKey" JD.bool)


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


handleKeyEvent : (KeyEvent -> KeyEventType) -> KeyEvent -> Msg
handleKeyEvent tag ke =
    let
        a =
            Debug.log "A" (tag ke)
    in
    case ke.key of
        Character c ->
            NoMsg

        Control c ->
            NoMsg


onKeyPress : JD.Decoder Msg
onKeyPress =
    JD.map (handleKeyEvent KeyPress) keyEventDecoder


onKeyUp : JD.Decoder Msg
onKeyUp =
    JD.map (handleKeyEvent KeyUp) keyEventDecoder


onKeyDown : JD.Decoder Msg
onKeyDown =
    JD.map (handleKeyEvent KeyDown) keyEventDecoder


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyPress onKeyPress
        , Browser.Events.onKeyUp onKeyUp
        , Browser.Events.onKeyDown onKeyDown
        , Browser.Events.onClick (JD.succeed ClickedAnywhere)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewAllProductsInGroup model
        , viewAllProducts model
        ]

viewAllProductsInGroup : Model -> Html Msg
viewAllProductsInGroup model =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "A" ]
                , th [] [ text "B" ]
                , th [] [ text "C" ]
                , th [] [ text "D" ]
                , th [] [ text "E" ]
                ]
            ]
        , tbody []
            (List.map viewSingleProduct model.currentGroup)
        ]

viewAllProducts : Model -> Html Msg
viewAllProducts model =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "A" ]
                , th [] [ text "B" ]
                , th [] [ text "C" ]
                , th [] [ text "D" ]
                , th [] [ text "E" ]
                ]
            ]
        , tbody []
            (List.map viewSingleProduct model.allProducts)
        ]


viewSingleProduct : Product -> Html Msg
viewSingleProduct product =
    tr []
        [ td [] [ text (String.fromInt product.id) ]
        , td [] [ text product.title ]
        ]
