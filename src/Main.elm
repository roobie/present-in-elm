module Main exposing (ContentSlideData, Model, Msg(..), Slide(..), SlideStep(..), TitleSlideData, defaultSlide, firstSlide, handleKey, init, initSlide, initSlides, main, onKeyUp, subscriptions, update, view, viewContentSlide, viewSlide, viewStep, viewTitleSlide, withDefaultSlide)

import Browser
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Json
import Ports



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type SlideStep
    = StepLine { content : String }
    | StepCode { content : String }
    | StepCodeBlock { content : String }


type alias TitleSlideData =
    { title : String
    }


type alias ContentSlideData =
    { title : String
    }


type Slide
    = TitleSlide TitleSlideData
    | ContentSlide ContentSlideData


initSlide title =
    TitleSlide { title = title }


firstSlide =
    initSlide "Exceptions"


defaultSlide =
    ( TitleSlide { title = "The end." }, [] )


withDefaultSlide =
    Maybe.withDefault defaultSlide


type alias Model =
    { slide : Slide
    , slides : List ( Slide, List SlideStep )
    , stepsSoFar : List SlideStep
    , stepsLeft : List SlideStep
    }


initSlides : List ( Slide, List SlideStep )
initSlides =
    [ ( ContentSlide
            { title = "Exceptions - A" }
      , [ StepLine { content = "Test1" }
        , StepCode { content = "type alias Test = {a:String}" }
        , StepCodeBlock
            { content =
                """type Msg
    = StepForward
    | NextSlide
"""
            }
        ]
      )
    , ( TitleSlide { title = "Exceptions - B" }, [] )
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model firstSlide initSlides [] []
    , Cmd.none
    )



-- UPDATE


type Msg
    = StepForward
    | NextSlide


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nextStep =
            List.head model.stepsLeft

        ( nextSlide, nextSlideSteps ) =
            withDefaultSlide (List.head model.slides)
    in
    case msg of
        StepForward ->
            case nextStep of
                Just step ->
                    ( { model
                        | stepsSoFar = step :: model.stepsSoFar
                        , stepsLeft = List.drop 1 model.stepsLeft
                      }
                    , Cmd.none
                    )

                Nothing ->
                    update NextSlide model

        NextSlide ->
            ( { model
                | stepsSoFar = []
                , stepsLeft = nextSlideSteps
                , slide = nextSlide
                , slides = List.drop 1 model.slides
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Sub.none
    Ports.globalKeyUp handleKey



-- VIEW


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    HE.on "keyup" (Json.map tagger HE.keyCode)


handleKey _ =
    StepForward


view : Model -> Html Msg
view model =
    div
        [ HA.class "fill"
        , HE.onClick StepForward
        ]
        [ div [ HA.class "row fill" ]
            [ viewSlide model
            ]
        ]


viewSlide : Model -> Html Msg
viewSlide model =
    case model.slide of
        TitleSlide titleSlide ->
            viewTitleSlide model titleSlide

        ContentSlide contentSlide ->
            viewContentSlide model contentSlide


viewContentSlide : Model -> TitleSlideData -> Html Msg
viewContentSlide model slide =
    div [ HA.class "fade-in" ]
        [ div [] [ text slide.title ]
        , hr [] []
        , div [] (List.map viewStep (List.reverse model.stepsSoFar))
        ]


viewTitleSlide : Model -> TitleSlideData -> Html Msg
viewTitleSlide model slide =
    div [ HA.class "col center fade-in" ]
        [ div [] [ text slide.title ]
        , hr [] []
        , div [] (List.map viewStep (List.reverse model.stepsSoFar))
        ]


viewStep : SlideStep -> Html Msg
viewStep step =
    case step of
        StepLine { content } ->
            div [] [ text content ]

        StepCode { content } ->
            div []
                [ code [] [ text content ]
                ]

        StepCodeBlock { content } ->
            div []
                [ pre [ HA.class "base02" ]
                    [ code [] [ text content ]
                    ]
                ]
