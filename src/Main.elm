module Main exposing (ContentSlideData, Model, Msg(..), Slide(..), SlideStep(..), TitleSlideData, defaultSlide, firstSlide, handleKey, init, initSlide, initSlides, main, onKeyUp, subscriptions, update, view, viewContentSlide, viewSlide, viewStep, viewTitleSlide, withDefaultSlide)

-- https://github.com/mdgriffith/elm-style-animation/issues/67
-- https://github.com/elm/compiler/issues/1851
-- https://package.elm-lang.org/packages/mdgriffith/elm-style-animation/latest/
-- import Animation

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

    -- animation stuff
    -- , style : Animation.State
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
    ( Model firstSlide
        initSlides
        []
        []
      -- (Animation.style
      --     [ Animation.left (Animation.px 0.0)
      --     , Animation.opacity 1.0
      --     ]
      -- )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Nop
    | StepBack
    | StepForward
    | NextSlide



-- | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        prevStep =
            List.head model.stepsSoFar

        nextStep =
            List.head model.stepsLeft

        ( nextSlide, nextSlideSteps ) =
            withDefaultSlide (List.head model.slides)
    in
    case msg of
        Nop ->
            ( model, Cmd.none )

        -- Animate animMsg ->
        --     ( { model
        --         | style = Animation.update animMsg model.style
        --       }
        --     , Cmd.none
        --     )
        StepBack ->
            case prevStep of
                Just step ->
                    ( { model
                        | stepsSoFar = List.drop 1 model.stepsSoFar
                        , stepsLeft = step :: model.stepsLeft
                      }
                    , Cmd.none
                    )

                Nothing ->
                    -- PrevSlide
                    ( model, Cmd.none )

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



-- Sub.batch
--     [ Ports.globalKeyUp handleKey
--     , Animation.subscription
--         Animate
--         [ model.style ]
--     ]
-- VIEW


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    HE.on "keyup" (Json.map tagger HE.keyCode)


handleKey keyCode =
    case keyCode of
        --K
        75 ->
            StepBack

        -- Right
        37 ->
            StepBack

        -- J
        74 ->
            StepForward

        -- Left
        39 ->
            StepForward

        _ ->
            Nop


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
    div []
        [ div [ HA.class "fade-in" ] [ text slide.title ]
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
            div [ HA.class "fade-in" ] [ text content ]

        StepCode { content } ->
            div [ HA.class "fade-in" ]
                [ code [] [ text content ]
                ]

        StepCodeBlock { content } ->
            div [ HA.class "fade-in" ]
                [ pre [ HA.class "base02" ]
                    [ code [] [ text content ]
                    ]
                ]
