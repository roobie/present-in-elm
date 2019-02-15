module Main exposing (main)

import Animation as A exposing (Animation)
import Browser
import Browser.Dom as BD
import Browser.Events as BE
import Ease
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Json
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector3 as V3 exposing (Vec3)
import Ports
import Random as R
import Svg exposing (..)
import Svg.Attributes as Svga exposing (..)
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


defaultPosition =
    V3.vec3 0 0 0


type VisualElement
    = Circle { radius : Float }


type alias Thing =
    { animX : Animation
    , animY : Animation
    , visualElement : VisualElement
    }


type alias Model =
    { time : Float
    , things : List Thing
    , viewport : BD.Viewport
    }


moveAnim : A.Clock -> Float -> Float -> Animation
moveAnim startTime from to =
    A.animation startTime
        |> A.from from
        |> A.to to
        -- |> A.ease Ease.inOutCubic
        -- |> A.ease Ease.inElastic
        -- |> A.ease Ease.outExpo
        -- |> A.ease Ease.inOutCirc
        |> A.ease (Ease.inOut Ease.inCubic Ease.outExpo)
        |> A.duration 40
        |> A.delay 10


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , things =
            [ { visualElement = Circle { radius = 20 }
              , animX = moveAnim 0 100 500
              , animY = moveAnim 0 300 20
              }
            ]
      , viewport =
            { scene =
                { width = 0
                , height = 0
                }
            , viewport =
                { x = 0
                , y = 0
                , width = 0
                , height = 0
                }
            }
      }
    , Task.perform StoreViewport BD.getViewport
    )


type Msg
    = RunAnimations Float
    | GlobalKeyUp Int
    | NewDestination Vec3
    | GetViewport
    | StoreViewport BD.Viewport


randomPoint : Float -> Float -> R.Generator Vec3
randomPoint maxX maxY =
    R.map3 V3.vec3
        (R.float 0 maxX)
        (R.float 0 maxY)
        (R.constant 0)


doAnimateThing : Float -> Thing -> Vec3
doAnimateThing time thing =
    V3.vec3
        (A.animate time thing.animX)
        (A.animate time thing.animY)
        0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { width, height } =
            model.viewport.scene

        pos =
            doAnimateThing model.time

        newAnim =
            moveAnim model.time

        setNewDestination nextX nextY thing =
            let
                { x, y } =
                    V3.toRecord (pos thing)
            in
            { thing
                | animX = newAnim x nextX
                , animY = newAnim y nextY
            }

        maybeNewDest =
            case List.head model.things of
                Just thing ->
                    if A.isDone model.time thing.animX then
                        R.generate NewDestination
                            (randomPoint width height)

                    else
                        Cmd.none

                Nothing ->
                    Cmd.none
    in
    case msg of
        GlobalKeyUp keyCode ->
            ( model
            , R.generate NewDestination
                (randomPoint width height)
            )

        RunAnimations t ->
            ( { model
                | time = model.time + 1
              }
              -- , Cmd.none
            , maybeNewDest
            )

        NewDestination v3 ->
            ( { model
                | things =
                    List.map
                        (setNewDestination (V3.getX v3) (V3.getY v3))
                        model.things
              }
            , Cmd.none
            )

        GetViewport ->
            (model
            , Task.perform StoreViewport BD.getViewport
            )

        StoreViewport vp ->
            ( { model | viewport = vp }, Cmd.none )


triggerGetViewport _ _ = GetViewport
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BE.onKeyUp (Json.map GlobalKeyUp HE.keyCode)
        , BE.onAnimationFrameDelta RunAnimations
        , BE.onResize triggerGetViewport
        ]


view : Model -> Html Msg
view model =
    div
        [ HA.class "fill"

        --, HE.onClick StepForward
        ]
        [ div [ HA.class "row fill" ]
            -- [ viewSlide model
            [ graphic model
            ]
        ]


graphic : Model -> Html Msg
graphic model =
    svg
        [--viewBox "0 0 1920 1080"
        ]
        (List.map (viewThing model.time) model.things)


viewThing : Float -> Thing -> Html Msg
viewThing time thing =
    let
        pos =
            V3.toRecord (doAnimateThing time thing)
    in
    case thing.visualElement of
        Circle c ->
            circle
                [ cx (String.fromFloat pos.x)
                , cy (String.fromFloat pos.y)
                , r (String.fromFloat c.radius)
                ]
                []
