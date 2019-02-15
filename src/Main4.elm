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
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)
import Ports
import Random as R
import Set exposing (Set)
import Svg exposing (svg)
import Svg.Attributes as Svga
import Task
import Url



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


defaultPosition =
    V2.vec2 0 0


defaultViewport =
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


type VisualElement
    = Circle { radius : Float }
    | Rect { w : Float, h : Float }
    | Text { content : String }


type TransformComponent
    = Scale Float Float
    | Translate Float Float
    | Rotate Float
    | SkewX Float
    | SkewY Float


type Transform
    = EmptyTransform
    | Matrix Mat4
    | Composition (List TransformComponent)


type alias Thing =
    { animX : Animation
    , animY : Animation
    , transform : Transform
    , fill : String
    , visualElement : VisualElement
    }


type SlideContent
    = TitleSlide { title : String }


type StepContent
    = LineOfText { text : String }
    | Image { href : Url.Url }


type alias Step =
    { content : StepContent }


type alias Slide =
    { stepsDone : List Step
    , stepsTodo : List Step
    , content : SlideContent
    }


defaultSlide =
    Slide [] [] (TitleSlide { title = "Default" })


type alias Model =
    { time : Float
    , things : List Thing
    , slidesDone : List Slide
    , slidesTodo : List Slide
    , currentSlide : Slide
    , viewport : BD.Viewport
    }


moveAnim : A.Clock -> Float -> Float -> Animation
moveAnim startTime from to =
    A.animation startTime
        |> A.from from
        |> A.to to
        |> A.ease (Ease.inOut Ease.inCubic Ease.outExpo)
        |> A.duration 40
        |> A.delay 10


inverse =
    M4.makeRotate 3 (V3.vec3 1 1 0)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , things =
            [ { visualElement = Rect { w = 50, h = 50 }
              , animX = A.static 0
              , animY = A.static 0

              -- , transform = EmptyTransform
              --   , transform = Composition [ Rotate (V3.vec3 100 10 10) ]
              , transform =
                    Composition
                        [ Translate 25 25
                        , Rotate 100
                        , Translate -25 -25
                        ]
              , fill = "#ff0000"
              }
            , { visualElement = Rect { w = 10, h = 10 }
              , animX = A.static 0
              , animY = A.static 0

              -- , transform = EmptyTransform
              --   , transform = Composition [ Rotate (V3.vec3 100 10 10) ]
              , transform =
                    Composition
                        [ Translate 10 -10
                        , Rotate 0
                        , Scale 1.5 1.5
                        , Translate -105 -105
                        ]
              , fill = "#00ff00"
              }
            , { visualElement = Rect { w = 30, h = 30 }
              , animX = A.static 0
              , animY = A.static 0

              -- , transform = EmptyTransform
              --   , transform = Composition [ Rotate (V3.vec3 100 10 10) ]
              , transform =
                    Composition
                        [ Translate 15 15
                        , Rotate 0
                        , Translate -15 -15

                        -- , Scale 1.5 1.5
                        , Translate -145 -145
                        ]
              , fill = "#cccc00"
              }
            , { visualElement = Text { content = "Hello" }
              , animX = A.static 0
              , animY = A.static 0
              , transform = EmptyTransform
              , fill = "#00ff00"
              }
            ]
      , viewport = defaultViewport
      , slidesDone = []
      , slidesTodo = []
      , currentSlide = defaultSlide
      }
    , Task.perform StoreViewport BD.getViewport
    )


type Msg
    = RunAnimations Float
    | GlobalKeyUp Int
    | NewDestination Vec3
    | GetViewport
    | StoreViewport BD.Viewport


randomPoint : ( Float, Float ) -> ( Float, Float ) -> R.Generator Vec3
randomPoint ( minX, maxX ) ( minY, maxY ) =
    R.map3 V3.vec3
        (R.float minX maxX)
        (R.float minY maxY)
        (R.constant 0)


randomPointInViewport : Model -> R.Generator Vec3
randomPointInViewport model =
    let
        vp =
            model.viewport.viewport

        halfW =
            vp.width / 2

        halfH =
            vp.height / 2
    in
    randomPoint ( -halfW, halfW ) ( -halfH, halfH )


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
            case thing.visualElement of
                Circle c ->
                    thing

                _ ->
                    { thing
                        | animX = newAnim x nextX
                        , animY = newAnim y nextY
                    }

        maybeNewDest =
            case List.head model.things of
                Just thing ->
                    -- if A.isDone model.time thing.animX then
                    --     R.generate NewDestination
                    --         (randomPoint width height)
                    -- else
                    Cmd.none

                Nothing ->
                    Cmd.none
    in
    case msg of
        GlobalKeyUp keyCode ->
            ( model
            , R.generate NewDestination
                (randomPointInViewport model)
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
            ( model
            , Task.perform StoreViewport BD.getViewport
            )

        StoreViewport vp ->
            ( { model
                | viewport = vp
                , things =
                    { visualElement = Circle { radius = 10 }
                    , fill = "#770f22"
                    , animX = A.static 0
                    , animY = A.static 0
                    , transform = EmptyTransform
                    }
                        :: model.things
              }
            , Cmd.none
            )


triggerGetViewport _ _ =
    GetViewport


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
            , viewCurrentSlide model
            ]
        ]


viewCurrentSlide : Model -> Html Msg
viewCurrentSlide model =
    div [ HA.class "col fill center" ]
        [ div [] []
        , div [] (List.map (viewDebugInfo model.time) model.things)
        ]


viewDebugInfo : Float -> Thing -> Html Msg
viewDebugInfo time thing =
    div []
        []


graphic : Model -> Html Msg
graphic model =
    let
        vp =
            model.viewport.viewport

        halfW =
            vp.width / 2

        halfH =
            vp.height / 2
    in
    svg
        [ Svga.viewBox
            (String.concat
                [ String.fromFloat -halfW
                , " "
                , String.fromFloat -halfH
                , " "
                , String.fromFloat vp.width
                , " "
                , String.fromFloat vp.height
                ]
            )
        ]
        (List.map (viewThing model.time) model.things)


transformSvgAttribute : Float -> Vec3 -> Thing -> Svg.Attribute Msg
transformSvgAttribute time pos thing =
    let
        { x, y } =
            V3.toRecord pos
    in
    case thing.transform of
        EmptyTransform ->
            Svga.name "empty-transform"

        Matrix mat4 ->
            let
                { m11, m21, m31, m12, m22, m32, m13, m23, m33 } =
                    M4.toRecord mat4
            in
            Svga.transform
                (String.concat
                    [ "matrix("
                    , String.fromFloat m11
                    , " "
                    , String.fromFloat m21
                    , " "
                    , String.fromFloat m12
                    , " "
                    , String.fromFloat m22
                    , " "
                    , String.fromFloat m13
                    , " "
                    , String.fromFloat m23
                    , ")"
                    ]
                )

        Composition c ->
            let
                str component accumulator =
                    accumulator
                        ++ " "
                        ++ (case component of
                                Scale sx sy ->
                                    String.concat
                                        [ "scale("

                                        -- , String.fromFloat sx
                                        -- , String.fromFloat (sx * (cos (degrees time)))
                                        , String.fromFloat sx
                                        , " "

                                        -- , String.fromFloat (sy * (sin (degrees time)))
                                        , String.fromFloat sy
                                        , ")"
                                        ]

                                Translate tx ty ->
                                    String.concat
                                        [ "translate("
                                        , String.fromFloat tx
                                        , " "
                                        , String.fromFloat ty
                                        , ")"
                                        ]

                                Rotate degs ->
                                    String.concat
                                        [ "rotate("
                                        , String.fromFloat time
                                        , " "
                                        , String.fromFloat x
                                        , " "
                                        , String.fromFloat y
                                        , ")"
                                        ]

                                SkewX f ->
                                    String.concat [ "skewX(", String.fromFloat f, ")" ]

                                SkewY f ->
                                    String.concat [ "skewY(", String.fromFloat f, ")" ]
                           )
            in
            Svga.transform
                (List.foldl str "" c)


viewThing : Float -> Thing -> Html Msg
viewThing time thing =
    let
        pos =
            doAnimateThing time thing

        { x, y } =
            V3.toRecord pos
    in
    case thing.visualElement of
        Circle c ->
            Svg.circle
                [ Svga.cx (String.fromFloat x)
                , Svga.cy (String.fromFloat y)
                , Svga.r (String.fromFloat c.radius)
                , Svga.fill thing.fill
                , transformSvgAttribute time pos thing
                ]
                []

        Rect r ->
            Svg.rect
                [ Svga.x (String.fromFloat x)
                , Svga.y (String.fromFloat y)
                , Svga.width (String.fromFloat r.w)
                , Svga.height (String.fromFloat r.h)
                , Svga.fill thing.fill
                , transformSvgAttribute time pos thing
                ]
                []

        Text t ->
            Svg.text_
                [ Svga.x (String.fromFloat x)
                , Svga.y (String.fromFloat y)
                , Svga.fill thing.fill
                , transformSvgAttribute time pos thing
                ]
                [ Svg.text t.content ]
