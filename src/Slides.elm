module Slides exposing (Deck, Slide(..), Step(..))


type alias Deck =
    { slide : Slide
    , stepsDone : List Step
    , stepsLeft : List Step
    , slidesDone : List Slide
    , slidesLeft : List Slide
    }


type Step
    = AddLineOfText String


type Slide
    = TitleSlide { title : String, subtitle : String }
    | TitleWithContent { title : String, content : List String }
