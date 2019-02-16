
module JsonParser exposing (run, JsonObject(..))

import Parser as P exposing ((|.), (|=), Parser, Trailing(..))


run : String -> Result (List P.DeadEnd) JsonObject
run json =
    P.run jsonParser json


type JsonNumber
    = JInt Int
    | JFloat Float


type JsonObject
    = JObject (List ( String, JsonObject ))
    | JArray (List JsonObject)
    | JString String
    | JNum JsonNumber
    | JBool Bool
    | JNull


jsonStringParser : P.Parser String
jsonStringParser =
    P.getChompedString <|
        P.succeed ()
        |. P.spaces
            |. P.symbol "\""
            |. P.chompWhile (\c -> not (c=='"' || c == ' ' || c == '\n' || c == '\r'))
            |. P.symbol "\""
        |. P.spaces


jsonPairParser : P.Parser ( String, JsonObject )
jsonPairParser =
    P.succeed Tuple.pair
        |. P.spaces
        |= jsonStringParser
        |. P.spaces
        |. P.symbol ":"
        |. P.spaces
        |= P.lazy (\_ -> jsonParser)


jsonNumberParser : P.Parser JsonNumber
jsonNumberParser =
    P.number
        { int = Just JInt
        , hex = Just JInt
        , octal = Just JInt
        , binary = Just JInt
        , float = Just JFloat
        }


jsonParser : P.Parser JsonObject
jsonParser =
    P.oneOf
        [ P.succeed JNull
            |. P.spaces
            |. P.keyword "null"
            |. P.spaces
        , P.map (\_ -> JBool True) (P.keyword "true")
        , P.map (\_ -> JBool False) (P.keyword "false")
        , P.succeed JString
            |. P.spaces
            |= jsonStringParser
        , P.succeed JArray
            |= P.sequence
                { start = "["
                , separator = ","
                , end = "]"
                , spaces = P.spaces
                , item = P.lazy (\_ -> jsonParser)
                , trailing = Optional
                }
        , P.succeed JObject
            |= P.sequence
                { start = "{"
                , separator = ","
                , end = "}"
                , spaces = P.spaces
                , item = P.lazy (\_ -> jsonPairParser)
                , trailing = Optional
                }
        , P.succeed JNum
            |= jsonNumberParser
        ]
