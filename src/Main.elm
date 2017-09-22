module Main exposing (..)

import Html exposing (code, pre, text, Html)
import GraphQL.Query exposing (..)
import GraphQL.Arguments exposing (union)


hero : String -> Query
hero episode =
    (object "hero" [ union "episode" episode ])
        [ field "name" []
        , (object "friends" [])
            [ field "name" [] ]
        ]


query1 : Query
query1 =
    alias "empireHero" (hero "EMPIRE")


query2 : Query
query2 =
    alias "jediHero" (hero "JEDI")


queryString : String
queryString =
    toNamedQueryString "HeroNameAndFriends" <| [ query1, query2 ]


main : Html a
main =
    code [] [ pre [] [ text queryString ] ]
