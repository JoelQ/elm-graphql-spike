module GraphQL.Query
    exposing
        ( Query
        , field
        , fieldWithArgs
        , fieldList
        , object
        , objectWithArgs
        , alias
        , toQueryString
        , toNamedQueryString
        )

{-| A set of functions that allow building up GraphQL queries in Elm.


# Philosophy

This library attempts to provide idomatic GraphQL query building in Elm. As
such, not all features of GraphQL are implemented for the user. For example,
GraphQL variables are not necessary when dynamically generating queries.

Fragments allow re-use of pieces of queries. This isn't necessary in Elm since
you can just extract and re-use functions instead.


# Usage

Here's what that might might look like:

    friends : Query
    friends =
      object "friends" [ field "name" ]

    hero : String -> Query
    hero episode =
      objectWithArgs "hero" [union "episode" episode]
        [ field "name"
        , friends
        ]

    rawQueryString : String
    rawQueryString =
      toQueryString
        [ alias "jediHero" (hero "JEDI")
        , alias "empireHero" (hero "EMPIRE)
        ]


# Definition

@docs Query


# Query building

@docs field, fieldList, fieldWithArgs, object, objectWithArgs, alias


# Serialization

@docs toQueryString, toNamedQueryString

-}

import GraphQL.Arguments exposing (..)


{-| Opaque type describing a GraphQL query. Can only be created via the
constructor functions `field`, `object`, or `alias`.
-}
type Query
    = Leaf String (List Argument)
    | Node String (List Argument) (List Query)
    | Alias String Query


{-| Query for a single scalar value

    field "name" -- get the name

-}
field : String -> Query
field name =
    fieldWithArgs name []


{-| It's common to want to get a list of scalar values. This is a shortcut to
having to write a bunch of calls to `field`.

    fieldList ["name", "height", "age"]
    -- fetch name, height, and age

-}
fieldList : List String -> List Query
fieldList =
    List.map field


{-| Query for a single scalar value. Can be modified by a list of
`GraphQL.Argument`s.

    fieldWithArgs "height" [union "unit" "METER"]
    -- get the height in meters

-}
fieldWithArgs : String -> List Argument -> Query
fieldWithArgs name args =
    Leaf name args


{-| Fetch an object that contains a series of fields or sub-objects.

    object "hero"
      [ field "name"
      , object "friends" [ field "name" ]
      ]

-}
object : String -> List Query -> Query
object name fields =
    objectWithArgs name [] fields


{-| Like `object` but allows a list of `GraphQL.Argument` to be passed.

    object "human" [string "id" "1000"]
      [ field "name"
      , field "height"
      ]

-}
objectWithArgs : String -> List Argument -> List Query -> Query
objectWithArgs name args fields =
    Node name args fields


{-| Allows aliasing different queries on the same field/object.

    alias "empireHero"
    (objectWithArgs "hero" [union "episode" "EMPIRE"][field "name"])

    alias "jediHero"
    (objectWithArgs "hero" [union "episode" "JEDI"][field "name"])

-}
alias : String -> Query -> Query
alias name query =
    case query of
        Alias _ _ ->
            query

        _ ->
            Alias name query


{-| Convert a list of queries into a GraphQL query string that can be sent over
the wire.

    object "hero" [ field "name" , object "friends" [ field "name" ] ]
        |> toQueryString

        -- "query { hero {
        --     name
        --     friends { name }
        --     }
        --  }"

-}
toQueryString : List Query -> String
toQueryString queries =
    "{ " ++ (String.join ("\n") <| List.map innerQuery queries) ++ " }"


{-| Just like `toQueryString` but allows the query to be named.

    object "hero" [ field "name" , object "friends" [ field "name" ] ]
        |> toNamedQueryString "HeroAndFriends"

        -- "query HeroNameAndFriends { hero {
        --     name
        --     friends { name }
        --     }
        --  }"

-}
toNamedQueryString : String -> List Query -> String
toNamedQueryString name queries =
    "query " ++ name ++ " " ++ toQueryString queries


innerQuery : Query -> String
innerQuery query =
    case query of
        Leaf name args ->
            name ++ toKeyValueStrings args

        Node name args fields ->
            name ++ toKeyValueStrings args ++ " {" ++ (String.join "\n" <| List.map innerQuery fields) ++ "}"

        Alias name real ->
            name ++ ": " ++ innerQuery real
