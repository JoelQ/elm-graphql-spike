module GraphQL.Arguments exposing (Argument, string, union, int, toKeyValueStrings)

{-| Typed arguments to GraphQL queries


# Definition

@docs Argument


# Constructors

@docs int, string, union


# Serialization

@docs toKeyValueStrings

-}


{-| An opaque type representing an argument to a GraphQL query. Can only be
constructed using one of the helper functions in this module.
-}
type Argument
    = StringArg String String
    | IntArg String Int
    | UnionArg String String


{-| A helper function for constructing integer arguments.

    [int "age" 42]
        |> toKeyValueStrings -- "(age: 42)"

-}
int : String -> Int -> Argument
int =
    IntArg


{-| A helper function for constructing string arguments.

    [string "name" "luke"]
        |> toKeyValueStrings -- "(name: \"luke\")"

-}
string : String -> String -> Argument
string =
    StringArg


{-| A helper function for constructing union type arguments.

    [union "episode" "JEDI"]
        |> toKeyValueStrings -- "(episode: JEDI)"

-}
union : String -> String -> Argument
union =
    UnionArg


{-| A helper function that turns a list of arguments into a string that can be
included in a GraphQL query.

    [int "age" 42, union "episode" "JEDI"]
        |> toKeyValueStrings -- "(age: 42, episode: JEDI)"

-}
toKeyValueStrings : List Argument -> String
toKeyValueStrings args =
    case args of
        [] ->
            ""

        _ ->
            "(" ++ (String.join ", " <| List.map toKeyValueString args) ++ ")"


toKeyValueString : Argument -> String
toKeyValueString arg =
    case arg of
        StringArg name value ->
            name ++ ": \"" ++ value ++ "\""

        IntArg name value ->
            name ++ ": " ++ toString value

        UnionArg name value ->
            name ++ ": " ++ value
