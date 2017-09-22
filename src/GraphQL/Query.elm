module GraphQL.Query
    exposing
        ( Query
        , field
        , object
        , alias
        , toQueryString
        , toNamedQueryString
        )

import GraphQL.Arguments exposing (..)


type Query
    = Leaf String (List Argument)
    | Node String (List Argument) (List Query)
    | Alias String Query


field : String -> List Argument -> Query
field name args =
    Leaf name args


object : String -> List Argument -> List Query -> Query
object name args fields =
    Node name args fields


alias : String -> Query -> Query
alias name query =
    case query of
        Alias _ _ ->
            query

        _ ->
            Alias name query


toQueryString : List Query -> String
toQueryString queries =
    "{ " ++ (String.join ("\n") <| List.map innerQuery queries) ++ " }"


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
