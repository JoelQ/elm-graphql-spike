module GraphQL.Arguments exposing (Argument, string, union, toKeyValueStrings)


type Argument
    = StringArg String String
    | UnionArg String String


string : String -> String -> Argument
string =
    StringArg


union : String -> String -> Argument
union =
    UnionArg


toKeyValueString : Argument -> String
toKeyValueString arg =
    case arg of
        StringArg name value ->
            name ++ ": \"" ++ value ++ "\""

        UnionArg name value ->
            name ++ ": " ++ value


toKeyValueStrings : List Argument -> String
toKeyValueStrings args =
    case args of
        [] ->
            ""

        _ ->
            "(" ++ (String.join ", " <| List.map toKeyValueString args) ++ ")"
