module GraphQL.Arguments exposing (Argument, string, union, int, toKeyValueStrings)


type Argument
    = StringArg String String
    | IntArg String Int
    | UnionArg String String


int : String -> Int -> Argument
int =
    IntArg


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

        IntArg name value ->
            name ++ ": " ++ toString value

        UnionArg name value ->
            name ++ ": " ++ value


toKeyValueStrings : List Argument -> String
toKeyValueStrings args =
    case args of
        [] ->
            ""

        _ ->
            "(" ++ (String.join ", " <| List.map toKeyValueString args) ++ ")"
