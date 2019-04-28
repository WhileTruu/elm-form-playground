module Person exposing (Error(..), Person, PersonForm, Validations, fromForm)

import Regex exposing (Regex)


type alias Person =
    { givenName : Name
    , familyName : Name
    , email : Email
    , phoneNumber : PhoneNumber
    , postalCode : PostalCode
    }


type Name
    = Name String


type Email
    = Email String


type PhoneNumber
    = PhoneNumber AreaCode Int


type PostalCode
    = PostalCode Int


type AreaCode
    = Russia
    | Sweden
    | Norway
    | Finland
    | Estonia
    | Other Int



-- CREATION


type alias PersonForm =
    { givenName : Maybe String
    , familyName : Maybe String
    , email : Maybe String
    , areaCode : Maybe String
    , phone : Maybe String
    , postalCode : Maybe String
    }


fromForm : PersonForm -> Result Validations Person
fromForm form =
    let
        errors =
            { givenName =
                form.givenName |> Maybe.map validateName
            , familyName =
                form.familyName |> Maybe.map validateName
            , email = Nothing
            , areaCode = Nothing
            , phone = Nothing
            , postalCode = Nothing
            }
    in
    Maybe.map Person (errors.givenName |> Maybe.andThen Result.toMaybe)
        |> andThenMapMaybe (errors.familyName |> Maybe.andThen Result.toMaybe)
        |> andThenMapMaybe (errors.email |> Maybe.andThen Result.toMaybe)
        |> andThenMapMaybe
            (Maybe.map2
                (\a b -> PhoneNumber a b)
                (errors.areaCode |> Maybe.andThen Result.toMaybe)
                (errors.phone |> Maybe.andThen Result.toMaybe)
            )
        |> andThenMapMaybe (errors.postalCode |> Maybe.andThen Result.toMaybe)
        |> Maybe.map Result.Ok
        |> Maybe.withDefault (Result.Err errors)


andThenMapMaybe : Maybe a -> Maybe (a -> b) -> Maybe b
andThenMapMaybe a =
    Maybe.andThen (\func -> Maybe.map func a)



--Result.Err
--errors


validateName : String -> Result Error Name
validateName string =
    if String.isEmpty string then
        Result.Err Required

    else if Regex.contains nameRegex string then
        Result.Ok <| Name string

    else
        Result.Err <| InvalidCharacters (replaceValidNameCharacters string)


nameRegex : Regex
nameRegex =
    Regex.fromStringWith { multiline = False, caseInsensitive = True }
        "^[A-Za-z\\u00C0-\\u00FF][A-Za-z\\u00C0-\\u00FF-]+([ A-Za-z\\u00C0-\\u00FF][A-Za-z\\u00C0-\\u00FF-]+)*$"
        |> Maybe.withDefault Regex.never


replaceValidNameCharacters : String -> String
replaceValidNameCharacters =
    Regex.replace
        (Regex.fromString "[A-Za-z\\u00C0-\\u00FF- ]"
            |> Maybe.withDefault Regex.never
        )
        (\_ -> "")



-- ERRORS


type alias Validations =
    { givenName : Maybe (Result Error Name)
    , familyName : Maybe (Result Error Name)
    , email : Maybe (Result Error Email)
    , areaCode : Maybe (Result Error AreaCode)
    , phone : Maybe (Result Error Int)
    , postalCode : Maybe (Result Error Int)
    }


type Error
    = Required
    | InvalidCharacters String
    | InvalidFormat String
    | NumberTooShort Int
    | NumberTooLong Int
