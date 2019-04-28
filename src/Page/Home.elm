module Page.Home exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, for, id, title, type_, value)
import Html.Styled.Events exposing (onBlur, onClick, onFocus, onInput)
import Person exposing (PersonForm)
import Task



-- MODEL


type alias Model =
    { form : PersonForm, validations : Person.Validations }


init : Model
init =
    { form = PersonForm Nothing Nothing Nothing Nothing Nothing Nothing
    , validations = initValidations
    }


initValidations : Person.Validations
initValidations =
    Person.Validations Nothing Nothing Nothing Nothing Nothing Nothing



-- VIEW


view : Model -> { title : String, body : List (Html.Html Msg) }
view model =
    { title = "Form Decoding \u{1F991}"
    , body =
        [ h1 [] [ text "Form Decoding \u{1F991}" ]
        , section []
            [ h2 [] [ text "Contact information" ]
            , p []
                [ span [ css [ display inlineBlock ] ]
                    [ label [ for "given-name", css [ display block ] ]
                        [ span [] [ text "given name " ]
                        , strong [] [ abbr [ title "required" ] [ text "*" ] ]
                        ]
                    , input
                        [ type_ "text"
                        , id "given-name"
                        , onInput EnteredGivenName
                        , value (model.form.givenName |> Maybe.withDefault "")
                        , onBlur Validate
                        , onFocus <| EnteredGivenName ""
                        ]
                        []
                    , model.validations.givenName
                        |> Maybe.andThen resultToMaybeError
                        |> Maybe.map (div [] << List.singleton << text << errorView)
                        |> Maybe.withDefault (text "")
                    ]
                , span [ css [ display inlineBlock ] ]
                    [ label [ for "family-name", css [ display block ] ]
                        [ span [] [ text "family name " ]
                        , strong [] [ abbr [ title "required" ] [ text "*" ] ]
                        ]
                    , input
                        [ type_ "text"
                        , id "family-name"
                        , onInput EnteredFamilyName
                        , value (model.form.familyName |> Maybe.withDefault "")
                        ]
                        []
                    ]
                ]
            , p []
                [ label [ for "mail", css [ display block ] ]
                    [ span [] [ text "e-mail address " ]
                    , strong [] [ abbr [ title "required" ] [ text "*" ] ]
                    ]
                , input [ type_ "email", id "mail" ] []
                ]
            , p []
                [ label [ for "phone", css [ display block ] ]
                    [ span [] [ text "phone number " ]
                    , strong [] [ abbr [ title "required" ] [ text "*" ] ]
                    ]
                , areaCodeInput
                , input [ type_ "tel", id "phone" ] []
                ]
            , p []
                [ label [ for "zip", css [ display block ] ]
                    [ span [] [ text "postal code " ]
                    , strong [] [ abbr [ title "required" ] [ text "*" ] ]
                    ]
                , input [ type_ "number", id "zip" ] []
                ]
            ]
        ]
            |> List.map toUnstyled
    }


errorView : Person.Error -> String
errorView errorPerson =
    case errorPerson of
        Person.Required ->
            "This field is required."

        Person.InvalidCharacters string ->
            "You have invalid characters: " ++ string

        Person.InvalidFormat string ->
            "invalid format."

        Person.NumberTooShort int ->
            "number too short: " ++ String.fromInt int

        Person.NumberTooLong int ->
            "number too long: " ++ String.fromInt int


resultToMaybeError : Result x a -> Maybe x
resultToMaybeError result =
    case result of
        Ok a ->
            Nothing

        Err x ->
            Just x


areaCodeInput : Html msg
areaCodeInput =
    select []
        [ option [ value "+7" ] [ text "+7 (Russia)" ]
        , option [ value "+46" ] [ text "+46 (Sweden)" ]
        , option [ value "+47" ] [ text "+47 (Norway)" ]
        , option [ value "+358" ] [ text "+358 (Finland)" ]
        , option [ value "+372" ] [ text "+372 (Estonia)" ]
        , option [ value "other" ] [ text "other" ]
        ]



-- UPDATE


type Msg
    = EnteredGivenName String
    | EnteredFamilyName String
    | Validate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredGivenName string ->
            ( { model
                | form = model.form |> (\a -> { a | givenName = Just string })
              }
            , Cmd.none
            )

        EnteredFamilyName string ->
            ( { model
                | form = model.form |> (\a -> { a | familyName = Just string })
              }
            , Cmd.none
            )

        Validate ->
            case Person.fromForm model.form of
                Result.Ok person ->
                    let
                        _ =
                            Debug.log "person" person
                    in
                    ( { model | validations = initValidations }, Cmd.none )

                Result.Err validations ->
                    ( { model | validations = validations }, Cmd.none )
