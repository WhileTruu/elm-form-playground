module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Styled exposing (toUnstyled)
import Page.Home
import Route exposing (Route)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Model
    = Redirect Nav.Key
    | Home Nav.Key Page.Home.Model


toNavKey : Model -> Nav.Key
toNavKey model =
    case model of
        Home navKey _ ->
            navKey

        Redirect navKey ->
            navKey


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    changeRouteTo (Route.fromUrl url) (Redirect key)



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Page.Home.Msg


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        navKey =
            toNavKey model
    in
    case maybeRoute of
        Nothing ->
            ( model, Route.replaceUrl navKey Route.Home )

        Just Route.Home ->
            ( Page.Home.init, Cmd.none ) |> updateWith (Home navKey) GotHomeMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (toNavKey model) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home navKey subModel ) ->
            Page.Home.update subMsg subModel
                |> updateWith (Home navKey) GotHomeMsg

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage toMsg { title, body } =
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Home _ subModel ->
            Page.Home.view subModel |> viewPage GotHomeMsg

        _ ->
            { title = "", body = [ text "not found" ] }
