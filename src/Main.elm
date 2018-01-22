module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard exposing (KeyCode)
import Grid exposing (Grid, Tile, Direction(..))
import View
import Random


main : Platform.Program Basics.Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL --


type alias Model =
    { score : Int
    , grid : Grid
    }


init : ( Model, Cmd Msg )
init =
    let
        grid =
            Grid.make { w = 4, h = 4 }
    in
        ( Model 0 grid, Random.generate InitGrid (Grid.init grid) )



-- UPDATE --


type Msg
    = NewGame
    | InitGrid Grid
    | NewTile (Maybe Tile)
    | KeyDown KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Cmd.none )

        InitGrid grid ->
            ( { model | grid = grid }, Cmd.none )

        NewTile Nothing ->
            ( model, Cmd.none )

        NewTile (Just { pos, val }) ->
            ( { model | grid = (Grid.set pos val model.grid) |> Maybe.withDefault model.grid }, Cmd.none )

        KeyDown code ->
            case codeToDirection code of
                Just dir ->
                    let
                        moves =
                            model.grid |> Grid.moves dir

                        newScore =
                            model.score + Grid.score moves

                        newGrid =
                            model.grid |> Grid.apply moves

                        cmd =
                            if Grid.hasMoved moves then
                                newGrid |> Grid.generate NewTile
                            else
                                Cmd.none
                    in
                        ( { model | grid = newGrid, score = newScore }, cmd )

                Nothing ->
                    ( model, Cmd.none )


codeToDirection : KeyCode -> Maybe Direction
codeToDirection code =
    case code of
        37 ->
            Just Left

        38 ->
            Just Up

        39 ->
            Just Right

        40 ->
            Just Down

        _ ->
            Nothing



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown



-- VIEWS --


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ View.viewGrid model.grid
        , div [ class "score" ] [ model.score |> toString |> text ]
        ]
