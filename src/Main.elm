module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as EE
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA
import Random
import Task
import Time


type alias Model =
    { grid : Grid
    , size : Int
    , seed : Random.Seed
    , debug : Bool
    }


type alias Grid =
    Dict String Square


type alias Square =
    { color : SquareColor
    , neighbors : Int
    , state : UserChoice
    , fixed : Bool
    }


defaultSquare : Square
defaultSquare =
    { color = Red
    , neighbors = 0
    , state = Unmarked
    , fixed = True
    }


type SquareColor
    = Red
    | Blue


type UserChoice
    = Unmarked
    | Marked SquareColor


type Msg
    = ToggleSquare String
    | ToggleDebug
    | GotSeed Random.Seed


toggleSquare : Square -> Square
toggleSquare ({ state } as square) =
    { square
        | state =
            case state of
                Unmarked ->
                    Marked Blue

                Marked Blue ->
                    Marked Red

                Marked Red ->
                    Unmarked
    }


view : Model -> Html Msg
view { size, grid, debug } =
    layout
        [ height fill
        , width fill
        , Background.color black
        ]
    <|
        column [ width fill, height fill ]
            [ el
                [ Font.bold
                , Font.color white
                , Font.size 100
                , height <| px 100
                , padding 10
                , centerX
                , EE.onClick ToggleDebug
                ]
              <|
                text <|
                    coordsToLabel size size
            , el [ centerX, centerY ] <|
                viewGrid debug size grid
            ]


black : Color
black =
    rgb255 34 34 34


white : Color
white =
    rgb255 221 221 221


grey : Color
grey =
    rgb255 42 42 42


red : Color
red =
    rgb255 255 56 75


blue : Color
blue =
    rgb255 28 192 224


viewColor : UserChoice -> Color
viewColor userChoice =
    case userChoice of
        Unmarked ->
            grey

        Marked Blue ->
            blue

        Marked Red ->
            red


viewSquare : Bool -> String -> Square -> Element Msg
viewSquare debug label { neighbors, state, fixed, color } =
    let
        size =
            90

        fontSize =
            50

        c =
            viewColor <|
                if fixed || debug then
                    Marked color

                else
                    state
    in
    el
        [ Background.color c
        , EE.onClick <| ToggleSquare label
        , pointer
        , Border.rounded 20
        , width <| px <| size
        , height <| px <| size
        ]
    <|
        el
            [ centerX
            , centerY
            , Font.size fontSize
            , Font.color white
            , htmlAttribute <| HA.style "user-select" "none"
            ]
        <|
            text <|
                if fixed && color == Blue then
                    String.fromInt <|
                        neighbors

                else
                    "\u{00A0}"


viewGrid : Bool -> Int -> Grid -> Element Msg
viewGrid debug size grid =
    let
        list =
            List.range 1 size
    in
    list
        |> List.map
            (\x ->
                list
                    |> List.map
                        (\y ->
                            let
                                label =
                                    coordsToLabel x y

                                square =
                                    Dict.get label grid |> Maybe.withDefault defaultSquare
                            in
                            viewSquare debug label square
                        )
                    |> row [ spacing 10 ]
            )
        |> column [ spacing 10 ]


coordsToLabel : Int -> Int -> String
coordsToLabel x y =
    String.fromInt x ++ " x " ++ String.fromInt y


labelToCoords : String -> ( Int, Int )
labelToCoords label =
    label
        |> String.replace " " ""
        |> String.split "x"
        |> (\list ->
                case list of
                    x :: y :: _ ->
                        ( String.toInt x |> Maybe.withDefault 0, String.toInt y |> Maybe.withDefault 0 )

                    _ ->
                        ( 0, 0 )
           )


buildEmptyGrid : Int -> Grid
buildEmptyGrid size =
    let
        list =
            List.range 1 size
    in
    list
        |> List.foldl
            (\x acc ->
                list
                    |> List.foldl
                        (\y result ->
                            ( coordsToLabel x y, defaultSquare ) :: result
                        )
                        acc
            )
            []
        |> Dict.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDebug ->
            ( { model | debug = not model.debug }, Cmd.none )

        GotSeed seed ->
            let
                ( newSeed, grid ) =
                    model.grid
                        |> randomizeGrid seed
                        |> Tuple.mapSecond removeUnlinked
                        |> Tuple.mapSecond countNeighbors
            in
            ( { model
                | seed = newSeed
                , grid = grid
              }
            , Cmd.none
            )

        ToggleSquare label ->
            ( updateSquare toggleSquare label model, Cmd.none )


randomBool : Int -> Int -> Random.Generator Bool
randomBool falses trues =
    Random.int 1 (falses + trues)
        |> Random.map ((<) falses)


randomizeGrid : Random.Seed -> Grid -> ( Random.Seed, Grid )
randomizeGrid seed grid =
    grid
        |> Dict.foldl
            (\label square ( s, g ) ->
                let
                    ( isRed, nS ) =
                        Random.step (randomBool 3 2) s

                    ( isFixed, newSeed ) =
                        Random.step (randomBool 5 3) nS
                in
                ( newSeed
                , Dict.insert label
                    { square
                        | color =
                            if isRed then
                                Red

                            else
                                Blue
                        , fixed = isFixed
                    }
                    g
                )
            )
            ( seed, Dict.empty )


removeUnlinked : Grid -> Grid
removeUnlinked grid =
    grid
        |> Dict.map (removeUnlinkedSquare grid)


countNeighbors : Grid -> Grid
countNeighbors grid =
    grid
        |> Dict.map (countNeighborsForSquare grid)


getSquareAt : Grid -> Int -> Int -> Square
getSquareAt grid x y =
    Maybe.withDefault defaultSquare <|
        if x < 1 || y < 1 then
            Nothing

        else
            grid
                |> Dict.get (coordsToLabel x y)


countInDirectionFrom : Grid -> Int -> Int -> String -> Int -> Int
countInDirectionFrom grid xOffset yOffset label current =
    let
        ( x, y ) =
            labelToCoords label

        nextX =
            x + xOffset

        nextY =
            y + yOffset

        nextLabel =
            coordsToLabel nextX nextY

        nextSquare =
            getSquareAt grid nextX nextY
    in
    if nextSquare.color == Red then
        current

    else
        countInDirectionFrom grid xOffset yOffset nextLabel <| 1 + current


countNeighborsForSquare : Grid -> String -> Square -> Square
countNeighborsForSquare grid label square =
    let
        count =
            [ countInDirectionFrom grid 1 0 label 0
            , countInDirectionFrom grid -1 0 label 0
            , countInDirectionFrom grid 0 1 label 0
            , countInDirectionFrom grid 0 -1 label 0
            ]
                |> List.foldl (+) 0
    in
    { square
        | neighbors = count
        , color =
            if count == 0 then
                Red

            else
                square.color
    }


isLinkedInDirectionFrom : Grid -> Int -> Int -> String -> Bool
isLinkedInDirectionFrom grid xOffset yOffset label =
    let
        ( x, y ) =
            labelToCoords label

        nextX =
            x + xOffset

        nextY =
            y + yOffset

        nextLabel =
            coordsToLabel nextX nextY

        nextSquare =
            getSquareAt grid nextX nextY
    in
    if nextSquare.color == Red then
        False

    else if nextSquare.fixed then
        True

    else
        isLinkedInDirectionFrom grid xOffset yOffset nextLabel


removeUnlinkedSquare : Grid -> String -> Square -> Square
removeUnlinkedSquare grid label square =
    let
        isLinkedToFixedBlueNeighbor _ =
            [ isLinkedInDirectionFrom grid 1 0 label
            , isLinkedInDirectionFrom grid -1 0 label
            , isLinkedInDirectionFrom grid 0 1 label
            , isLinkedInDirectionFrom grid 0 -1 label
            ]
                |> List.foldl (||) False
    in
    { square
        | color =
            if square.color == Red then
                Red

            else if not <| isLinkedToFixedBlueNeighbor () then
                Red

            else
                Blue
    }


updateSquare : (Square -> Square) -> String -> Model -> Model
updateSquare fn label model =
    { model | grid = Dict.update label (Maybe.map fn) model.grid }


subs : Model -> Sub Msg
subs _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = buildEmptyGrid 7, size = 7, seed = Random.initialSeed 42, debug = False }
    , Time.now
        |> Task.map (Time.posixToMillis >> Random.initialSeed)
        |> Task.perform GotSeed
    )


main : Program () Model Msg
main =
    Browser.element <|
        { init = init
        , subscriptions = subs
        , update = update
        , view = view
        }
