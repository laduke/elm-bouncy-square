module Main exposing (..)

import Svg exposing (Svg, ellipse, rect, svg, text)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Html
import Html
import Debug exposing (log)


--import Debug exposing (log)

import Keyboard
import AnimationFrame


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


init : ( Model, Cmd b )
init =
    ( marker ( 90, 90 ) ( 2, 2 ) ( 400, 400 ) ( 20, 20 ) "green"
    , Cmd.none
    )


type alias Model =
    { xy : Position
    , speed : Speed
    , bounds : Position
    , size : Size
    , color : String
    }


type alias Position =
    ( Int, Int )


type alias Size =
    ( Int, Int )


type alias Speed =
    ( Int, Int )


marker : Position -> Speed -> Position -> Size -> String -> Model
marker position speed bounds size color =
    Model position speed bounds size color



-- UPDATE


type Msg
    = KeyMsg Keyboard.KeyCode
    | TimeUpdate Time


newPosition : Position -> Speed -> Position
newPosition pos speed =
    let
        ( oldX, oldY ) =
            pos

        ( speedX, speedY ) =
            speed
    in
        (,) (oldX + speedX) (oldY + speedY)


clamper : Model -> Model
clamper model =
    let
        ( x, y ) =
            model.xy

        ( bx, by ) =
            model.bounds

        ( sX, sY ) =
            model.size
    in
        { model
            | xy = ( Basics.clamp 0 (bx - sX) x, Basics.clamp 0 (by - sY) y )
        }


killSpeed : Int -> Bool -> Int
killSpeed speed touching =
    if touching then
        speed * -1
    else
        speed


boundsCheck : Model -> Model
boundsCheck model =
    let
        ( x, y ) =
            model.xy

        ( sizeX, sizeY ) =
            model.size

        ( speedX, speedY ) =
            model.speed

        ( boundsX, boundsY ) =
            model.bounds

        touchingX =
            x <= sizeX || (x + sizeX) >= boundsX

        touchingY =
            y <= sizeY || (y + sizeY) >= boundsY
    in
        { model | speed = ( killSpeed speedX touchingX, killSpeed speedY touchingY ) }


applyPhysics : Float -> Model -> Model
applyPhysics dt model =
    let
        ( x, y ) =
            model.xy

        ( speedX, speedY ) =
            model.speed
    in
        { model | xy = ( (x + (speedX * Basics.round dt) // 20), (y + (speedY * Basics.round dt) // 20) ) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( model
                |> (applyPhysics dt)
                |> clamper
                |> boundsCheck
            , Cmd.none
            )

        --( touching, Cmd.none )
        KeyMsg code ->
            let
                ( x, y ) =
                    model.speed
            in
                case code of
                    37 ->
                        ( { model | speed = ( x - 2, y ) }, Cmd.none )

                    38 ->
                        ( { model | speed = ( x, y - 2 ) }, Cmd.none )

                    39 ->
                        ( { model | speed = ( x + 2, y ) }, Cmd.none )

                    40 ->
                        ( { model | speed = ( x, y + 2 ) }, Cmd.none )

                    _ ->
                        log (toString model)
                            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ --Time.every second Tick
          AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyMsg
        ]



-- VIEW


view : Model -> Svg Msg
view model =
    let
        ( boundsX, boundsY ) =
            model.bounds

        boundaryX =
            toString (boundsX)

        boundaryY =
            toString (boundsY)
    in
        svg
            [ version "1.1"
            , width boundaryX
            , height (toString (boundsY + 100))
            , viewBox ("0 0 " ++ boundaryX ++ " " ++ toString (boundsY + 100))
            ]
            [ rect
                [ width boundaryX
                , height boundaryY
                , fill "grey"
                  --, onClick Move
                ]
                []
            , svg
                []
                [ particle model ]
            , svg []
                [ scoreRect model ]
            ]


scoreRect : Model -> Svg Msg
scoreRect model =
    let
        ( speedX, speedY ) =
            model.speed

        msg =
            "x/y: "
                ++ (speedX |> abs |> toString)
                ++ "/"
                ++ (speedY |> abs |> toString)
    in
        Svg.text_
            [ x "300"
            , y "420"
            , width "200"
            , height "30"
            , fontSize "16"
            ]
            [ Html.text msg ]


particle : Model -> Svg Msg
particle marker =
    let
        ( posX, posY ) =
            marker.xy

        xStr =
            toString posX

        yStr =
            toString posY

        ( sX, sY ) =
            marker.size

        ( speedX, speedY ) =
            marker.speed
    in
        ellipse
            [ fill marker.color
            , cx xStr
            , cy yStr
            -- , rx (toString ( sX - (abs speedX ) ))
            -- , ry (toString ( sY - (abs speedY ) ))
            , rx (toString sX )
            , ry (toString sY )
            ]
            []
