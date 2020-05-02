module Main exposing (main)

import Browser
import Browser.Events as Browser
import Bytes exposing (Bytes)
import Control exposing (Control)
import Cpu exposing (Cpu, Register)
import Display exposing (Display)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Keyboard
import Memory exposing (Memory)
import Memory.Word exposing (Nibble, Word)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = SelectRom
    | ExtractRom File
    | LoadRom Bytes
    | Run
    | Pause
    | Step
    | StepRand Register Word Word
    | PressKey Nibble
    | DownKey Nibble
    | UpKey Nibble
    | DecrTimers
    | Reset


type alias Model =
    { control : Control
    , display : Display
    , memory : Memory
    , cpu : Cpu
    , rom : Maybe File
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { control = Control.init
      , display = Display.init
      , memory = Tuple.first Memory.init
      , cpu = Cpu.init
      , rom = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectRom ->
            ( model, Select.file [] ExtractRom )

        ExtractRom file ->
            ( { model | rom = Just file }
            , Task.perform LoadRom (File.toBytes file)
            )

        LoadRom rom ->
            case Memory.loadRom rom of
                -- TODO: handle errors here
                Nothing ->
                    ( model, Cmd.none )

                Just mem ->
                    let
                        initModel =
                            Tuple.first <| init ()
                    in
                    ( { initModel | memory = mem, rom = model.rom }, Cmd.none )

        Run ->
            ( { model | control = Control.run model.control }
            , Cpu.continue True Step
            )

        Pause ->
            ( { model | control = Control.pause model.control }, Cmd.none )

        Step ->
            let
                ( ( newCpu, newMem, newDisp ), cmd ) =
                    Cpu.execute
                        StepRand
                        (Cpu.continue (Control.isRunning model.control) Step)
                        ( model.cpu, model.memory, model.display )
                    <|
                        Cpu.decode <|
                            Cpu.fetch model.cpu model.memory
            in
            ( { model | memory = newMem, cpu = newCpu, display = newDisp }
            , cmd
            )

        StepRand reg mask rand ->
            ( { model | cpu = Cpu.executeRand reg mask rand model.cpu }
            , Cpu.continue (Control.isRunning model.control) Step
            )

        PressKey key ->
            if Cpu.isWaiting model.cpu then
                ( { model | cpu = Cpu.executeKey key model.cpu }
                , Cpu.continue (Control.isRunning model.control) Step
                )

            else
                ( model, Cmd.none )

        DownKey key ->
            ( { model | cpu = Cpu.setKey key model.cpu }, Cmd.none )

        UpKey key ->
            ( { model | cpu = Cpu.unsetKey key model.cpu }, Cmd.none )

        DecrTimers ->
            ( { model | cpu = Cpu.decrTimers model.cpu }, Cmd.none )

        Reset ->
            let
                initModel =
                    Tuple.first <| init ()

                cmd =
                    Maybe.withDefault Cmd.none <|
                        Maybe.map (Task.perform LoadRom << File.toBytes)
                            model.rom
            in
            ( { initModel | rom = model.rom }, cmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.onAnimationFrame (always DecrTimers)


view : Model -> Html Msg
view model =
    div [ id "main-container" ]
        [ Cpu.view model.cpu
        , Keyboard.view PressKey DownKey UpKey
        , Display.view model.display
        , Control.view SelectRom Run Pause Step Reset model.control
        ]
