port module Sound exposing (emitTimer, start, stop)

import Json.Encode as E
import Memory.Word as Word exposing (Word)


port startWebAudio : () -> Cmd msg


port stopWebAudio : () -> Cmd msg


port sendTimerToWebAudio : E.Value -> Cmd msg


start : Cmd msg
start =
    startWebAudio ()


stop : Cmd msg
stop =
    stopWebAudio ()


emitTimer : Word -> Cmd msg
emitTimer =
    sendTimerToWebAudio << Word.toJson
