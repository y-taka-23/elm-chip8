module Cpu exposing
    ( Cpu
    , Register
    , continue
    , decode
    , execute
    , executeKey
    , executeRand
    , fetch
    , init
    , view
    )

import Dict exposing (Dict)
import Display exposing (Display)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Memory exposing (Address, Memory)
import Memory.Word as Word exposing (Nibble(..), Word)
import Process
import Random
import Task


type Cpu
    = Cpu
        { step : Int
        , pc : Address
        , index : Address
        , registers : Registers
        , sp : Nibble
        , stack : Stack
        , delayTimer : Word
        , soundTimer : Word
        , waitingKey : Maybe Register
        }


type Register
    = Register Int


type Registers
    = Registers (Dict Int Word)


read : Register -> Registers -> Word
read (Register x) (Registers regs) =
    Maybe.withDefault Word.undefined <| Dict.get x regs


write : Register -> Word -> Registers -> Registers
write (Register x) w (Registers regs) =
    Registers <| Dict.insert x w regs


type Stack
    = Stack (Dict Int Address)


ith : Nibble -> Stack -> Address
ith (Nibble i) (Stack stack) =
    Maybe.withDefault (Tuple.second Memory.init) <| Dict.get i stack


push : Address -> ( Nibble, Stack ) -> ( Nibble, Stack )
push addr ( Nibble sp, Stack stack ) =
    ( Nibble <| sp + 1, Stack <| Dict.insert sp addr stack )


pop : Nibble -> Stack -> ( Nibble, Address )
pop (Nibble sp) (Stack stack) =
    ( Nibble <| sp - 1
    , Maybe.withDefault (Tuple.second Memory.init) <| Dict.get (sp - 1) stack
    )


type Instruction
    = Clear
    | Return
    | Jump Address
    | Call Address
    | SkipEq Register Word
    | SkipNeq Register Word
    | SkipRegEq Register Register
    | Load Register Word
    | Add Register Word
    | Move Register Register
    | Or Register Register
    | And Register Register
    | Xor Register Register
    | AddReg Register Register
    | SubReg Register Register
    | ShiftR Register Register
    | ShiftL Register Register
    | SkipRegNeq Register Register
    | LoadIdx Address
    | JumpIdx Address
    | Rand Register Word
    | Draw Register Register Nibble
    | SkipPress Register
    | SkipUp Register
    | MoveDelay Register
    | KeyDelay Register
    | LoadDelay Register
    | LoadSound Register
    | AddIdx Register
    | LoadSprite Register
    | Bcd Register
    | Store Register
    | Read Register
    | Unknown


init : Cpu
init =
    Cpu
        { step = 0
        , pc = Tuple.second Memory.init
        , index = Tuple.second Memory.init
        , registers = Registers Dict.empty
        , sp = Nibble 0
        , stack = Stack Dict.empty
        , delayTimer = Word.undefined
        , soundTimer = Word.undefined
        , waitingKey = Nothing
        }


fromNibble : Nibble -> Register
fromNibble (Nibble x) =
    Register x


fetch : Cpu -> Memory -> ( Word, Word )
fetch (Cpu cpu) mem =
    ( Memory.read cpu.pc mem
    , Memory.read (Memory.next cpu.pc) mem
    )


decode : ( Word, Word ) -> Instruction
decode ( w1, w2 ) =
    case ( Word.toNibbles w1, Word.toNibbles w2 ) of
        ( ( Nibble 0x00, Nibble 0x00 ), ( Nibble 0x0E, Nibble 0x00 ) ) ->
            Clear

        ( ( Nibble 0x00, Nibble 0x00 ), ( Nibble 0x0E, Nibble 0x0E ) ) ->
            Return

        ( ( Nibble 0x01, n2 ), ( n3, n4 ) ) ->
            Jump (Memory.fromNibble n2 n3 n4)

        ( ( Nibble 0x02, n2 ), ( n3, n4 ) ) ->
            Call (Memory.fromNibble n2 n3 n4)

        ( ( Nibble 0x03, n2 ), ( _, _ ) ) ->
            SkipEq (fromNibble n2) w2

        ( ( Nibble 0x04, n2 ), ( _, _ ) ) ->
            SkipNeq (fromNibble n2) w2

        ( ( Nibble 0x05, n2 ), ( n3, Nibble 0x00 ) ) ->
            SkipRegEq (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x06, n2 ), ( _, _ ) ) ->
            Load (fromNibble n2) w2

        ( ( Nibble 0x07, n2 ), ( _, _ ) ) ->
            Add (fromNibble n2) w2

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x00 ) ) ->
            Move (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x01 ) ) ->
            Or (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x02 ) ) ->
            And (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x03 ) ) ->
            Xor (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x04 ) ) ->
            AddReg (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x05 ) ) ->
            SubReg (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x06 ) ) ->
            ShiftR (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x08, n2 ), ( n3, Nibble 0x0E ) ) ->
            ShiftL (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x09, n2 ), ( n3, Nibble 0x00 ) ) ->
            SkipRegNeq (fromNibble n2) (fromNibble n3)

        ( ( Nibble 0x0A, n2 ), ( n3, n4 ) ) ->
            LoadIdx (Memory.fromNibble n2 n3 n4)

        ( ( Nibble 0x0B, n2 ), ( n3, n4 ) ) ->
            JumpIdx (Memory.fromNibble n2 n3 n4)

        ( ( Nibble 0x0C, n2 ), _ ) ->
            Rand (fromNibble n2) w2

        ( ( Nibble 0x0D, n2 ), ( n3, n4 ) ) ->
            Draw (fromNibble n2) (fromNibble n3) n4

        ( ( Nibble 0x0E, n2 ), ( Nibble 0x09, Nibble 0x0E ) ) ->
            SkipPress (fromNibble n2)

        ( ( Nibble 0x0E, n2 ), ( Nibble 0x0A, Nibble 0x01 ) ) ->
            SkipUp (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x00, Nibble 0x07 ) ) ->
            MoveDelay (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x00, Nibble 0x0A ) ) ->
            KeyDelay (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x01, Nibble 0x05 ) ) ->
            LoadDelay (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x01, Nibble 0x08 ) ) ->
            LoadSound (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x01, Nibble 0x0E ) ) ->
            AddIdx (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x02, Nibble 0x09 ) ) ->
            LoadSprite (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x03, Nibble 0x03 ) ) ->
            Bcd (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x05, Nibble 0x05 ) ) ->
            Store (fromNibble n2)

        ( ( Nibble 0x0F, n2 ), ( Nibble 0x06, Nibble 0x05 ) ) ->
            Read (fromNibble n2)

        _ ->
            Unknown


execute :
    (Register -> Word -> Word -> msg)
    -> Cmd msg
    -> ( Cpu, Memory, Display )
    -> Instruction
    -> ( ( Cpu, Memory, Display ), Cmd msg )
execute onRand cont ( cpu, mem, disp ) inst =
    case inst of
        Clear ->
            ( ( next cpu, mem, Display.init ), cont )

        Return ->
            ( ( return cpu, mem, disp ), cont )

        Jump addr ->
            ( ( jump addr cpu, mem, disp ), cont )

        Call addr ->
            ( ( call addr cpu, mem, disp ), cont )

        SkipEq reg cond ->
            if getRegister reg cpu == cond then
                ( ( skip cpu, mem, disp ), cont )

            else
                ( ( next cpu, mem, disp ), cont )

        SkipNeq reg cond ->
            if getRegister reg cpu /= cond then
                ( ( skip cpu, mem, disp ), cont )

            else
                ( ( next cpu, mem, disp ), cont )

        Load reg w ->
            ( ( next <| setRegister reg w cpu, mem, disp ), cont )

        Add reg w ->
            ( ( next <| accRegister reg w cpu, mem, disp ), cont )

        Move regX regY ->
            ( ( next <| moveRegister regX regY cpu, mem, disp ), cont )

        And regX regY ->
            ( ( next <| andRegister regX regY cpu, mem, disp ), cont )

        AddReg regX regY ->
            ( ( next <| addRegister regX regY cpu, mem, disp ), cont )

        SubReg regX regY ->
            ( ( next <| subRegister regX regY cpu, mem, disp ), cont )

        LoadIdx addr ->
            ( ( next <| setIndex addr cpu, mem, disp ), cont )

        Rand reg mask ->
            ( ( cpu, mem, disp )
            , Random.generate (onRand reg mask) Word.random
            )

        Draw regX regY size ->
            let
                ( newCpu, newDisp ) =
                    drawSprite regX regY size ( cpu, mem, disp )
            in
            ( ( next newCpu, mem, newDisp ), cont )

        KeyDelay reg ->
            ( ( waitKey reg cpu, mem, disp ), Cmd.none )

        AddIdx reg ->
            ( ( next <| addIndex reg cpu, mem, disp ), cont )

        Bcd reg ->
            ( ( next cpu, dumpBcd reg ( cpu, mem ), disp ), cont )

        Store reg ->
            ( ( next cpu, dumpRegisters reg ( cpu, mem ), disp ), cont )

        Read reg ->
            ( ( next <| loadRegisters reg ( cpu, mem ), mem, disp ), cont )

        _ ->
            ( ( cpu, mem, disp ), Cmd.none )


continue : Bool -> msg -> Cmd msg
continue isRunning msg =
    if isRunning then
        Process.sleep 0 |> Task.perform (always msg)

    else
        Cmd.none


executeRand : Register -> Word -> Word -> Cpu -> Cpu
executeRand reg mask rand cpu =
    next <| setRegister reg (Word.and mask rand) cpu


executeKey : Nibble -> Cpu -> Cpu
executeKey key cpu =
    let
        ( newCpu, reg ) =
            unwaitKey cpu
    in
    next <| setRegister reg (Word.fromNibble key) newCpu


setIndex : Address -> Cpu -> Cpu
setIndex addr (Cpu cpu) =
    Cpu { cpu | index = addr }


getIndex : Cpu -> Address
getIndex (Cpu cpu) =
    cpu.index


addIndex : Register -> Cpu -> Cpu
addIndex reg cpu =
    let
        ( addr, x ) =
            ( getIndex cpu, getRegister reg cpu )

        ( newIdx, carry ) =
            Memory.add addr <| Memory.fromWord x
    in
    setRegister (Register 0x0F) (Word.fromFlag carry) <| setIndex newIdx cpu


setRegister : Register -> Word -> Cpu -> Cpu
setRegister reg w (Cpu cpu) =
    Cpu { cpu | registers = write reg w cpu.registers }


getRegister : Register -> Cpu -> Word
getRegister reg (Cpu cpu) =
    read reg cpu.registers


moveRegister : Register -> Register -> Cpu -> Cpu
moveRegister regX regY cpu =
    setRegister regX (getRegister regY cpu) cpu


accRegister : Register -> Word -> Cpu -> Cpu
accRegister reg w cpu =
    let
        sum =
            Tuple.first <| Word.add w <| getRegister reg cpu
    in
    setRegister reg sum cpu


addRegister : Register -> Register -> Cpu -> Cpu
addRegister regX regY cpu =
    let
        ( sum, carry ) =
            Word.add (getRegister regX cpu) (getRegister regY cpu)
    in
    setRegister (Register 0x0F) (Word.fromFlag carry) <|
        setRegister regX sum cpu


subRegister : Register -> Register -> Cpu -> Cpu
subRegister regX regY cpu =
    let
        ( diff, notBorrow ) =
            Word.sub (getRegister regX cpu) (getRegister regY cpu)
    in
    setRegister (Register 0x0F) (Word.fromFlag notBorrow) <|
        setRegister regX diff cpu


andRegister : Register -> Register -> Cpu -> Cpu
andRegister regX regY cpu =
    let
        cap =
            Word.and (getRegister regX cpu) (getRegister regY cpu)
    in
    setRegister regX cap cpu


setFlag : Bool -> Cpu -> Cpu
setFlag flag cpu =
    setRegister (Register 0x0F) (Word.fromFlag flag) cpu


waitKey : Register -> Cpu -> Cpu
waitKey reg (Cpu cpu) =
    Cpu { cpu | waitingKey = Just reg }


unwaitKey : Cpu -> ( Cpu, Register )
unwaitKey (Cpu cpu) =
    ( Cpu { cpu | waitingKey = Nothing }
    , Maybe.withDefault (Register 0x00) cpu.waitingKey
    )


drawSprite :
    Register
    -> Register
    -> Nibble
    -> ( Cpu, Memory, Display )
    -> ( Cpu, Display )
drawSprite regX regY size ( cpu, mem, disp ) =
    let
        sprite =
            Memory.readSprite size (getIndex cpu) mem

        ( x, y ) =
            ( getRegister regX cpu, getRegister regY cpu )

        ( newDisp, coll ) =
            Display.draw (Word.toCoordinate x y) sprite disp
    in
    ( setFlag coll cpu, newDisp )


dumpBcd : Register -> ( Cpu, Memory ) -> Memory
dumpBcd reg ( cpu, mem ) =
    let
        ( w1, w2, w3 ) =
            Word.toBcd <| getRegister reg cpu
    in
    Memory.writeChunk (getIndex cpu) [ w1, w2, w3 ] mem


dumpRegisters : Register -> ( Cpu, Memory ) -> Memory
dumpRegisters (Register reg) ( cpu, mem ) =
    let
        words =
            List.map (\r -> getRegister r cpu) <|
                List.map Register <|
                    List.range 0x00 reg
    in
    Memory.writeChunk (getIndex cpu) words mem


loadRegisters : Register -> ( Cpu, Memory ) -> Cpu
loadRegisters (Register reg) ( cpu, mem ) =
    let
        words =
            Memory.readChunk (Nibble (reg + 1)) (getIndex cpu) mem
    in
    List.foldl (\( r, w ) c -> setRegister r w c) cpu <|
        List.indexedMap (\i w -> ( Register i, w )) words


nextPc : Address -> Address
nextPc =
    Memory.next << Memory.next


next : Cpu -> Cpu
next (Cpu cpu) =
    Cpu { cpu | step = cpu.step + 1, pc = nextPc cpu.pc }


skip : Cpu -> Cpu
skip (Cpu cpu) =
    Cpu { cpu | step = cpu.step + 1, pc = (nextPc << nextPc) cpu.pc }


jump : Address -> Cpu -> Cpu
jump addr (Cpu cpu) =
    Cpu { cpu | step = cpu.step + 1, pc = addr }


call : Address -> Cpu -> Cpu
call addr (Cpu cpu) =
    let
        ( newSp, newStack ) =
            push cpu.pc ( cpu.sp, cpu.stack )
    in
    Cpu { cpu | pc = addr, sp = newSp, stack = newStack }


return : Cpu -> Cpu
return (Cpu cpu) =
    let
        ( newSp, newPc ) =
            pop cpu.sp cpu.stack
    in
    next <| Cpu { cpu | pc = newPc, sp = newSp }


view : Cpu -> Html msg
view (Cpu cpu) =
    div [ id "cpu", class "pane" ]
        [ div [ id "step", class "column" ]
            [ stretchCell "Step Count" <| text <| String.fromInt cpu.step ]
        , div [ class "raw" ]
            [ div [ id "register", class "column" ]
                [ div [ class "raw" ]
                    [ stretchCell "PC" <| Memory.viewAddress cpu.pc
                    , stretchCell "I" <| Memory.viewAddress cpu.index
                    ]
                , div [ class "raw" ]
                    [ stretchCell "DT" <| Word.view cpu.delayTimer
                    , stretchCell "ST" <| Word.view cpu.soundTimer
                    ]
                , div [ class "raw" ]
                    [ unitCell "V0" (Register 0x00) cpu.registers
                    , unitCell "V1" (Register 0x01) cpu.registers
                    , unitCell "V2" (Register 0x02) cpu.registers
                    , unitCell "V3" (Register 0x03) cpu.registers
                    ]
                , div [ class "raw" ]
                    [ unitCell "V4" (Register 0x04) cpu.registers
                    , unitCell "V5" (Register 0x05) cpu.registers
                    , unitCell "V6" (Register 0x06) cpu.registers
                    , unitCell "V7" (Register 0x07) cpu.registers
                    ]
                , div [ class "raw" ]
                    [ unitCell "V8" (Register 0x08) cpu.registers
                    , unitCell "V9" (Register 0x09) cpu.registers
                    , unitCell "VA" (Register 0x0A) cpu.registers
                    , unitCell "VB" (Register 0x0B) cpu.registers
                    ]
                , div [ class "raw" ]
                    [ unitCell "VC" (Register 0x0C) cpu.registers
                    , unitCell "VD" (Register 0x0D) cpu.registers
                    , unitCell "VE" (Register 0x0E) cpu.registers
                    , unitCell "VF" (Register 0x0F) cpu.registers
                    ]
                ]
            , div [ id "stack", class "column" ] <|
                stackPointer cpu.sp
                    :: (List.map (\i -> stackSlot (Nibble i) cpu.stack) <|
                            List.range 0 0x0F
                       )
            ]
        ]


stretchCell : String -> Html msg -> Html msg
stretchCell label val =
    div [ class "cell", class "stretch" ]
        [ div [] [ text label ], div [] [ val ] ]


unitCell : String -> Register -> Registers -> Html msg
unitCell label reg regs =
    let
        val =
            Word.view <| read reg regs
    in
    div [ class "cell" ]
        [ div [] [ text label ], div [] [ val ] ]


stackPointer : Nibble -> Html msg
stackPointer (Nibble i) =
    div [ class "raw" ]
        [ div [ class "cell" ] [ text "SP" ]
        , div [ class "cell" ] [ text <| String.fromInt i ]
        ]


stackSlot : Nibble -> Stack -> Html msg
stackSlot (Nibble i) stack =
    div [ class "raw" ]
        [ div [ class "cell" ] [ text <| "Depth " ++ String.fromInt i ]
        , div [ class "cell" ]
            [ Memory.viewAddress <| ith (Nibble i) stack ]
        ]
