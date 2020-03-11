module Cpu exposing (Cpu, decode, execute, fetch, init)

import Memory exposing (Address, Memory, Word)


type Cpu
    = Cpu { pc : Address }


type Instruction
    = Unknown


init : Cpu
init =
    Cpu { pc = Tuple.second Memory.init }


fetch : Cpu -> Memory -> ( Word, Word )
fetch (Cpu cpu) mem =
    ( Memory.read cpu.pc mem
    , Memory.read (Memory.next cpu.pc) mem
    )


decode : ( Word, Word ) -> Instruction
decode ( _, _ ) =
    Unknown


execute : Cpu -> Memory -> Instruction -> ( Cpu, Memory )
execute (Cpu cpu) mem _ =
    ( Cpu cpu, mem )
