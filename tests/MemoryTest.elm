module MemoryTest exposing (suite)

import Expect
import Fuzz
import Memory
import Memory.Word as Word exposing (Nibble(..))
import Test exposing (Test, describe, fuzz2, fuzz3)


suite : Test
suite =
    describe "Memory"
        [ describe "add"
            [ fuzz3
                Memory.fuzzer
                Memory.fuzzer
                Memory.fuzzer
                "should be accosiative"
              <|
                \addr1 addr2 addr3 ->
                    let
                        sum =
                            \a1 a2 -> Tuple.first <| Memory.add a1 a2
                    in
                    sum addr1 (sum addr2 addr3)
                        |> Expect.equal
                            (sum (sum addr1 addr2) addr3)
            , fuzz2
                Memory.fuzzer
                Memory.fuzzer
                "should be commutative"
              <|
                \addr1 addr2 ->
                    Memory.add addr1 addr2
                        |> Expect.equal (Memory.add addr2 addr1)
            ]
        , describe "readChunk"
            [ fuzz2
                Memory.fuzzer
                (Fuzz.list Word.fuzzer)
                "should be the right inverse of writeChunk"
              <|
                \addr ws ->
                    let
                        initMemory =
                            Tuple.first Memory.init
                    in
                    Memory.writeChunk addr ws initMemory
                        |> Memory.readChunk (Nibble <| List.length ws) addr
                        |> Expect.equal ws
            ]
        ]
