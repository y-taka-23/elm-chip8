module Memory.WordTest exposing (suite)

import Expect
import Fuzz
import List.Extra as List
import Memory.Word as Word
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "Memory.Word"
        [ describe "add"
            [ fuzz3
                Word.fuzzer
                Word.fuzzer
                Word.fuzzer
                "should be accosiative"
              <|
                \w1 w2 w3 ->
                    Word.add w1 (Word.add w2 w3)
                        |> Expect.equal (Word.add (Word.add w1 w2) w3)
            , fuzz2
                Word.fuzzer
                Word.fuzzer
                "should be commutative"
              <|
                \w1 w2 ->
                    Word.add w1 w2
                        |> Expect.equal (Word.add w2 w1)
            ]
        , describe "and"
            [ fuzz3
                Word.fuzzer
                Word.fuzzer
                Word.fuzzer
                "should be accosiative"
              <|
                \w1 w2 w3 ->
                    Word.and w1 (Word.and w2 w3)
                        |> Expect.equal (Word.and (Word.and w1 w2) w3)
            , fuzz2
                Word.fuzzer
                Word.fuzzer
                "should be commutative"
              <|
                \w1 w2 ->
                    Word.and w1 w2
                        |> Expect.equal (Word.and w2 w1)
            , fuzz
                Word.fuzzer
                "should be idempotent"
              <|
                \w ->
                    Word.and w w
                        |> Expect.equal w
            ]
        , describe "undefined"
            [ fuzz
                Word.fuzzer
                "should be the left unit of add"
              <|
                \w ->
                    Word.add Word.undefined w
                        |> Expect.equal w
            , fuzz
                Word.fuzzer
                "should be the right unit of add"
              <|
                \w ->
                    Word.add w Word.undefined
                        |> Expect.equal w
            , fuzz
                Word.fuzzer
                "should be the left zero of and"
              <|
                \w ->
                    Word.and Word.undefined w
                        |> Expect.equal Word.undefined
            , fuzz
                Word.fuzzer
                "should be the right zero of and"
              <|
                \w ->
                    Word.and w Word.undefined
                        |> Expect.equal Word.undefined
            ]
        , describe "fromFlag"
            [ test
                "should not degenerate"
              <|
                \_ ->
                    Word.fromFlag True
                        |> Expect.notEqual (Word.fromFlag False)
            ]
        , describe "toCoordinate"
            [ fuzz2
                (Fuzz.tuple ( Word.fuzzer, Word.fuzzer ))
                (Fuzz.tuple ( Word.fuzzer, Word.fuzzer ))
                "should not degenerate"
              <|
                \( w1, w2 ) ( w3, w4 ) ->
                    if w1 /= w3 || w2 /= w4 then
                        Word.toCoordinate w1 w2
                            |> Expect.notEqual (Word.toCoordinate w3 w4)

                    else
                        Expect.pass
            ]
        , describe "toNibbles"
            [ fuzz2
                Word.fuzzer
                Word.fuzzer
                "should not degenerate"
              <|
                \w1 w2 ->
                    if w1 /= w2 then
                        Word.toNibbles w1
                            |> Expect.notEqual (Word.toNibbles w2)

                    else
                        Expect.pass
            ]
        , describe "toSprite"
            [ fuzz2
                (Fuzz.list Word.fuzzer)
                (Fuzz.list Word.fuzzer)
                "should not degenerate"
              <|
                \ws1 ws2 ->
                    if ws1 /= ws2 then
                        Word.toSprite ws1
                            |> Expect.notEqual (Word.toSprite ws2)

                    else
                        Expect.pass
            , fuzz
                (Fuzz.list Word.fuzzer)
                "should preserve the number of lines"
              <|
                \ws ->
                    Word.toSprite ws
                        |> List.length
                        |> Expect.equal (List.length ws)
            , fuzz2
                Fuzz.int
                (Fuzz.list Word.fuzzer)
                "should convert each line into eight dots"
              <|
                \i ws ->
                    Word.toSprite ws
                        |> List.getAt i
                        |> Maybe.map List.length
                        |> Maybe.withDefault 8
                        |> Expect.equal 8
            , fuzz2
                Fuzz.int
                (Fuzz.list Word.fuzzer)
                "should be commutative with the index access"
              <|
                \i ws ->
                    let
                        convertThenPick =
                            Word.toSprite ws
                                |> List.getAt i
                                |> Maybe.map List.singleton

                        pickThenConvert =
                            List.getAt i ws
                                |> Maybe.map List.singleton
                                |> Maybe.map Word.toSprite
                    in
                    convertThenPick
                        |> Expect.equal pickThenConvert
            ]
        ]
