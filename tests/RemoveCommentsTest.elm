module RemoveCommentsTest exposing (all)

import Test exposing (describe, test, Test)
import RemoveComments exposing (removeComments)
import TestUtil exposing (ruleTester, expectErrors)
import Lint exposing (parseSource)
import Regex
import Expect

expectOk : Result a b -> Expect.Expectation
expectOk result =
    case result of
        Ok _ ->
            Expect.pass
        Err error ->
            Expect.fail (toString error)



makeTests : String -> List Test
makeTests code =
    [ test "should remove line comments" <|
            \() ->
                removeComments code
                    |> String.contains "comment text"

                    |> Expect.false ("⟹ single line comment in: \n\n" ++ code
                        ++ "\n\n ⟹ should not be in: \n\n" ++ (removeComments code))

    , test "should not remove any thing other than comments" <|
            \() ->
                removeComments code
                    |> Regex.find Regex.All (Regex.regex "KeepMe")
                    |> List.length
                    |> Expect.equal 2

    , test "resulting code should parse" <|
            \() ->
              let
                source = removeComments code
              in
                source
                    |> parseSource
                    |> expectOk
    ]

all : Test
all =
    describe "removalComments"
        <| List.map (\(name, fixture) -> describe name (makeTests fixture))
        <| fixtures


fixtures : List ( String, String )
fixtures =
    [ ( "single line"
      , """
module SingleLine exposing (function)

import Json.Decode

type alias KeepMe = String
-- comment text
function : Json.Decode.Decoder -> KeepMe
function =
  Json.Decode.string

"""
      )
    , ( "single line inside function"
      , """
module IndentedSingleLine exposing (function)

import Json.Decode

function : Json.Decode.Decoder -> KeepMe
function decoder =
  -- comment text
  "KeepMe"

"""
      )
    , ( "multiline comment"
      , """
module MultliLine exposing (function)

import Json.Decode

type alias KeepMe = String
{- comment text
   and another line
-}
function : Json.Decode.Decoder -> KeepMe
function =
  Json.Decode.string

"""
      )
    , ( "multiline comment inside function"
      ,  """
module IndentedMultliLine exposing (function)

import Json.Decode

function : Json.Decode.Decoder -> KeepMe
  {- comment text
     and another line
  -}
function decoder =
  -- comment text
  "KeepMe"

"""
      )
    ]
