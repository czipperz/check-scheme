module ParseTests where

import Test.HUnit
import qualified Parse
import Grammar
import qualified Pos
import TestUtil

parseTests = concat [parseConstantTests, parseVariableTests, parseListTests,
                     generalParseTests]

tag = Pos.tag "*test*"

parseProgram = Parse.parseProgram "*test*"
assertParse name func =
  assertTest name func . parseProgram
assertParseEq name programString expected =
  assertParse name (assertEqual name expected) programString

parseConstantTests = concat [parseBooleanTests, parseIntegerTests, parseCharacterTests, parseStringTests]

parseBooleanTests = [parseBoolean_t, parseBoolean_f]
parseBoolean_t = assertParseEq "parseBoolean_t" "#t"
  (Right [tag 1 1 . Constant $ Boolean True])
parseBoolean_f = assertParseEq "parseBoolean_f" "#f"
  (Right [tag 1 1 . Constant $ Boolean False])

parseIntegerTests = [parseInteger_13, parseInteger_0, parseInteger_N470]
parseInteger_13 = assertParseEq "parseInteger_13" "13"
  (Right [tag 1 1 . Constant $ Integer 13])
parseInteger_0 = assertParseEq "parseInteger_0" "0"
  (Right [tag 1 1 . Constant $ Integer 0])
parseInteger_N470 = assertParseEq "parseInteger_N470" "-470"
  (Right [tag 1 1 . Constant . Integer $ -470])

parseCharacterTests = [parseCharacter_a, parseCharacter_A, parseCharacter_s, parseCharacter_n,
                       parseCharacter_1, parseCharacter_space, parseCharacter_newline]
parseCharacter_a = assertParseEq "parseCharacter_a" "#\\a"
  (Right [tag 1 1 . Constant $ Character 'a'])
parseCharacter_A = assertParseEq "parseCharacter_A" "#\\A"
  (Right [tag 1 1 . Constant $ Character 'A'])
parseCharacter_s = assertParseEq "parseCharacter_a" "#\\s"
  (Right [tag 1 1 . Constant $ Character 's'])
parseCharacter_n = assertParseEq "parseCharacter_n" "#\\n"
  (Right [tag 1 1 . Constant $ Character 'n'])
parseCharacter_1 = assertParseEq "parseCharacter_1" "#\\1"
  (Right [tag 1 1 . Constant $ Character '1'])
parseCharacter_space = assertParseEq "parseCharacter_space" "#\\space"
  (Right [tag 1 1 . Constant $ Character ' '])
parseCharacter_newline = assertParseEq "parseCharacter_newline" "#\\newline"
  (Right [tag 1 1 . Constant $ Character '\n'])

parseStringTests = [parseString_empty, parseString_abc, parseString_escaped_backslash, parseString_escaped_quote]
parseString_empty = assertParseEq "parseString_empty" "\"\""
  (Right [tag 1 1 . Constant $ String ""])
parseString_abc = assertParseEq "parseString_abc" "\"abc\""
  (Right [tag 1 1 . Constant $ String "abc"])
parseString_escaped_backslash = assertParseEq "parseString_escaped_backslash" "\"\\\\\""
  (Right [tag 1 1 . Constant $ String "\\"])
parseString_escaped_quote = assertParseEq "parseString_escaped_quote" "\"\\\"\""
  (Right [tag 1 1 . Constant $ String "\""])

parseVariableTests = [parseVariable_abc, parseVariable_plus, parseVariable_a13q]
parseVariable_abc = assertParseEq "parseVariable_abc" "abc"
  (Right [tag 1 1 $ Variable "abc"])
parseVariable_plus = assertParseEq "parseVariable_+" "+"
  (Right [tag 1 1 $ Variable "+"])
parseVariable_a13q = assertParseEq "parseVariable_a13?" "a13?"
  (Right [tag 1 1 $ Variable "a13?"])

parseListTests = [parseList_empty, parseList_one_item, parseList_two_items,
                  parseList_two_items_with_extra_spaces, parseList_embedded_lists]
parseList_empty = assertParseEq "parseList_empty" "()"
  (Right [tag 1 1 $ List []])
parseList_one_item = assertParseEq "parseList_one_item" "(a)"
  (Right [tag 1 1 $ List [tag 1 2 $ Variable "a"]])
parseList_two_items = assertParseEq "parseList_two_items" "(a b)"
  (Right [tag 1 1 $ List [tag 1 2 $ Variable "a", tag 1 4 $ Variable "b"]])
parseList_two_items_with_extra_spaces = assertParseEq "parseList_two_items_with_extra_spaces" "( a  b )"
  (Right [tag 1 1 $ List [tag 1 3 $ Variable "a", tag 1 6 $ Variable "b"]])
parseList_embedded_lists = assertParseEq "parseList_embedded_lists" "(a (b) ((c)) (d) e)"
  (Right [tag 1 1 $ List [tag 1 2 $ Variable "a",
                          tag 1 4 $ List [tag 1 5 $ Variable "b"],
                          tag 1 8 $ List [tag 1 9 $ List [tag 1 10 $ Variable "c"]],
                          tag 1 14 $ List [tag 1 15 $ Variable "d"],
                          tag 1 18 $ Variable "e"]])

generalParseTests = [parseEmptyFile, parseEmptyFileWithSpaces, parseTwoExpressions]
parseEmptyFile = assertParseEq "parseEmptyFile" ""
  (Right [])
parseEmptyFileWithSpaces = assertParseEq "parseEmptyFileWithSpaces" "  "
  (Right [])
parseTwoExpressions = assertParseEq "parseTwoExpressions" "(define v 3) v"
  (Right [tag 1 1 $ List [tag 1 2 $ Variable "define", tag 1 9 $ Variable "v", tag 1 11 $ Constant (Integer 3)],
          tag 1 14 $ Variable "v"])
