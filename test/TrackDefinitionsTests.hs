module TrackDefinitionsTests where

import Test.HUnit
import Parse
import TrackDefinitions
import TestUtil
import Pos hiding (tag)

tag l c = Tag (Pos "*test*" l c)

trackDefinitionsTests = [trackDefinitions_NoDefinitionsNoUsages,
                         trackDefinitions_DefineThenUse,
                         trackDefinitions_UseThenDefine,
                         trackDefinitions_FunctionUsedWrongNumberArguments,
                         trackDefinitions_FunctionUsedCorrectNumberArguments_0,
                         trackDefinitions_FunctionUsedCorrectNumberArguments_3,
                         trackDefinitions_LambdaBindsInBody,
                         trackDefinitions_LambdaBindsAfterBodyStops,
                         trackDefinitions_LetBindsAfterBodyStops,
                         trackDefinitions_LetBindsWithValue,
                         trackDefinitions_LetBindsWithLambdaWrong,
                         trackDefinitions_LetBindsWithLambdaCorrect,
                         trackDefinitions_LetBindsAnotherVariableWrong,
                         trackDefinitions_LetBindsAnotherVariableCorrect,
                         trackDefinitions_PlusDefined,
                         trackDefinitions_PlusDefinedNoArguments,
                         trackDefinitions_PlusDefinedOneArgument,
                         trackDefinitions_PlusDefinedManyArguments]

unwrap :: Either String a -> a
unwrap (Left err) = error err
unwrap (Right x) = x

assertTrackDefinitionsEq name expectedMessages program =
  let parsed = unwrap $ parseProgram "*test*" program
      messages = TrackDefinitions.run parsed in
    assertEqTest name expectedMessages messages

trackDefinitions_NoDefinitionsNoUsages =
  assertTrackDefinitionsEq "trackDefinitions_NoDefinitionsNoUsages"
    [] "3 4"
trackDefinitions_DefineThenUse =
  assertTrackDefinitionsEq "trackDefinitions_DefineThenUse"
    [] "(define v 3) v"
trackDefinitions_UseThenDefine =
  assertTrackDefinitionsEq "trackDefinitions_DefineThenUse"
    [tag 1 1 $ UnboundVariable "v"] "v (define v 3)"

trackDefinitions_FunctionUsedWrongNumberArguments =
  assertTrackDefinitionsEq "trackDefinitions_FunctionUsedWrongNumberArguments"
    [tag 1 26 $ WrongNumberOfArguments "v" 1] "(define v (lambda () 3)) (v 1)"
trackDefinitions_FunctionUsedCorrectNumberArguments_0 =
  assertTrackDefinitionsEq "trackDefinitions_FunctionUsedCorrectNumberArguments_0"
    [] "(define v (lambda () 3)) (v)"
trackDefinitions_FunctionUsedCorrectNumberArguments_3 =
  assertTrackDefinitionsEq "trackDefinitions_FunctionUsedCorrectNumberArguments_3"
    [] "(define v (lambda (a b c) 4)) (v 1 2 3)"

trackDefinitions_LambdaBindsInBody =
  assertTrackDefinitionsEq "trackDefinitions_LambdaBindsInBody"
    [] "(lambda (a b) a b)"
trackDefinitions_LambdaBindsAfterBodyStops =
  assertTrackDefinitionsEq "trackDefinitions_LambdaBindsAfterBodyStops"
    [tag 1 18 $ UnboundVariable "b"] "(lambda (a b) a) b"

trackDefinitions_LetBindsAfterBodyStops =
  assertTrackDefinitionsEq "trackDefinitions_LetBindsAfterBodyStops"
    [tag 1 15 $ UnboundVariable "b"] "(let (a b) a) b"
trackDefinitions_LetBindsWithValue =
  assertTrackDefinitionsEq "trackDefinitions_LetBindsWithValue"
    [] "(let ((a 3) b) a)"
trackDefinitions_LetBindsWithLambdaWrong =
  assertTrackDefinitionsEq "trackDefinitions_LetBindsWithLambdaWrong"
    [tag 1 29 $ WrongNumberOfArguments "a" 0] "(let ((a (lambda (x) 3)) b) (a))"
trackDefinitions_LetBindsWithLambdaCorrect =
  assertTrackDefinitionsEq "trackDefinitions_LetBindsWithLambdaCorrect"
    [] "(let ((a (lambda (x) 3)) b) (a 3))"
trackDefinitions_LetBindsAnotherVariableWrong =
  assertTrackDefinitionsEq "trackDefinitions_LetBindsAnotherVariableWrong"
    [tag 1 40 $ WrongNumberOfArguments "b" 0] "(let ((a (lambda (x) 3))) (let ((b a)) (b)))"
trackDefinitions_LetBindsAnotherVariableCorrect =
  assertTrackDefinitionsEq "trackDefinitions_LetBindsAnotherVariableCorrect"
    [] "(let ((a (lambda (x) 3))) (let ((b a)) (b 3)))"

trackDefinitions_PlusDefined =
  assertTrackDefinitionsEq "trackDefinitions_PlusDefined"
    [] "(+ 1 3)"
trackDefinitions_PlusDefinedNoArguments =
  assertTrackDefinitionsEq "trackDefinitions_PlusDefinedNoArguments"
    [tag 1 1 $ WrongNumberOfArguments "+" 0] "(+)"
trackDefinitions_PlusDefinedOneArgument =
  assertTrackDefinitionsEq "trackDefinitions_PlusDefinedOneArgument"
    [] "(+ 1)"
trackDefinitions_PlusDefinedManyArguments =
  assertTrackDefinitionsEq "trackDefinitions_PlusDefinedManyArguments"
    [] "(+ 1 2 3 4 5 6 7 8)"
