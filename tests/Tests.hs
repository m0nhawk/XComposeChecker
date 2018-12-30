import Data.Either
import Text.ParserCombinators.Parsec
import Test.Tasty
import Test.Tasty.HUnit

import XComposeTypes
import XComposeParser
import XComposeChecker

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [testKeysymParse, testKeyParse, testKeysParse, testResParse, testCommentIgnore, testLine, testGroups]

testKeysymParse =
  testGroup "keysym" [oneLetter, oneSymbol, allSymbols, allSymbolsAndLetters]
  where
    oneLetter = testCase "one letter" $ assertEqual [] "a" (fromRight "" (parse keysym "" "a"))
    oneSymbol = testCase "one symbol" $ assertEqual [] "-" (fromRight "" (parse keysym "" "-"))
    allSymbols = testCase "all symbols" $ assertEqual [] "*?_-.[]~=&:;!#$%^(){}" (fromRight "" (parse keysym "" "*?_-.[]~=&:;!#$%^(){}"))
    allSymbolsAndLetters = testCase "all symbols" $ assertEqual [] "*?_-.[]~=&:;!#$%^(){}abcdefghijklmnopqrstuvwxyz" (fromRight "" (parse keysym "" "*?_-.[]~=&:;!#$%^(){}abcdefghijklmnopqrstuvwxyz"))

testKeyParse =
  testGroup "key" [oneLetter, oneSymbol, allSymbols, allSymbolsAndLetters]
  where
    oneLetter = testCase "one letter" $ assertEqual [] "a" (fromRight "" (parse key "" "<a>"))
    oneSymbol = testCase "one symbol" $ assertEqual [] "-" (fromRight "" (parse key "" "<->"))
    allSymbols = testCase "all symbols" $ assertEqual [] "*?_-.[]~=&:;!#$%^(){}" (fromRight "" (parse key "" "<*?_-.[]~=&:;!#$%^(){}>"))
    allSymbolsAndLetters = testCase "all symbols" $ assertEqual [] "*?_-.[]~=&:;!#$%^(){}abcdefghijklmnopqrstuvwxyz" (fromRight "" (parse key "" "<*?_-.[]~=&:;!#$%^(){}abcdefghijklmnopqrstuvwxyz>"))

testKeysParse =
  testGroup "keys" [oneKey, twoKeys]
  where
    oneKey = testCase "one key" $ assertEqual [] ["a"] (fromRight [""] (parse keys "" "<a>"))
    twoKeys = testCase "two keys" $ assertEqual [] ["a", "a"] (fromRight [""] (parse keys "" "<a> <a>"))

testResParse =
  testGroup "res" [onlyEvent, eventAndKeysym]
  where
    onlyEvent = testCase "only event" $ assertEqual [] (Output "±" Nothing) (fromRight (Output "" Nothing) (parse res "" "\"±\""))
    eventAndKeysym = testCase "event and keysym" $ assertEqual [] (Output "€" (Just "EuroSign")) (fromRight (Output "" Nothing) (parse res "" "\"€\" EuroSign"))
    
testCommentIgnore =
  testGroup "comment" [commentIgnore]
  where
    commentIgnore = testCase "comment ignore" $ assertEqual [] Nothing (fromRight Nothing (parse comment "" "# this is comment!"))

testLine =
  testGroup "line" [noComment, withComment, commentString]
  where
    noComment = testCase "no comment" $ assertEqual [] (Just (Sequence ["Multi_key", "plus", "minus"] (Output "±" Nothing))) (fromRight Nothing (parse line "" "<Multi_key> <plus> <minus> : \"±\""))
    withComment = testCase "with comment" $ assertEqual [] (Just (Sequence ["Multi_key", "x", "x"] (Output "×" (Just "U00D7")))) (fromRight Nothing (parse line "" "<Multi_key> <x> <x> : \"×\" U00D7 # U+00D7 MULTIPLICATION SIGN"))
    commentString = testCase "comment string" $ assertEqual [] Nothing (fromRight Nothing (parse line "" "# this is comment"))
      
testGroups =
  testGroup "group" [emptyGroup, twoLines]
  where
    emptyGroup = testCase "empty group" $ assertEqual [] [] (fromRight [Nothing] (parse group "" ""))
    twoLines = testCase "two lines" $ assertEqual [] [Just (Sequence ["Multi_key", "plus", "minus"] (Output "±" Nothing)), Just (Sequence ["Multi_key", "x", "x"] (Output "×" (Just "U00D7"))), Nothing] (fromRight [Nothing] (parse group "" "<Multi_key> <plus> <minus> : \"±\"\n<Multi_key> <x> <x> : \"×\" U00D7 # U+00D7 MULTIPLICATION SIGN"))

testFile =
  testGroup "file" [emptyFile, fullFile]
  where
    emptyFile = testCase "empty file" $ assertEqual [] [] (fromRight [] (parse file "" ""))
    fullFile = testCase "full file" $ assertEqual [] [Sequence ["Multi_key", "plus", "minus"] (Output "±" Nothing), Sequence ["Multi_key", "x", "x"] (Output "×" (Just "U00D7"))] (fromRight [] (parse file "" "<Multi_key> <plus> <minus> : \"±\"\n<Multi_key> <x> <x> : \"×\" U00D7 # U+00D7 MULTIPLICATION SIGN"))
    