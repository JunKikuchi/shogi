{-# LANGUAGE OverloadedStrings #-}

module ShogiTest.ColorTest (tests) where

import           Data.Aeson
import           Data.Map         as Map
import           Shogi.Color
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Color"
  [ testEncode
  , testDecode
  ]

testEncode :: TestTree
testEncode = testGroup "encode"
  [ testCase "Black" $ encode Black @?= "\"Black\""
  , testCase "White" $ encode White @?= "\"White\""
  , testCase "Map Key" $ encode (Map.fromList [(Black, 'b'), (White, 'w')]) @?= "{\"Black\":\"b\",\"White\":\"w\"}"
  ]

testDecode :: TestTree
testDecode = testGroup "decode"
  [ testCase "Black" $ (decode "\"Black\"" :: Maybe Color) @?= Just Black
  , testCase "White" $ (decode "\"White\"" :: Maybe Color) @?= Just White
  , testCase "black" $ (decode "\"black\"" :: Maybe Color) @?= Nothing
  , testCase "Object Key" $ (decode "{\"Black\":\"b\",\"White\":\"w\"}" :: Maybe (Map.Map Color Char)) @?= Just (Map.fromList [(Black, 'b'), (White, 'w')])
  ]
