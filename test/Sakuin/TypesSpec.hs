module Sakuin.TypesSpec (tests) where

import Sakuin
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Types"
    [ testCase "parses a canonical Nix store path" $
        (\sp -> (spDir sp, spHash sp, spName sp))
          <$> parseStorePath "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-hello-2.12.1"
          @?= Just ("/nix/store", "0123456789abcdfghijklmnpqrsvwxyz", "hello-2.12.1"),
      testCase "maps WithOrigin values while preserving their origin" $ do
        let packageOrigin = Origin "hello" "out" True "aarch64-darwin"
            annotated :: WithOrigin Text
            annotated = WithOrigin packageOrigin "hello"
            mapped = (<> "!") <$> annotated
        originTuple (origin mapped) @?= originTuple packageOrigin
        value mapped @?= "hello!",
      testCase "parses relative references in the narinfo fixture" $ do
        narinfo <- readFileBS "test/assets/5a5lrqlgqqhfd02lp7l8gqdypcckxiqd.narinfo"
        case parseNarInfo narinfo of
          Nothing -> assertFailure "failed to parse narinfo fixture"
          Just parsed -> do
            storePathTuple (niStorePath parsed)
              @?= ("/nix/store", "5a5lrqlgqqhfd02lp7l8gqdypcckxiqd", "Agda-2.8.0-bin")
            niNarPath parsed
              @?= "nar/1xz06276mlns3mspsj26zjar4pn77b106am2bjqc63zvlvalwgm0.nar.zst"
            let references = niReferences parsed
            length references @?= 7
            (storePathTuple <$> viaNonEmpty head references)
              @?= Just ("/nix/store", "2l4pgv9hmhsk7q57rk6lp4hpgii084f2", "Agda-2.8.0-data")
            all ((== "/nix/store") . spDir) references @?= True
    ]

storePathTuple :: StorePath -> (Text, Text, Text)
storePathTuple sp = (spDir sp, spHash sp, spName sp)

originTuple :: Origin -> (Text, Text, Bool, Text)
originTuple entryOrigin =
  (orAttr entryOrigin, orOutput entryOrigin, orToplevel entryOrigin, orSystem entryOrigin)
