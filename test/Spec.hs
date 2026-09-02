import Data.Map qualified as Map
import Sakuin
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main =
  defaultMain $
    testGroup
      "nix-sakuin"
      [ testGroup
          "parseStorePath"
          [ testCase "parses a canonical Nix store path" $
              (\sp -> (spDir sp, spHash sp, spName sp))
                <$> parseStorePath "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-hello-2.12.1"
                @?= Just ("/nix/store", "0123456789abcdfghijklmnpqrsvwxyz", "hello-2.12.1")
          ],
        testGroup
          "parsePackages"
          [ testCase "parses the nix-env package fixture" $ do
              json <- readFileBS "test/assets/packages.json"
              case parsePackages (toString (decodeUtf8 json :: Text)) of
                Left err -> assertFailure err
                Right (Packages packages) ->
                  Map.keys packages
                    @?= [ "8x37013i8mdk7i7pcr6j45qjaclpi447",
                          "a8gldf6cnq7zqrdq3nqzym0kjy91dxbf",
                          "bryk0z6qn4p4jy10blggrfpq74jh455c",
                          "l6nadiwa0mxax8h3lk301gkl923alg25",
                          "yj989irxax60w546axvwj6y3swmkkq05"
                        ]
          ],
        testGroup
          "WithOrigin"
          [ testCase "maps the value while preserving its origin" $ do
              let packageOrigin = Origin "hello" "out" True "aarch64-darwin"
                  annotated :: WithOrigin Text
                  annotated = WithOrigin packageOrigin "hello"
                  mapped = (<> "!") <$> annotated
              originTuple (origin mapped) @?= originTuple packageOrigin
              value mapped @?= "hello!"
          ],
        testGroup
          "parseNarInfo"
          [ testCase "parses relative references in the narinfo fixture" $ do
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
      ]

storePathTuple :: StorePath -> (Text, Text, Text)
storePathTuple sp = (spDir sp, spHash sp, spName sp)

originTuple :: Origin -> (Text, Text, Bool, Text)
originTuple value = (orAttr value, orOutput value, orToplevel value, orSystem value)
