module Sakuin.SearchSpec (tests) where

import Data.Either (isLeft)
import Data.Text qualified as T
import Sakuin.Search
import Sakuin.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Search"
    [ testCase "matches keywords, suffixes, and regular expressions" $ do
        keywordMatcher "EXAM ple" "/bin/example" @?= True
        suffixMatcher "/bin/example" "/prefix/bin/example" @?= True
        regex <- either fail pure $ regexMatcher "^/bin/.*ple$"
        regex "/bin/example" @?= True
        isLeft (regexMatcher "[") @?= True,
      testCase "recognizes rooted listings heuristically" $ do
        let fullPath = "/gnu/store/0123456789abcdfghijklmnpqrsvwxyz-hello-2.12/bin/hello"
        isListingOf fullPath "/bin/hello" @?= True
        isListingOf fullPath "/bin" @?= False
        listingPathMatches fullPath ("/bin" `T.isPrefixOf`) @?= True
        hasStoreHash fullPath "0123456789abcdfghijklmnpqrsvwxyz" @?= True,
      testCase "handles hyphenated package names and rejects malformed listings" $ do
        listingPathMatches
          "/store-dir/0123456789abcdfghijklmnpqrsvwxyz-hello-world-1.0/libexec/tools/helper"
          ("/libexec/tools" `T.isPrefixOf`)
          @?= True
        isListingOf "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-hello" "/" @?= False
        isListingOf "/nix/store/short-hello/bin/hello" "/bin" @?= False
        isListingOf "/nix/store/0123456789abcdfghijklmnpqrsvwxyu-hello/bin/hello" "/bin" @?= False
        isListingOf "/nix/store/0123456789abcdfghijklmnpqrsvwxyz/bin/hello" "/bin" @?= False,
      testCase "applies structured TSV filters" $ do
        let line = "haskellPackages.alex.out\t21807552 x\t/nix/store/8x37013i8mdk7i7pcr6j45qjaclpi447-alex/bin/alex"
            matches package hash fileTypes =
              matchesTsvSearchFilter
                defaultFilters
                  { filterPackage = package,
                    filterHash = hash,
                    filterTypes = fileTypes
                  }
                (const True)
                line
        matches (Just "haskellPackages.alex") Nothing [] @?= True
        matches (Just "pythonPackages") Nothing [] @?= False
        matches Nothing (Just "8x37013i8mdk7i7pcr6j45qjaclpi447") [] @?= True
        matches Nothing (Just "different") [] @?= False
        matches Nothing Nothing ['r', 'x'] @?= True
        matches Nothing Nothing ['d', 's'] @?= False,
      testCase "matches whole names and paths at package root" $ do
        wholeName <- either fail pure $ pathMatcher "bin/foo" False defaultFilters {filterWholeName = True}
        atRoot <- either fail pure $ pathMatcher "/bin/foo" False defaultFilters {filterAtRoot = True}
        both <- either fail pure $ pathMatcher "/bin/foo" False defaultFilters {filterWholeName = True, filterAtRoot = True}
        let fullPath suffix = "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-example" <> suffix
        wholeName (fullPath "/xx/bin/foo") @?= True
        wholeName (fullPath "/bin/foobar") @?= False
        atRoot (fullPath "/bin/foobar") @?= True
        atRoot (fullPath "/libexec/bin/foo") @?= False
        both (fullPath "/bin/foo") @?= True
        both (fullPath "/bin/foobar") @?= False,
      testCase "handles nonstandard stores and malformed TSV candidates" $ do
        let filters = defaultFilters {filterHash = Just "0123456789abcdfghijklmnpqrsvwxyz", filterTypes = ['x'], filterAtRoot = True}
            line = "hello.out\t42 x\t/store-dir/0123456789abcdfghijklmnpqrsvwxyz-hello-world/bin/hello"
        matcher <- either fail pure $ pathMatcher "/bin/hello" False filters
        matchesTsvSearchFilter filters matcher line @?= True
        matchesTsvSearchFilter defaultFilters matcher "missing-tabs" @?= False
        matchesTsvSearchFilter defaultFilters matcher "hello.out\t42 x\t/not-a-store-path/bin/hello" @?= False
        case pathMatcher "[" True defaultFilters of
          Left _ -> pure ()
          Right _ -> assertFailure "invalid regex unexpectedly compiled"
    ]

defaultFilters :: TsvSearchFilter
defaultFilters =
  TsvSearchFilter
    { filterPackage = Nothing,
      filterHash = Nothing,
      filterTypes = [],
      filterWholeName = False,
      filterAtRoot = False
    }
