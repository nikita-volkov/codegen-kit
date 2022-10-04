module Main where

import Coalmine.Tasty
import qualified Tests.ByModule.ByLanguage.Haskell.Composers.Hashable
import qualified Tests.HaskellExpFormatter
import Prelude

main =
  defaultMain . testGroup "All" $
    [ testGroup "HaskellExpFormatter" $
        Tests.HaskellExpFormatter.tests,
      testGroup "ByModule" $
        [ testGroup "ByLanguage" $
            [ testGroup "Haskell" $
                [ testGroup "Composers" $
                    [ testGroup "Hashable" $
                        Tests.ByModule.ByLanguage.Haskell.Composers.Hashable.tests
                    ]
                ]
            ]
        ]
    ]
