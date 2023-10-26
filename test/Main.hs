module Main where

import Coalmine.Tasty
import qualified Tests.ByModule.ByLanguage.Haskell.Composers.Exp
import qualified Tests.ByModule.ByLanguage.Haskell.Composers.Hashable
import Prelude

main =
  (defaultMain . testGroup "All")
    [ testGroup "HaskellExpFormatter"
        $ Tests.ByModule.ByLanguage.Haskell.Composers.Exp.tests,
      testGroup "ByModule"
        $ [ testGroup "ByLanguage"
              $ [ testGroup "Haskell"
                    $ [ testGroup "Composers"
                          $ [ testGroup "Hashable"
                                $ Tests.ByModule.ByLanguage.Haskell.Composers.Hashable.tests
                            ]
                      ]
                ]
          ]
    ]
