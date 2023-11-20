module CodegenKit.HaskellPackage.Contexts.Code3 where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude hiding (exp)
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements (CodeRequirements)
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements qualified as CodeRequirements
import CodegenKit.HaskellPackage.Contexts.Package qualified as Package
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Dependencies qualified as Dependencies
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import TextBuilderDev qualified as UnilineSplice

newtype Config = Config
  { -- | Function attempting to look up an alias or
    -- qualified reference for a module,
    -- producing empty text otherwise.
    aliasModule :: Text -> Text
  }

instance Semigroup Config where
  left <> right =
    Config
      { aliasModule = \input ->
          let leftResult = left.aliasModule input
           in if Text.null leftResult
                then right.aliasModule input
                else leftResult
      }

instance Monoid Config where
  mempty =
    Config
      { aliasModule = const Text.empty
      }

newtype Context a = Context
  { reader :: Config -> CodeRequirements -> (a, CodeRequirements)
  }
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT Config (State CodeRequirements))

instance (Semigroup a) => Semigroup (Context a) where
  left <> right =
    Context
      ( \config requirements ->
          case left.reader config requirements of
            (leftResult, leftResultRequirements) ->
              case right.reader config leftResultRequirements of
                (rightResult, rightResultRequirements) ->
                  ( leftResult <> rightResult,
                    rightResultRequirements
                  )
      )
