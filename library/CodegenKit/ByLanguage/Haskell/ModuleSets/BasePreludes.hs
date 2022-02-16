module CodegenKit.ByLanguage.Haskell.ModuleSets.BasePreludes
  ( -- *
    allName,
    operatorsName,
    dataTypesName,

    -- *
    all,
    dataTypes,
    operators,
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.Name as Name
import qualified CodegenKit.ByLanguage.Haskell.Dependencies as Dependencies
import qualified CodegenKit.ByLanguage.Haskell.Packaging as Packaging
import qualified CodegenKit.ByLanguage.Haskell.Snippets as Snippets
import CodegenKit.Prelude hiding (all)
import qualified TextBuilderDev as B'

-- *

allName :: Name
allName = "base-all"

dataTypesName :: Name
dataTypesName = "base-data-types"

operatorsName :: Name
operatorsName = "base-operators"

-- *

operators :: Packaging.Modules
operators =
  Packaging.module_ False operatorsName deps contents
  where
    deps =
      [ Dependencies.base
      ]
    contents nsNameList =
      [i|
        -- |
        -- A collection of common operators provided across
        -- various modules of the \"base\" package.
        module $moduleRef
          ( -- * From "Control.Applicative"
            (Control.Applicative.*>),
            (Control.Applicative.<*),
            (Control.Applicative.<*>),
            (Control.Applicative.<|>),

            -- * From "Control.Monad"
            (Control.Monad.<=<),
            (Control.Monad.=<<),
            (Control.Monad.>=>),
            (Control.Monad.>>),
            (Control.Monad.>>=),

            -- * From "Data.Bits"
            (Data.Bits..&.),
            (Data.Bits..|.),

            -- * From "Data.Bool"
            (Data.Bool.&&),
            (Data.Bool.||),
            (Data.Eq./=),
            (Data.Eq.==),

            -- * From "Data.Function"
            (Data.Function.$$),
            (Data.Function.&),
            (Data.Function..),

            -- * From "Data.Functor"
            (Data.Functor.$$>),
            (Data.Functor.<$$),
            (Data.Functor.<$$>),
            (Data.Functor.<&>),

            -- * From "Data.Ord"
            (Data.Ord.<),
            (Data.Ord.<=),
            (Data.Ord.>),
            (Data.Ord.>=),

            -- * From "Data.Ratio"
            (Data.Ratio.%),

            -- * From "Data.Semigroup"
            (Data.Semigroup.<>),

            -- * From "Prelude"
            (Prelude.$$!),
            (Prelude.*),
            (Prelude.+),
            (Prelude.-),
            (Prelude./),
            (Prelude.^),
            (Prelude.^^),
          )
        where

        import qualified Control.Applicative
        import qualified Control.Monad
        import qualified Data.Bits
        import qualified Data.Bool
        import qualified Data.Eq
        import qualified Data.Function
        import qualified Data.Functor
        import qualified Data.Ord
        import qualified Data.Ratio
        import qualified Data.Semigroup
        import qualified Prelude

      |]
      where
        moduleRef = Snippets.moduleRef nsNameList operatorsName

dataTypes :: Packaging.Modules
dataTypes =
  Packaging.module_ False dataTypesName deps contents
  where
    deps =
      [ Dependencies.base
      ]
    contents nsNameList =
      [i|
        -- |
        -- A module that reexports only the data types
        -- defined across various modules of the \"base\" package.
        --
        -- By data types we mean that it is the ones we use
        -- to define data structures.
        -- It is not abstraction integration wrappers,
        -- like 'Data.Semigroup.First'.
        -- It is not resource types like 'System.IO.Handle'.
        module $moduleRef
          ( -- * From "Prelude"
            Prelude.Bool (..),
            Prelude.Char,
            Prelude.Double,
            Prelude.Either (..),
            Prelude.Float,
            Prelude.Integer,
            Prelude.Maybe (..),
            Prelude.String,

            -- * From "Data.Int"
            Data.Int.Int,
            Data.Int.Int16,
            Data.Int.Int32,
            Data.Int.Int64,
            Data.Int.Int8,

            -- * From "Data.Word"
            Data.Word.Word,
            Data.Word.Word16,
            Data.Word.Word32,
            Data.Word.Word64,
            Data.Word.Word8,

            -- * From "Data.Ratio"
            Data.Ratio.Rational,

            -- * From "Numeric.Natural"
            Numeric.Natural.Natural,

            -- * From "Data.List.NonEmpty"
            Data.List.NonEmpty.NonEmpty (..),
          )
        where

        import qualified Data.Int
        import qualified Data.List.NonEmpty
        import qualified Data.Ratio
        import qualified Data.Word
        import qualified Numeric.Natural
        import qualified Prelude

      |]
      where
        moduleRef = Snippets.moduleRef nsNameList dataTypesName

all :: Packaging.Modules
all =
  Packaging.module_ False allName deps contents
  where
    deps =
      [ Dependencies.base
      ]
    contents nsNameList =
      [i|
        -- |
        -- Reexports of most of the definitions from the \"base\" package,
        -- which it is a common practice to import unqualified.
        module $moduleRef
          ( module Exports,
          )
        where

        import Prelude as Exports hiding (fail, concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, id, (.))
        import Control.Applicative as Exports
        import Control.Arrow as Exports hiding (first, second)
        import Control.Category as Exports
        import Control.Concurrent as Exports
        import Control.Exception as Exports
        import Control.Monad as Exports hiding (fail, mapM_, sequence_, forM_, msum, mapM, sequence, forM)
        import Control.Monad.Fail as Exports
        import Control.Monad.Fix as Exports hiding (fix)
        import Control.Monad.IO.Class as Exports
        import Control.Monad.ST as Exports
        import Data.Bifunctor as Exports
        import Data.Bits as Exports
        import Data.Bool as Exports
        import Data.Char as Exports
        import Data.Coerce as Exports
        import Data.Complex as Exports
        import Data.Data as Exports
        import Data.Dynamic as Exports
        import Data.Either as Exports
        import Data.Fixed as Exports
        import Data.Foldable as Exports hiding (toList)
        import Data.Function as Exports hiding (id, (.))
        import Data.Functor as Exports
        import Data.Functor.Classes as Exports
        import Data.Functor.Compose as Exports
        import Data.Functor.Contravariant as Exports
        import Data.Functor.Identity as Exports
        import Data.Int as Exports
        import Data.IORef as Exports
        import Data.Ix as Exports
        import Data.List as Exports hiding (concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
        import Data.List.NonEmpty as Exports (NonEmpty(..))
        import Data.Maybe as Exports
        import Data.Monoid as Exports hiding (Alt, First(..), Last(..), (<>))
        import Data.Ord as Exports
        import Data.Proxy as Exports
        import Data.Ratio as Exports
        import Data.Semigroup as Exports
        import Data.STRef as Exports
        import Data.String as Exports
        import Data.Traversable as Exports
        import Data.Tuple as Exports
        import Data.Unique as Exports
        import Data.Version as Exports
        import Data.Void as Exports
        import Data.Word as Exports
        import Debug.Trace as Exports
        import Foreign.ForeignPtr as Exports
        import Foreign.Ptr as Exports
        import Foreign.StablePtr as Exports
        import Foreign.Storable as Exports
        import GHC.Conc as Exports hiding (orElse, withMVar, threadWaitWriteSTM, threadWaitWrite, threadWaitReadSTM, threadWaitRead)
        import GHC.Exts as Exports (lazy, inline, sortWith, groupWith, IsList(..))
        import GHC.Generics as Exports (Generic)
        import GHC.IO.Exception as Exports
        import GHC.OverloadedLabels as Exports
        import GHC.Records as Exports
        import Numeric as Exports
        import Numeric.Natural as Exports
        import System.Environment as Exports
        import System.Exit as Exports
        import System.IO as Exports (Handle, hClose)
        import System.IO.Error as Exports
        import System.IO.Unsafe as Exports
        import System.Mem as Exports
        import System.Mem.StableName as Exports
        import System.Timeout as Exports
        import Text.ParserCombinators.ReadP as Exports (ReadP, ReadS, readP_to_S, readS_to_P)
        import Text.ParserCombinators.ReadPrec as Exports (ReadPrec, readPrec_to_P, readP_to_Prec, readPrec_to_S, readS_to_Prec)
        import Text.Printf as Exports (printf, hPrintf)
        import Text.Read as Exports (Read(..), readMaybe, readEither)
        import Unsafe.Coerce as Exports
        
      |]
      where
        moduleRef = Snippets.moduleRef nsNameList allName
