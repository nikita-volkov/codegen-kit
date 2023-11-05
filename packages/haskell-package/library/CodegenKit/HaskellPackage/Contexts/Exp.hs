module CodegenKit.HaskellPackage.Contexts.Exp where

import Coalmine.Prelude
import CodegenKit.HaskellPackage.Contexts.CompiledCode qualified as CompiledCode
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Exp qualified as BaseExp

toCompiledCode :: Config -> Exp -> CompiledCode.CompiledCode
toCompiledCode config exp =
  error "TODO"

data Config = Config
  { deref :: Text -> Text,
    overloadedRecordDot :: Bool
  }

data Exp = Exp
  { -- | Compile into a more primitive exp.
    compile :: Config -> BaseExp.Exp
  }

reference :: Text -> Text -> Exp
reference moduleName symbolName =
  case moduleName of
    "" -> error "TODO"
    _ -> Exp compile
      where
        compile config =
          BaseExp.reference (config.deref moduleName) symbolName

accessField ::
  -- | Field selector function for use when overloaded-record-dot is disabled.
  Exp ->
  -- | Field name for use when overloaded-record-dot is enabled.
  Text ->
  -- | Record value.
  Exp ->
  -- | Field accessor expression based on preferences.
  Exp
accessField selector fieldName value =
  Exp compile
  where
    compile config =
      if config.overloadedRecordDot
        then
          BaseExp.overloadedRecordDotField fieldName
            $ value.compile config
        else
          BaseExp.appChain
            (selector.compile config)
            [value.compile config]

infixBinOp :: Text -> Text -> Exp -> Exp -> Exp
infixBinOp moduleName symbolName left right =
  Exp \config ->
    BaseExp.infixBinOp
      (config.deref moduleName)
      symbolName
      (left.compile config)
      (right.compile config)

intLiteral :: (Integral a) => a -> Exp
intLiteral value =
  Exp \_ ->
    BaseExp.intLiteral value

stringLiteral :: Text -> Exp
stringLiteral value =
  Exp \_ ->
    BaseExp.stringLiteral value

multilineList :: [Exp] -> Exp
multilineList exps =
  Exp \config ->
    BaseExp.multilineList (fmap (\exp -> exp.compile config) exps)

list :: [Exp] -> Exp
list exps =
  Exp \config ->
    BaseExp.list (fmap (\exp -> exp.compile config) exps)

appChain :: Exp -> [Exp] -> Exp
appChain fn exps =
  Exp \config ->
    BaseExp.appChain
      (fn.compile config)
      (fmap (\exp -> exp.compile config) exps)

staticMonoid :: [Exp] -> Exp
staticMonoid exps =
  Exp \config ->
    BaseExp.staticMonoid
      (config.deref "Prelude")
      (fmap (\exp -> exp.compile config) exps)

apChain :: Exp -> [Exp] -> Exp
apChain constructor params =
  Exp \config ->
    BaseExp.apChain
      (config.deref "Prelude")
      (constructor.compile config)
      (fmap (\exp -> exp.compile config) params)
