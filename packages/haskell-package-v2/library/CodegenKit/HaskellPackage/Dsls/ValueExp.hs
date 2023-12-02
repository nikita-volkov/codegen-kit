-- | Value expression printer DSL managing metadata about dependencies, imports and extensions.
module CodegenKit.HaskellPackage.Dsls.ValueExp where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude hiding (exp)
import CodegenKit.HaskellPackage.Aggregates.CodeGrouping qualified as CodeGrouping
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements (CodeRequirements)
import CodegenKit.HaskellPackage.Aggregates.CodeRequirements qualified as CodeRequirements
import CodegenKit.HaskellPackage.ComonadicContexts.Code qualified as Code
import CodegenKit.HaskellPackage.Dsls.Exp as Exp
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Exp qualified as LegacyExp
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Dependencies qualified as Dependencies
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import TextBuilderDev qualified as UnilineSplice

infixBinOp :: Text -> Text -> Exp -> Exp -> Exp
infixBinOp moduleName symbolName left right =
  if left.isMultiline || right.isMultiline
    then
      Exp True True
        $ mconcat
          [ toGroupedCodeSplice left,
            fmap (Splice.indent 2)
              $ mconcat
                [ "\n",
                  operatorCode,
                  " ",
                  fmap (Splice.indent indent) (toGroupedCodeSplice right)
                ]
          ]
    else
      Exp True False
        $ mconcat
          [ toGroupedCodeSplice left,
            " ",
            operatorCode,
            " ",
            toGroupedCodeSplice right
          ]
  where
    (indent, operatorCode) =
      case Text.uncons symbolName of
        Nothing -> (0, "")
        Just (head, _) ->
          if isAlpha head
            then
              ( Text.length symbolName + 3,
                pure ("`" <> to symbolName <> "`")
              )
            else
              ( Text.length symbolName + 1,
                pure (to symbolName)
              )

decimalLiteral :: (Integral a) => a -> Exp
decimalLiteral = Exp.decimalLiteral

stringLiteral :: Text -> Exp
stringLiteral = Exp.stringLiteral

list :: [Exp] -> Exp
list exps =
  if all (\x -> not (x.needsGrouping || x.isMultiline)) exps
    then
      Exp False False
        $ mconcat
          [ "[",
            List.intercalate ", " (fmap toUngroupedCodeSplice exps),
            "]"
          ]
    else multilineList exps

multilineList :: [Exp] -> Exp
multilineList = \case
  [] ->
    Exp False False "[]"
  a : b ->
    Exp False True
      $ mconcat
        [ "[ ",
          fmap (Splice.indent 2) (toUngroupedCodeSplice a <> foldMap (mappend ",\n" . toUngroupedCodeSplice) b),
          "\n]"
        ]

appChain :: Exp -> [Exp] -> Exp
appChain =
  chainApplication

multilinePostAppChain :: Exp -> [Exp] -> Exp
multilinePostAppChain baseExp chain =
  Exp True True
    $ toGroupedCodeSplice baseExp
    <> fmap (Splice.indent 2) (foldMap (mappend "\n& " . fmap (Splice.indent 4) . toGroupedCodeSplice) chain)

overloadedRecordDotField ::
  -- | Field name.
  Text ->
  -- | Record value.
  Exp ->
  -- | Field accessor expression based on preferences.
  Exp
overloadedRecordDotField fieldName exp =
  Exp False exp.isMultiline
    $ toGroupedCodeSplice exp
    <> "."
    <> pure (to fieldName)

-- * --

apChain ::
  Exp ->
  [Exp] ->
  Exp
apChain constructor params =
  case params of
    [] ->
      if constructor.isMultiline
        then
          Exp True True
            $ pureCode
            <> fmap (Splice.indent 2) ("\n" <> toGroupedCodeSplice constructor)
        else
          Exp True False
            $ pureCode
            <> " "
            <> toGroupedCodeSplice constructor
    [param]
      | not (constructor.isMultiline || param.isMultiline) ->
          Exp True False
            $ toGroupedCodeSplice constructor
            <> " "
            <> mapOperatorCode
            <> " "
            <> toGroupedCodeSplice param
    _ ->
      Exp True True
        $ toGroupedCodeSplice constructor
        <> fmap (Splice.indent 2) ("\n" <> mapOperatorCode <> " " <> List.intercalate ("\n" <> apOperatorCode <> " ") (fmap (fmap (Splice.indent 4) . toGroupedCodeSplice) params))
  where
    preludeRefCode = toUngroupedCodeSplice . importedSymbol "Prelude"
    mapOperatorCode = preludeRefCode "<$>"
    apOperatorCode = preludeRefCode "<*>"
    pureCode = preludeRefCode "pure"

alternatives ::
  [Exp] ->
  Exp
alternatives = \case
  [] -> importedSymbol "Control.Applicative" "empty"
  [a] -> a
  [a, b] -> infixBinOp "Control.Applicative" "<|>" a b
  alternatives -> appChain (importedSymbol "Control.Applicative" "asum") [list alternatives]

staticMonoid ::
  [Exp] ->
  Exp
staticMonoid = \case
  [] -> importedSymbol "Data.Monoid" "mempty"
  [a] -> a
  [a, b] -> infixBinOp "Data.Monoid" "<>" a b
  a -> appChain (importedSymbol "Data.Monoid" "mconcat") [list a]
