module CodegenKit.HaskellPackage.Contexts.Exp
  ( toGroupedCode,
    toUngroupedCode,
    Exp,
    code,
    reference,
    infixBinOp,
    intLiteral,
    stringLiteral,
    multilineList,
    list,
    appChain,
    multilinePostAppChain,
    overloadedRecordDotField,
    apChain,
    alternatives,
    staticMonoid,
  )
where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.Prelude
import CodegenKit.HaskellPackage.Contexts.Code (Code)
import CodegenKit.HaskellPackage.Contexts.Code qualified as Code
import CodegenKit.HaskellPackage.Contexts.CompiledCode qualified as CompiledCode
import CodegenKit.HaskellPackage.Contexts.GroupingCode (GroupingCode)
import CodegenKit.HaskellPackage.Contexts.GroupingCode qualified as GroupingCode
import CodegenKit.Legacy.ByLanguage.Haskell.Composers.Exp qualified as LegacyExp
import Data.Text qualified as Text

toGroupedCode :: Exp -> Code
toGroupedCode =
  GroupingCode.toGroupedCode . (.groupingCode)

toUngroupedCode :: Exp -> Code
toUngroupedCode =
  GroupingCode.toUngroupedCode . (.groupingCode)

newtype Exp = Exp
  { groupingCode :: GroupingCode
  }

mapGroupingCode ::
  (GroupingCode -> GroupingCode) ->
  Exp ->
  Exp
mapGroupingCode = coerce

code ::
  -- | Requires grouping.
  Bool ->
  -- | Is multiline.
  Bool ->
  Code ->
  Exp
code needsGrouping isMultiline code =
  Exp $ GroupingCode.GroupingCode needsGrouping isMultiline code

reference :: Text -> Text -> Exp
reference moduleName symbolName =
  Exp $ GroupingCode.importedSymbol moduleName symbolName

infixBinOp :: Text -> Text -> Exp -> Exp -> Exp
infixBinOp moduleName symbolName left right =
  if left.groupingCode.isMultiline || right.groupingCode.isMultiline
    then
      Exp
        $ GroupingCode.GroupingCode True True
        $ mconcat
          [ toGroupedCode left,
            Code.indent 2
              $ mconcat
                [ "\n",
                  operatorCode,
                  " ",
                  Code.indent indent (toGroupedCode right)
                ]
          ]
    else
      Exp
        $ GroupingCode.GroupingCode True False
        $ mconcat
          [ toGroupedCode left,
            " ",
            operatorCode,
            " ",
            toGroupedCode right
          ]
  where
    (indent, operatorCode) =
      case Text.uncons symbolName of
        Nothing -> (0, "")
        Just (head, _) ->
          if isAlpha head
            then
              ( Text.length symbolName + 3,
                Code.splice ("`" <> to symbolName <> "`")
              )
            else
              ( Text.length symbolName + 1,
                Code.splice (to symbolName)
              )

intLiteral :: (Integral a) => a -> Exp
intLiteral =
  code False False . Code.decimalLiteral

stringLiteral :: Text -> Exp
stringLiteral =
  Exp . GroupingCode.stringLiteral

multilineList :: [Exp] -> Exp
multilineList = \case
  [] ->
    code False False "[]"
  a : b ->
    code False True
      $ mconcat
        [ "[ ",
          Code.indent 2 (toUngroupedCode a <> foldMap (mappend ",\n" . toUngroupedCode) b),
          "\n]"
        ]

list :: [Exp] -> Exp
list exps =
  if all (\(Exp (GroupingCode.GroupingCode needsGrouping isMultiline _)) -> not (needsGrouping || isMultiline)) exps
    then
      code False False
        $ mconcat
          [ "[",
            List.intercalate ", " (fmap toUngroupedCode exps),
            "]"
          ]
    else multilineList exps

appChain :: Exp -> [Exp] -> Exp
appChain fn params =
  if all (not . (.groupingCode.isMultiline)) params && not fn.groupingCode.isMultiline
    then
      code True False
        $ toUngroupedCode fn
        <> foldMap (mappend " " . toGroupedCode) params
    else
      code True True
        $ toGroupedCode fn
        <> Code.indent 2 (foldMap (mappend "\n" . toGroupedCode) params)

multilinePostAppChain :: Exp -> [Exp] -> Exp
multilinePostAppChain baseExp chain =
  code True True
    $ toGroupedCode baseExp
    <> Code.indent 2 (foldMap (mappend "\n& " . Code.indent 4 . toGroupedCode) chain)

overloadedRecordDotField ::
  -- | Field name.
  Text ->
  -- | Record value.
  Exp ->
  -- | Field accessor expression based on preferences.
  Exp
overloadedRecordDotField fieldName exp =
  code False exp.groupingCode.isMultiline
    $ toGroupedCode exp
    <> "."
    <> Code.splice (to fieldName)

-- * --

apChain ::
  Exp ->
  [Exp] ->
  Exp
apChain constructor params =
  case params of
    [] ->
      if constructor.groupingCode.isMultiline
        then
          code True True
            $ pureCode
            <> Code.indent 2 ("\n" <> toGroupedCode constructor)
        else
          code True False
            $ pureCode
            <> " "
            <> toGroupedCode constructor
    [param]
      | not (constructor.groupingCode.isMultiline || param.groupingCode.isMultiline) ->
          code True False
            $ toGroupedCode constructor
            <> " "
            <> mapOperatorCode
            <> " "
            <> toGroupedCode param
    _ ->
      code True True
        $ toGroupedCode constructor
        <> Code.indent 2 ("\n" <> mapOperatorCode <> " " <> List.intercalate ("\n" <> apOperatorCode <> " ") (fmap (Code.indent 4 . toGroupedCode) params))
  where
    preludeRefCode = toUngroupedCode . reference "Prelude"
    mapOperatorCode = preludeRefCode "<$>"
    apOperatorCode = preludeRefCode "<*>"
    pureCode = preludeRefCode "pure"

alternatives ::
  -- | Prelude namespace.
  Text ->
  [Exp] ->
  Exp
alternatives preludeNs = \case
  [] -> reference preludeNs "empty"
  [a] -> a
  [a, b] -> infixBinOp preludeNs "<|>" a b
  alternatives -> appChain (reference preludeNs "asum") [list alternatives]

staticMonoid ::
  -- | Prelude namespace.
  Text ->
  [Exp] ->
  Exp
staticMonoid preludeNs = \case
  [] -> reference preludeNs "mempty"
  [a] -> a
  [a, b] -> infixBinOp preludeNs "<>" a b
  a -> appChain (reference preludeNs "mconcat") [list a]
