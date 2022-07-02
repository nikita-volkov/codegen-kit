-- |
-- Smart haskell expressions formatter,
-- taking care of parenthesis and indentation.
module CodegenKit.ByLanguage.Haskell.ExpFormatter where

import qualified Coalmine.MultilineTextBuilder as B
import CodegenKit.Prelude
import qualified Data.Text as Text
import qualified TextBuilderDev as B'

-- * --

groupedExp :: Exp -> B.Builder
groupedExp (Exp needsGrouping isMultiline content) =
  if needsGrouping
    then
      if isMultiline
        then
          mconcat
            [ "( ",
              B.indent 2 content,
              "\n)"
            ]
        else "(" <> content <> ")"
    else content

ungroupedExp :: Exp -> B.Builder
ungroupedExp (Exp _ _ content) =
  content

-- ** --

isMultiline :: Exp -> Bool
isMultiline (Exp _ isMultiline _) =
  isMultiline

-- * --

data Exp
  = Exp
      !Bool
      -- ^ Needs grouping.
      !Bool
      -- ^ Is multiline.
      !B.Builder
      -- ^ Possibly multiline content.

-- * Essentials

infixBinOp :: Text -> Exp -> Exp -> Exp
infixBinOp operator l r =
  if isMultiline l || isMultiline r
    then
      Exp True True $
        mconcat
          [ groupedExp l,
            B.indent
              2
              ( mconcat
                  [ "\n",
                    from @Text operator,
                    " ",
                    B.indent
                      (Text.length operator + 1)
                      (groupedExp r)
                  ]
              )
          ]
    else
      Exp True False $
        mconcat
          [ groupedExp l,
            " ",
            from @Text operator,
            " ",
            groupedExp r
          ]

reference :: Text -> Text -> Exp
reference qualification reference =
  Exp False False $
    if Text.null qualification
      then from @Text reference
      else from @Text qualification <> "." <> from @Text reference

intLiteral :: Integral a => a -> Exp
intLiteral int =
  Exp False False $
    to $ B'.decimal int

stringLiteral :: Text -> Exp
stringLiteral text =
  Exp False isMultiline splice
  where
    isMultiline =
      case processedLines of
        [] -> False
        [_] -> False
        _ -> True
    splice =
      mconcat
        [ "\"",
          B.intercalate "\\n\\\n\\" processedLines,
          "\""
        ]
    processedLines =
      fmap (from @Text)
        . Text.lines
        . Text.pack
        . join
        . fmap escapeChar
        . Text.unpack
        $ text
      where
        escapeChar = \case
          '\\' -> "\\\\"
          '"' -> "\\\""
          a -> [a]

multilineList :: [Exp] -> Exp
multilineList =
  \case
    [] ->
      Exp False False "[]"
    a : b ->
      Exp False True $
        mconcat
          [ "[ ",
            B.indent 2 (ungroupedExp a <> foldMap (mappend ",\n" . ungroupedExp) b),
            "\n]"
          ]

list :: [Exp] -> Exp
list exps =
  if all (\(Exp needsGrouping isMultiline _) -> not (needsGrouping || isMultiline)) exps
    then
      Exp False False $
        mconcat
          [ "[",
            B.intercalate ", " (fmap ungroupedExp exps),
            "]"
          ]
    else multilineList exps

appChain :: Exp -> [Exp] -> Exp
appChain fn params =
  if all (not . isMultiline) params && not (isMultiline fn)
    then
      Exp True False $
        ungroupedExp fn <> foldMap (mappend " " . groupedExp) params
    else
      Exp True True $
        groupedExp fn <> B.indent 2 (foldMap (mappend "\n" . groupedExp) params)

multilinePostAppChain :: Exp -> [Exp] -> Exp
multilinePostAppChain baseExp chain =
  Exp True True $
    groupedExp baseExp <> B.indent 2 (foldMap (mappend "\n& " . B.indent 4 . groupedExp) chain)

-- * --

apChain ::
  -- | Prelude namespace.
  Text ->
  Exp ->
  [Exp] ->
  Exp
apChain preludeNs constructor params =
  case params of
    [] ->
      if isMultiline constructor
        then
          Exp True True $
            ungroupedExp (reference preludeNs "pure") <> B.indent 2 ("\n" <> groupedExp constructor)
        else
          Exp True False $
            ungroupedExp (reference preludeNs "pure") <> " " <> groupedExp constructor
    [param]
      | not (isMultiline constructor || isMultiline param) ->
          Exp True False $
            groupedExp constructor <> " <$> " <> groupedExp param
    _ ->
      Exp True True $
        groupedExp constructor
          <> B.indent 2 ("\n<$> " <> B.intercalate "\n<*> " (fmap (B.indent 4 . groupedExp) params))

alternatives ::
  -- | Prelude namespace.
  Text ->
  [Exp] ->
  Exp
alternatives preludeNs = \case
  [] -> reference preludeNs "empty"
  [a] -> a
  [a, b] -> infixBinOp "<|>" a b
  alternatives -> appChain (reference preludeNs "asum") [list alternatives]
