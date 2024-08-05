module CodegenKit.HaskellPackage.Contexts.Type where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.Prelude
import CodegenKit.HaskellPackage.Contexts.Code qualified as Code

toGroupedCode :: Type -> Code.Code
toGroupedCode x =
  if x.needsGrouping
    then
      if x.isMultiline
        then "( " <> Code.indent 2 x.content <> "\n)"
        else "(" <> x.content <> ")"
    else x.content

toUngroupedCode :: Type -> Code.Code
toUngroupedCode x =
  x.content

data Type = Type
  { needsGrouping :: Bool,
    isMultiline :: Bool,
    content :: Code.Code
  }

localSymbol :: Text -> Type
localSymbol text =
  Type
    { needsGrouping = False,
      isMultiline = False,
      content = Code.splice (from text)
    }

-- | Produce code with a symbol reference that is determined based on the imports and produces requirements for them.
importedSymbol ::
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  Type
importedSymbol moduleRef memberName =
  Type
    { needsGrouping = False,
      isMultiline = False,
      content = Code.importedSymbol moduleRef memberName
    }

importedSymbolWithDependency ::
  -- | Package name.
  Text ->
  -- | Min inclusive version first position.
  Word ->
  -- | Min inclusive version remaining positions.
  [Word] ->
  -- | Max exclusive version first position.
  Word ->
  -- | Max exclusive version remaining positions.
  [Word] ->
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  Type
importedSymbolWithDependency packageName minHead minTail maxHead maxTail moduleRef memberName =
  Type
    { needsGrouping = False,
      isMultiline = False,
      content =
        Code.dependency packageName minHead minTail maxHead maxTail
          <> Code.importedSymbol moduleRef memberName
    }

chainApplication :: Type -> [Type] -> Type
chainApplication function params =
  if all (not . (.isMultiline)) params && not function.isMultiline
    then
      Type
        { needsGrouping = True,
          isMultiline = False,
          content =
            toUngroupedCode function
              <> foldMap (mappend " " . toGroupedCode) params
        }
    else
      Type
        { needsGrouping = True,
          isMultiline = False,
          content =
            toGroupedCode function
              <> Code.indent 2 (foldMap (mappend "\n" . toGroupedCode) params)
        }

binApplication :: Type -> Type -> Type
binApplication function param =
  if function.isMultiline && param.isMultiline
    then
      Type
        { needsGrouping = True,
          isMultiline = True,
          content =
            function.content <> Code.indent 2 ("\n" <> toGroupedCode param)
        }
    else
      Type
        { needsGrouping = True,
          isMultiline = False,
          content =
            function.content <> " " <> toGroupedCode param
        }

tuple :: [Type] -> Type
tuple params =
  if any (.isMultiline) params
    then
      Type
        { needsGrouping = False,
          isMultiline = True,
          content =
            case params of
              head : tail ->
                mconcat
                  [ "( ",
                    Code.indent 2 (toUngroupedCode head <> foldMap (mappend ",\n" . toUngroupedCode) tail),
                    "\n)"
                  ]
              _ -> "()"
        }
    else
      Type
        { needsGrouping = False,
          isMultiline = False,
          content =
            mconcat
              [ "(",
                List.mapIntercalate (.content) ", " params,
                ")"
              ]
        }
