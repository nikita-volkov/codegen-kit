module CodegenKit.HaskellPackage.Contexts.Type where

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
