module CodegenKit.HaskellPackage.Contexts.Code.Templates.Module
  ( Params (..),
    Result,
    compile,
  )
where

import Coalmine.MultilineTextBuilder qualified as TextBlock
import Coalmine.Prelude

data Params = Params
  { moduleName :: TextBlock,
    exports :: Maybe [Text],
    imports :: TextBlock,
    content :: TextBlock
  }

type Result = Text

compile :: Params -> Result
compile params =
  case params.exports of
    Just exportList ->
      [i|
        module ${params.moduleName}
          ( $exportSplice
          )
        where${importsSplice}${contentSplice}
      |]
      where
        exportSplice =
          exportList
            & sort
            & fmap to
            & fmap (<> ",")
            & TextBlock.intercalate "\n"
            & TextBlock.indent 2
    Nothing ->
      [i|
        module ${params.moduleName} where${importsSplice}${contentSplice}
      |]
  where
    importsSplice =
      if TextBlock.null params.imports
        then ""
        else "\n\n" <> params.imports
    contentSplice =
      if TextBlock.null params.content
        then ""
        else "\n\n" <> params.content
