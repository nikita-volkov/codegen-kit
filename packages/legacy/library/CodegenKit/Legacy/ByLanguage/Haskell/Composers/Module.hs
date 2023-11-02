-- | Small framework for automation of import-management.
module CodegenKit.Legacy.ByLanguage.Haskell.Composers.Module
  ( compileModule,
    Body,
    importing,
    importedSymbol,
    splice,
    indent,
    intercalate,
  )
where

import Coalmine.MultilineTextBuilder (Builder)
import Coalmine.MultilineTextBuilder qualified as Builder
import CodegenKit.Legacy.ByLanguage.Haskell.CodeTemplate qualified as CodeTemplate
import CodegenKit.Legacy.ByLanguage.Haskell.Templates.ImportsBlock qualified as ImportsBlockTemplate
import CodegenKit.Legacy.Prelude hiding (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

compileModule ::
  -- | Module name.
  Text ->
  -- | Unqualified imports.
  [Text] ->
  -- | Qualified import alias map.
  [(Text, Text)] ->
  -- | Module contents.
  Body ->
  Text
compileModule moduleName unqualifiedImports aliasMapList (Body compileBody) =
  [i|
    module $moduleName where

    $importsSplice

    $bodySplice
  |]
  where
    aliasMap =
      Map.fromList aliasMapList
    unqualifiedSet =
      Set.fromList unqualifiedImports
    (bodySplice, requestedImports) =
      compileBody resolveModule
      where
        resolveModule imported =
          if Set.member imported unqualifiedSet
            then ""
            else case Map.lookup imported aliasMap of
              Just alias -> alias
              Nothing -> imported
    importsSplice =
      CodeTemplate.compileCodeTemplate
        CodeTemplate.CodeStyle
          { importQualifiedPost = False,
            overloadedRecordDot = False,
            strictData = False
          }
        ImportsBlockTemplate.ImportsBlock
          { unqualified =
              unqualifiedImports
                & fmap
                  ( \qualifiedName ->
                      ImportsBlockTemplate.UnqualifiedImport {qualifiedName, symbols = Nothing}
                  ),
            qualified =
              requestedImports
                & toList
                & foldMap
                  ( \qualifiedName ->
                      if Set.member qualifiedName unqualifiedSet
                        then []
                        else pure $ case Map.lookup qualifiedName aliasMap of
                          Just alias ->
                            ImportsBlockTemplate.QualifiedImport
                              { qualifiedName,
                                alias
                              }
                          Nothing ->
                            ImportsBlockTemplate.QualifiedImport
                              { qualifiedName,
                                alias = ""
                              }
                  )
          }

-- | Module contents.
--
-- Automates over imports and qualification.
data Body = Body
  { -- | Given a function aliasing a qualified module, compile into a splice and a list of requested imports.
    bodyCompiler :: !((Text -> Text) -> (Builder, Acc Text))
  }

instance Semigroup Body where
  Body lCompile <> Body rCompile =
    Body
      ( \resolveModule ->
          lCompile resolveModule <> rCompile resolveModule
      )

instance Monoid Body where
  mempty = Body mempty

instance IsString Body where
  fromString = splice . fromString

importing ::
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  (Text -> Body) ->
  Body
importing moduleRef memberName cont =
  Body $ \resolveModule ->
    let ref =
          case resolveModule moduleRef of
            "" -> memberName
            prefix -> mconcat [prefix, ".", memberName]
     in case cont ref of
          Body compileNested ->
            compileNested resolveModule
              & second (pure moduleRef <>)

-- | Produce code with a symbol reference that is determined based on the imports and produces requirements for them.
importedSymbol ::
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  Body
importedSymbol moduleRef memberName =
  importing moduleRef memberName $ splice . to

splice :: Builder -> Body
splice builder =
  Body (const (builder, mempty))

indent :: Int -> Body -> Body
indent spaces (Body compileBody) =
  Body (first (Builder.indent spaces) . compileBody)

intercalate :: Builder -> [Body] -> Body
intercalate separator bodies =
  Body
    ( \resolveModule ->
        case unzip (fmap (($ resolveModule) . bodyCompiler) bodies) of
          (splices, requestedImports) ->
            ( Builder.intercalate separator splices,
              mconcat requestedImports
            )
    )
