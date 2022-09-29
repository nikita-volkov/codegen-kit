-- | Small framework for automation of import-management.
module CodegenKit.ByLanguage.Haskell.ModuleBuilder
  ( compileModule,
    Body,
    importing,
    splice,
    indent,
    intercalate,
  )
where

import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as Builder
import CodegenKit.Prelude hiding (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

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
    importsSplice =
      Builder.intercalate "\n" compiledImportList
      where
        compiledImportList =
          compiledUnqualifiedImportList <> compiledQualifiedImportList
        compiledQualifiedImportList =
          requestedImports
            & toList
            & nubSort
            & fmap compileImport
            & join
          where
            compileImport imported =
              if Set.member imported unqualifiedSet
                then []
                else pure $ case Map.lookup imported aliasMap of
                  Just alias ->
                    [j|import qualified $imported as $alias|]
                  Nothing ->
                    [j|import qualified $imported|]
        compiledUnqualifiedImportList =
          unqualifiedImports
            & nubSort
            & fmap compileImport
          where
            compileImport imported =
              [j|import $imported|]
    (bodySplice, requestedImports) =
      compileBody resolveModule
      where
        resolveModule imported =
          if Set.member imported unqualifiedSet
            then ""
            else case Map.lookup imported aliasMap of
              Just alias -> alias
              Nothing -> imported

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
