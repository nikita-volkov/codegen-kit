-- | Small framework for automation of import-management.
module CodegenKit.ByLanguage.Haskell.ModuleBuilder
  ( compileModule,
    Body,
    imported,
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
compileModule moduleName unqualifiedImports aliasMapList (Body requestedImports compileBody) =
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
    bodySplice =
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
  { -- | Requested qualified imports.
    bodyRequestedImports :: !(Acc Text),
    -- | Given a function aliasing a qualified module, compile into a splice.
    bodyCompiler :: !((Text -> Text) -> Builder)
  }

instance Semigroup Body where
  Body lImports lCompile <> Body rImports rCompile =
    Body
      (lImports <> rImports)
      ( \resolveModule ->
          lCompile resolveModule <> rCompile resolveModule
      )

instance Monoid Body where
  mempty = Body mempty mempty

instance IsString Body where
  fromString = splice . fromString

imported ::
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  Body
imported moduleRef memberName =
  Body
    (pure moduleRef)
    ( \resolveModule ->
        case resolveModule moduleRef of
          prefix ->
            if Text.null prefix
              then to memberName
              else to prefix <> "." <> to memberName
    )

splice :: Builder -> Body
splice builder =
  Body mempty (const builder)

indent :: Int -> Body -> Body
indent spaces (Body requestedImports compileBody) =
  Body requestedImports (Builder.indent spaces . compileBody)

intercalate :: Builder -> [Body] -> Body
intercalate separator bodies =
  Body
    (foldMap bodyRequestedImports bodies)
    ( \resolveModule ->
        Builder.intercalate
          separator
          (fmap (($ resolveModule) . bodyCompiler) bodies)
    )
