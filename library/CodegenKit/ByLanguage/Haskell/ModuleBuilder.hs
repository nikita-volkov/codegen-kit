-- | Small framework for automation of import-management.
module CodegenKit.ByLanguage.Haskell.ModuleBuilder
  ( compileModule,
    Body,
    qualifiedRef,
    splice,
    indent,
    intercalate,
  )
where

import Coalmine.MultilineTextBuilder (Builder)
import qualified Coalmine.MultilineTextBuilder as Builder
import CodegenKit.Prelude hiding (intercalate)
import qualified Data.HashMap.Strict as HashMap

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
    aliasHashMap =
      HashMap.fromList aliasMapList
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
          where
            compileImport ref =
              case HashMap.lookup ref aliasHashMap of
                Just alias ->
                  [j|import qualified $ref as $alias|]
                Nothing ->
                  [j|import qualified $ref|]
        compiledUnqualifiedImportList =
          unqualifiedImports
            & nubSort
            & fmap compileImport
          where
            compileImport ref =
              [j|import $ref|]
    bodySplice =
      compileBody resolveModule
      where
        resolveModule ref =
          case HashMap.lookup ref aliasHashMap of
            Just alias -> alias
            Nothing -> ref

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

qualifiedRef ::
  -- | Fully qualified module reference.
  Text ->
  -- | Member name.
  Text ->
  Body
qualifiedRef moduleRef memberName =
  Body
    (pure moduleRef)
    ( \resolveModule ->
        to (resolveModule moduleRef) <> "." <> to memberName
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
