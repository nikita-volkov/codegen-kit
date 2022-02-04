-- |
-- Brings all variations of rendering to order by defining
-- package sets.
--
-- The abstraction.
module CodegenKit.Package
  ( -- *
    Package (..),

    -- * Execution
    write,
    print,

    -- *
    fromModuleList,
    inDir,
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.SimplePaths as Paths
import CodegenKit.Module (Module)
import qualified CodegenKit.Module as Module
import CodegenKit.Prelude hiding (inDir, print)
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Directory

-- *

newtype Package = Package [(FilePath, Text)]
  deriving (Semigroup, Monoid)

instance ToText Package where
  toText = toText . toMultilineTextBuilder

instance ToTextBuilder Package where
  toTextBuilder = toTextBuilder . toMultilineTextBuilder

instance ToMultilineTextBuilder Package where
  toMultilineTextBuilder (Package package) =
    B.intercalate "\n\n" $ fmap renderFile package
    where
      renderFile (path, contents) =
        mconcat
          [ toMultilineTextBuilder $ toTextBuilder path,
            ":",
            B.indent 2 $ mappend "\n" $ toMultilineTextBuilder contents
          ]

instance ToString Package where
  toString = toString . toMultilineTextBuilder

-- * Execution

write :: Package -> IO ()
write (Package files) =
  traverse_ (uncurry writeFileCreatingDirs) files
  where
    writeFileCreatingDirs path contents = do
      Directory.createDirectoryIfMissing True $ toString $ Paths.filePathDir path
      TextIO.writeFile (toString path) contents

print :: Package -> IO ()
print = TextIO.putStrLn . toText

-- *

fromModuleList :: [([Name], Module)] -> Package
fromModuleList =
  Package . fmap (\(ns, Module.Module mod) -> mod ns)

-- |
-- Prepend a directory path to all contents of this package.
inDir :: DirPath -> Package -> Package
inDir path (Package contents) =
  Package $ fmap (first (Paths.inDir path)) contents
