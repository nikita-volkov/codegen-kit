module CodegenKit.PackageAssemblyDsl
  ( -- *
    Package,

    -- ** Execution
    write,
    print,

    -- **
    fromModule,
    inDir,

    -- *
    Module (..),
  )
where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.SimplePaths as Paths
import CodegenKit.Prelude hiding (inDir, print)
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Directory

-- *

-- |
-- Brings all variations of rendering to order by defining
-- package sets.
--
-- The abstraction.
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

-- |
-- Lift and resolve a module definition.
fromModule :: [Name] -> Module -> Package
fromModule ns (Module mod) =
  Package [mod ns]

-- |
-- Prepend a directory path to all contents of this package.
inDir :: DirPath -> Package -> Package
inDir path (Package contents) =
  Package $ fmap (first (Paths.inDir path)) contents

-- *

-- |
-- Module file-name and content generated given the namespace provided by the user.
newtype Module
  = -- |
    -- Function from namespace into filename and contents.
    Module ([Name] -> (FilePath, Text))
