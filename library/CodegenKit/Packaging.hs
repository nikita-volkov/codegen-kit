module CodegenKit.Packaging
  ( -- * --
    FileSet (..),

    -- ** Execution
    write,
    print,

    -- ** --
    fromFile,
    fromModule,
    inDir,

    -- * --
    Module (..),
  )
where

import qualified Coalmine.EvenSimplerPaths as Path
import qualified Coalmine.MultilineTextBuilder as B
import CodegenKit.Prelude hiding (inDir, print)
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Directory

-- * --

-- |
-- Brings all variations of rendering to order by defining
-- package sets.
--
-- The abstraction.
newtype FileSet = FileSet [(Path, Text)]
  deriving (Semigroup, Monoid)

instance BroadPrinting FileSet where
  toBroadBuilder (FileSet package) =
    B.intercalate "\n\n" $ fmap renderFile package
    where
      renderFile (path, contents) =
        mconcat
          [ toBroadBuilder path,
            ":",
            B.indent 2 $ mappend "\n" $ from @Text contents
          ]

-- * Execution

write :: FileSet -> IO ()
write (FileSet files) =
  traverse_ (uncurry writeFileCreatingDirs) files
  where
    writeFileCreatingDirs path contents = do
      forM_ (Path.parent path) $
        Directory.createDirectoryIfMissing True . printCompactAsString
      TextIO.writeFile (printCompactAsString path) contents

print :: FileSet -> IO ()
print = printBroadToStdOut

-- * --

fromFile :: Path -> Text -> FileSet
fromFile path content =
  FileSet [(path, content)]

-- |
-- Lift and resolve a module definition.
fromModule :: [Name] -> Module -> FileSet
fromModule ns (Module mod) =
  FileSet [mod ns]

-- |
-- Prepend a directory path to all contents of this package.
inDir :: Path -> FileSet -> FileSet
inDir path (FileSet contents) =
  FileSet $ fmap (first (mappend path)) contents

-- * --

-- |
-- Module file-name and content generated given the namespace provided by the user.
newtype Module
  = -- |
    -- Function from namespace into filename and contents.
    Module ([Name] -> (Path, Text))
