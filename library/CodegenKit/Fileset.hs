module CodegenKit.Fileset
  ( -- * --
    Fileset,

    -- ** Execution
    write,
    print,

    -- ** --
    file,
    inDir,
  )
where

import Coalmine.EvenSimplerPaths (Path)
import qualified Coalmine.EvenSimplerPaths as Paths
import qualified Coalmine.MultilineTextBuilder as B
import CodegenKit.Prelude hiding (print)
import qualified Data.Text.IO as TextIO

-- * --

-- |
-- Brings all variations of rendering to order by defining
-- package sets.
--
-- The abstraction.
newtype Fileset = Fileset [(Path, Text)]
  deriving (Semigroup, Monoid)

instance BroadPrinting Fileset where
  toBroadBuilder (Fileset package) =
    B.intercalate "\n\n" $ fmap renderFile package
    where
      renderFile (path, contents) =
        mconcat
          [ toBroadBuilder path,
            ":",
            B.indent 2 $ mappend "\n" $ to contents
          ]

-- * Execution

write :: Fileset -> IO ()
write (Fileset files) =
  traverse_ (uncurry writeFileCreatingDirs) files
  where
    writeFileCreatingDirs path contents = do
      Paths.createDirsTo path
      TextIO.writeFile (printCompactAsString path) contents

print :: Fileset -> IO ()
print = printBroadToStdOut

-- * --

file :: Path -> Text -> Fileset
file path content =
  Fileset [(path, content)]

-- |
-- Prepend a directory path to all contents of this package.
inDir :: Path -> Fileset -> Fileset
inDir path (Fileset contents) =
  Fileset $ fmap (first (mappend path)) contents
