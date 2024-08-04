module CodegenKit.HaskellPackage.Dsls.Package where

import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Fileset qualified as Fileset
import Coalmine.MultilineTextBuilder qualified as Splice
import Coalmine.Prelude
import CodegenKit.HaskellPackage.ComonadicContexts.Code qualified as Code

printPackage :: Package -> Text
printPackage =
  error "TODO"

writePackage :: Path -> Package -> IO ()
writePackage =
  error "TODO"

data Package

package ::
  -- | Name.
  Text ->
  -- | Synopsis.
  Text ->
  -- | Description.
  Text ->
  -- | Version.
  Version ->
  -- | Exposed modules.
  [Module] ->
  -- | Other modules.
  [Module] ->
  Package
package =
  error "TODO"

data Module
