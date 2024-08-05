module CodegenKit.HaskellPackage.Dsls.Package where

import Coalmine.Prelude

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
