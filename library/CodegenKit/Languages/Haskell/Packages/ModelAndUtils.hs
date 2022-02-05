module CodegenKit.Languages.Haskell.Packages.ModelAndUtils where

import qualified Coalmine.MultilineTextBuilder as B
import qualified Coalmine.SimplePaths as Paths
import qualified CodegenKit.Languages.Haskell.Contents.Model as Model
import qualified CodegenKit.Languages.Haskell.Contents.ModelAccessors as ModelAccessors
import CodegenKit.PackageAssembly
import CodegenKit.Prelude
import qualified TextBuilder as B'

-- *

package ns =
  mconcat
    [ m "Model" model,
      m "ModelAccessors" modelAccessors
    ]
  where
    m modName contents =
      fromFile
        (Paths.inDir packageModulesDir $ fromString $ modName <> ".hs")
        contents
    packageModulesDir =
      fromString . toString $ foldMap (flip mappend "/") ns
    packageNamespace =
      fromString . toString $ B'.intercalate "." ns
    model =
      Model.content packageNamespace sections
      where
        sections =
          error "TODO"
    modelAccessors =
      ModelAccessors.content
        packageNamespace
        hasFieldConfigs
        hasVariantConfigs
      where
        hasFieldConfigs =
          error "TODO"
        hasVariantConfigs =
          error "TODO"
