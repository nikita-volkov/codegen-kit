module CodegenKit.Languages.Haskell.Packages.ModelAndUtils.Config where

import CodegenKit.Prelude
import qualified Domain

Domain.declare
  Nothing
  ( mconcat
      [ Domain.enumDeriver,
        Domain.boundedDeriver,
        Domain.showDeriver,
        Domain.eqDeriver,
        Domain.ordDeriver,
        Domain.hashableDeriver,
        Domain.genericDeriver,
        Domain.accessorIsLabelDeriver,
        Domain.constructorIsLabelDeriver
      ]
  )
  =<< Domain.loadSchema "domain/haskell-model-config.domain.yaml"
