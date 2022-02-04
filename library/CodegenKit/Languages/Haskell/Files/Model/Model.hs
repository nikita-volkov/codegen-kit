module CodegenKit.Languages.Haskell.Files.Model.Model where

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
  =<< Domain.loadSchema "domain/haskell-model.domain.yaml"
