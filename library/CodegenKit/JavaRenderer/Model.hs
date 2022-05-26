module CodegenKit.JavaRenderer.Model where

import CodegenKit.Prelude
import qualified Domain
import qualified DomainOptics

Domain.declare
  (Just (False, True))
  ( mconcat
      [ Domain.enumDeriver,
        Domain.boundedDeriver,
        Domain.showDeriver,
        Domain.eqDeriver,
        Domain.ordDeriver,
        Domain.genericDeriver,
        Domain.accessorIsLabelDeriver,
        Domain.constructorIsLabelDeriver
      ]
  )
  =<< Domain.loadSchema "domain/java-renderer.domain.yaml"
