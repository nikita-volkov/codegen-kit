module CodegenKit.Haskell.Contexts.Package where

import Coalmine.Prelude
import CodegenKit.Legacy.Dependencies qualified as Dependencies

data Component

data Module

module_ ::
  -- | Compiler of the module given the preferences.
  -- It does not neccessarily have to satisfy them.
  (Preferences -> CompiledModule) ->
  Module
module_ =
  error "TODO"

data Preferences = Preferences
  { strictData :: Bool,
    overloadedRecordDot :: Bool,
    importQualifiedPost :: Bool
  }

data CompiledModule = CompiledModule
  { path :: Path,
    name :: Text,
    requestedExtensions :: Set Text,
    requestedDependencies :: Dependencies.Dependencies,
    content :: Text
  }
  deriving (Show, Eq)
