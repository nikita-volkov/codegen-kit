module CodegenKit.Haskell.Contexts.Code where

import Coalmine.Prelude
import CodegenKit.Haskell.Contexts.Package qualified as Package

toPackageModule :: ModuleConfig -> Code -> Package.Module
toPackageModule =
  error "TODO"

toModuleFile :: Preferences -> ModuleConfig -> Code -> Fileset
toModuleFile =
  error "TODO"

toModuleText :: Preferences -> ModuleConfig -> Code -> Text
toModuleText =
  error "TODO"

toHeadlessText :: Preferences -> Code -> Text
toHeadlessText =
  error "TODO"

data ModuleConfig = ModuleConfig
  { -- | Components of the namespace including the module name.
    namespace :: [Text],
    -- | Qualified import alias map.
    -- If a requested import is not present in it,
    -- it will be imported unqualified.
    importAliases :: [(Text, Text)]
  }

data Preferences = Preferences
  { strictData :: Bool,
    overloadedRecordDot :: Bool
  }

data Code
