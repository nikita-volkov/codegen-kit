module CodegenKit.Haskell.Contexts.Package where

import Coalmine.Prelude

data Component

data Module

module_ ::
  -- | Namespace.
  [Text] ->
  -- | Qualified import alias map.
  -- If a requested import is not present in it,
  -- it will be imported unqualified.
  [(Text, Text)] ->
  -- | Printer of the file contents,
  -- given the preferences.
  -- It does not neccessarily have to satisfy them.
  (CodePreferences -> Text) ->
  Module
module_ =
  error "TODO"

data CodePreferences
