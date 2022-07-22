module CodegenKit.ByLanguage.Java.Builders.CompareTo where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude

data Component = Component
  { componentStatements :: Builder -> Builder -> Builder
  }

optional :: Component -> Component
optional Component {..} =
  Component statements
  where
    statements leftExp rightExp =
      [j|
        if ($leftExp.isPresent()) {
          if ($rightExp.isPresent()) {
            $substatements
          } else return 1;
        } else {
          if ($rightExp.isPresent()) return -1;
        }
      |]
      where
        substatements =
          componentStatements
            [j|$leftExp.get()|]
            [j|$rightExp.get()|]
