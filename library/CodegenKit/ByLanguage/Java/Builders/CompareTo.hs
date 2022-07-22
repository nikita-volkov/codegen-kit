module CodegenKit.ByLanguage.Java.Builders.CompareTo where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude

data Component = Component
  { componentStatements :: Builder -> Builder -> Builder
  }

-- * General

primitive :: Component
primitive =
  Component $ \leftExp rightExp ->
    [j|
      status = ($leftExp < $rightExp) ? -1 : (($leftExp == $rightExp) ? 0 : 1);
      if (status != 0) return status;
    |]

comparable :: Component
comparable =
  Component $ \leftExp rightExp ->
    [j|
      status = $leftExp.compareTo($rightExp);
      if (status != 0) return status;
    |]

-- * Specific Primitives

boolean :: Component
boolean =
  Component $ \leftExp rightExp ->
    [j|
      status = Boolean.compare($leftExp, $rightExp);
      if (status != 0) return status;
    |]

int :: Component
int =
  Component $ \leftExp rightExp ->
    [j|
      status = Integer.compare($leftExp, $rightExp);
      if (status != 0) return status;
    |]

long :: Component
long =
  Component $ \leftExp rightExp ->
    [j|
      status = Long.compare($leftExp, $rightExp);
      if (status != 0) return status;
    |]

-- * Optionals

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
