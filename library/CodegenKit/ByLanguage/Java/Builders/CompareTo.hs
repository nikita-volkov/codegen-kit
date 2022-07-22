module CodegenKit.ByLanguage.Java.Builders.CompareTo where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude

-- * Splicing

compareTo :: Builder -> [Component] -> Builder
compareTo className components =
  [j|
    public int compareTo($className that) {
      int status;$statements
      return 0;
    }
  |]
  where
    statements =
      foldMap (mappend "\n" . componentStatements) components

-- * Component

data Component = Component
  { componentStatements :: Builder
  }

component :: Builder -> Type -> Component
component fieldName Type {..} =
  Component
    ( typeStatements
        [j|this.$fieldName|]
        [j|that.$fieldName|]
    )

-- * Type

data Type = Type
  { typeStatements :: Builder -> Builder -> Builder
  }

-- * General

primitive :: Type
primitive =
  Type $ \leftExp rightExp ->
    [j|
      status = ($leftExp < $rightExp) ? -1 : (($leftExp == $rightExp) ? 0 : 1);
      if (status != 0) return status;
    |]

comparable :: Type
comparable =
  Type $ \leftExp rightExp ->
    [j|
      status = $leftExp.compareTo($rightExp);
      if (status != 0) return status;
    |]

-- * Specific Primitives

boolean :: Type
boolean =
  Type $ \leftExp rightExp ->
    [j|
      status = Boolean.compare($leftExp, $rightExp);
      if (status != 0) return status;
    |]

byte :: Type
byte =
  Type $ \leftExp rightExp ->
    [j|
      status = Byte.compare($leftExp, $rightExp);
      if (status != 0) return status;
    |]

short :: Type
short =
  Type $ \leftExp rightExp ->
    [j|
      status = Short.compare($leftExp, $rightExp);
      if (status != 0) return status;
    |]

int :: Type
int =
  Type $ \leftExp rightExp ->
    [j|
      status = Integer.compare($leftExp, $rightExp);
      if (status != 0) return status;
    |]

long :: Type
long =
  Type $ \leftExp rightExp ->
    [j|
      status = Long.compare($leftExp, $rightExp);
      if (status != 0) return status;
    |]

float :: Type
float =
  Type $ \leftExp rightExp ->
    [j|
      status = Float.compare($leftExp, $rightExp);
      if (status != 0) return status;
    |]

double :: Type
double =
  Type $ \leftExp rightExp ->
    [j|
      status = Double.compare($leftExp, $rightExp);
      if (status != 0) return status;
    |]

-- * Optionals

optional :: Type -> Type
optional Type {..} =
  Type statements
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
          typeStatements
            [j|$leftExp.get()|]
            [j|$rightExp.get()|]

optionalInt :: Type
optionalInt =
  Type statements
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
          typeStatements
            int
            [j|$leftExp.getAsInt()|]
            [j|$rightExp.getAsInt()|]

optionalLong :: Type
optionalLong =
  Type statements
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
          typeStatements
            long
            [j|$leftExp.getAsLong()|]
            [j|$rightExp.getAsLong()|]

optionalDouble :: Type
optionalDouble =
  Type statements
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
          typeStatements
            double
            [j|$leftExp.getAsDouble()|]
            [j|$rightExp.getAsDouble()|]
