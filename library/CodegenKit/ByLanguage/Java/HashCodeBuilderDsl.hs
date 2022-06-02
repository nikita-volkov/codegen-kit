module CodegenKit.ByLanguage.Java.HashCodeBuilderDsl
  ( -- * --
    HashCodeSnippets (..),
    hashCodeSnippets,

    -- * Field
    Field,
    addFieldImport,
    hashExpField,
    byteField,
    shortField,
    intField,
    longField,
    floatField,
    doubleField,
    booleanField,
    charField,
    arrayField,
    objectField,
  )
where

import Coalmine.MultilineTextBuilder
import qualified CodegenKit.ByLanguage.Java.Code as Code
import CodegenKit.Prelude
import qualified Data.HashSet as HashSet

-- * Field

data HashCodeSnippets = HashCodeSnippets
  { hashCodeSnippetsImports :: HashSet Text,
    hashCodeSnippetsClassHashPropertyDecl :: Builder,
    hashCodeSnippetsHashCodeMethodDecls :: Builder
  }

hashCodeSnippets ::
  -- | Class name.
  Builder ->
  [Field] ->
  HashCodeSnippets
hashCodeSnippets className fields =
  case fields of
    [] ->
      HashCodeSnippets
        mempty
        (Code.classHashStaticProperty className)
        (Code.unitHashCodeMethod)
    _ ->
      HashCodeSnippets
        (HashSet.unions . fmap fieldImports $ fields)
        (Code.classHashStaticProperty className)
        (Code.statementsHashCodeMethod . fmap fieldHashUpdateStatements $ fields)

-- * Field

data Field = Field
  { fieldImports :: HashSet Text,
    fieldHashUpdateStatements :: Builder
  }

addFieldImport :: Text -> Field -> Field
addFieldImport import_ Field {..} =
  Field
    (HashSet.insert import_ fieldImports)
    fieldHashUpdateStatements

hashExpField :: Builder -> Field
hashExpField hashExp =
  Field
    mempty
    [j|hash = (hash << 5) - hash + $hashExp;|]

byteField :: Builder -> Field
byteField name =
  hashExpField [j|(int) $name|]

shortField :: Builder -> Field
shortField name =
  hashExpField [j|(int) $name|]

intField :: Builder -> Field
intField name =
  hashExpField name

longField :: Builder -> Field
longField name =
  hashExpField [j|(int) ($name ^ ($name >>> 32))|]

floatField :: Builder -> Field
floatField name =
  hashExpField [j|Float.floatToIntBits($name)|]

doubleField :: Builder -> Field
doubleField name =
  Field
    mempty
    [j|
      {
        long bits = Double.doubleToLongBits($name);
        hash = (hash << 5) - hash + (int) (bits ^ (bits >>> 32));
      }
    |]

booleanField :: Builder -> Field
booleanField name =
  hashExpField [j|$name ? 1231 : 1237|]

charField :: Builder -> Field
charField name =
  hashExpField [j|(int) $name|]

arrayField :: Builder -> Field
arrayField name =
  addFieldImport "java.util.Arrays" $
    hashExpField [j|Arrays.hashCode($name)|]

objectField :: Builder -> Field
objectField name =
  Field
    mempty
    [j|
      if ($name == null) {
        hash = (hash << 5) - hash;
      } else {
        hash = (hash << 5) - hash + $name.hashCode();
      }
    |]
