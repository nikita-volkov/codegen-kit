module CodegenKit.ByLanguage.Java.Builders.HashCode
  ( -- * --
    HashCodeSnippets (..),
    hashCodeSnippets,

    -- * Field
    Field,
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
import qualified CodegenKit.ByLanguage.Java.Builders.HashCode.Snippets as Snippets
import CodegenKit.Prelude
import qualified Data.Set as Set

-- * --

data HashCodeSnippets = HashCodeSnippets
  { hashCodeSnippetsImports :: Set Text,
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
        (Snippets.classHashStaticProperty className)
        (Snippets.unitHashCodeMethod)
    _ ->
      HashCodeSnippets
        (Set.unions . fmap fieldImports $ fields)
        (Snippets.classHashStaticProperty className)
        (Snippets.statementsHashCodeMethod . fmap fieldHashUpdateStatements $ fields)

-- * Field

data Field = Field
  { fieldImports :: Set Text,
    fieldHashUpdateStatements :: Builder
  }

addFieldImport :: Text -> Field -> Field
addFieldImport import_ Field {..} =
  Field
    (Set.insert import_ fieldImports)
    fieldHashUpdateStatements

hashExpField :: Builder -> Field
hashExpField hashExp =
  Field
    mempty
    [j|hash = (hash << 5) - hash + $hashExp;|]

byteField :: Builder -> Field
byteField name =
  hashExpField [j|(int) this.$name|]

shortField :: Builder -> Field
shortField name =
  hashExpField [j|(int) this.$name|]

intField :: Builder -> Field
intField name =
  hashExpField [j|this.$name|]

longField :: Builder -> Field
longField name =
  hashExpField [j|(int) (this.$name ^ (this.$name >>> 32))|]

floatField :: Builder -> Field
floatField name =
  hashExpField [j|Float.floatToIntBits(this.$name)|]

doubleField :: Builder -> Field
doubleField name =
  Field
    mempty
    [j|
      {
        long bits = Double.doubleToLongBits(this.$name);
        hash = (hash << 5) - hash + (int) (bits ^ (bits >>> 32));
      }
    |]

booleanField :: Builder -> Field
booleanField name =
  hashExpField [j|(this.$name ? 1231 : 1237)|]

charField :: Builder -> Field
charField name =
  hashExpField [j|(int) this.$name|]

arrayField :: Builder -> Field
arrayField name =
  addFieldImport "java.util.Arrays" $
    hashExpField [j|Arrays.hashCode(this.$name)|]

objectField :: Builder -> Field
objectField name =
  hashExpField [j|this.$name.hashCode()|]

nullCheckedObjectField :: Builder -> Field
nullCheckedObjectField name =
  Field
    mempty
    [j|
      if (this.$name == null) {
        hash = (hash << 5) - hash;
      } else {
        hash = (hash << 5) - hash + 1;
        hash = (hash << 5) - hash + this.$name.hashCode();
      }
    |]
