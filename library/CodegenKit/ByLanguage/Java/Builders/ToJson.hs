module CodegenKit.ByLanguage.Java.Builders.ToJson
  ( -- * --
    snippetsMethodDecl,

    -- * --
    Snippets,
    snippets,

    -- * --
    Field,
    intField,
    longField,
    floatField,
    doubleField,
    stringField,
    dateField,
    optionalField,
    optionalIntField,
    optionalLongField,
    optionalDoubleField,
  )
where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude
import qualified Data.HashSet as HashSet

-- * --

data Snippets = Snippets
  { snippetsMethodDecl :: Builder
  }

snippets :: [Field] -> Snippets
snippets fields =
  Snippets
    [j|
      /**
       * Serialize to compact JSON representation.
       */
      public String toCompactJsonString() {
        StringBuilder builder = new StringBuilder();
        builder.append("{");
        $statements
        builder.append("}");
        return builder.toString();
      }
    |]
  where
    statements =
      List.intercalateMap
        fieldStatements
        "\nbuilder.append(',');\n"
        fields

-- * --

data Field = Field
  { fieldStatements :: Builder
  }

namedField :: Builder -> Builder -> Field
namedField name valueStatements =
  Field
    [j|
      builder.append("\"$name\":");
      $valueStatements
    |]

intField :: Builder -> Field
intField name =
  namedField
    name
    [j|
      builder.append(this.$name);
    |]

longField :: Builder -> Field
longField name =
  namedField
    name
    [j|
      builder.append(this.$name);
    |]

floatField :: Builder -> Field
floatField name =
  namedField
    name
    [j|
      builder.append(this.$name);
    |]

doubleField :: Builder -> Field
doubleField name =
  namedField
    name
    [j|
      builder.append(this.$name);
    |]

stringField :: Builder -> Field
stringField name =
  namedField
    name
    [j|
      builder.append('"');
      builder.append(this.$name.replace("\\", "\\\\").replace("\"", "\\\"").replace("\b", "\\b").replace("\f", "\\f").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t"));
      builder.append('"');
    |]

dateField :: Builder -> Field
dateField name =
  namedField
    name
    [j|
      builder.append(this.$name.toString());
    |]

optionalField :: (Builder -> Field) -> Builder -> Field
optionalField presentField name =
  namedField name $
    [j|
      if (this.$name.isPresent()) {
        ${name}Present = this.$name.get();
        $presentStatements
      } else {
        builder.append("null");
      }
    |]
  where
    presentStatements =
      fieldStatements $ presentField (name <> "Present")

optionalIntField :: Builder -> Field
optionalIntField name =
  namedField name $
    [j|
      if (this.$name.isPresent()) {
        builder.append(this.$name.getAsInt());
      } else {
        builder.append("null");
      }
    |]

optionalLongField :: Builder -> Field
optionalLongField name =
  namedField name $
    [j|
      if (this.$name.isPresent()) {
        builder.append(this.$name.getAsLong());
      } else {
        builder.append("null");
      }
    |]

optionalDoubleField :: Builder -> Field
optionalDoubleField name =
  namedField name $
    [j|
      if (this.$name.isPresent()) {
        builder.append(this.$name.getAsDouble());
      } else {
        builder.append("null");
      }
    |]
