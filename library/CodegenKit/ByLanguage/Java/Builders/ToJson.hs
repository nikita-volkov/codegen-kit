module CodegenKit.ByLanguage.Java.Builders.ToJson
  ( -- * --
    snippetsMethodDecl,

    -- * --
    Snippets,
    snippets,

    -- * --
    Field,
    field,

    -- * --
    FieldType,
    booleanFieldType,
    shortFieldType,
    intFieldType,
    longFieldType,
    floatFieldType,
    doubleFieldType,
    bigDecimalFieldType,
    stringFieldType,
    dateFieldType,
    uuidFieldType,
    optionalFieldType,
    optionalIntFieldType,
    optionalLongFieldType,
    optionalDoubleFieldType,
    customToStringFieldType,
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
      List.mapIntercalate
        fieldStatements
        "\nbuilder.append(',');\n"
        fields

-- * --

data Field = Field
  { fieldStatements :: Builder
  }

field :: Builder -> FieldType -> Field
field name (FieldType _ statements) =
  Field
    [j|
      builder.append("\"$name\":");
      $appliedStatements
    |]
  where
    appliedStatements =
      statements name [j|this.$name|]

-- * --

data FieldType = FieldType
  { fieldBoxedType :: Builder,
    fieldTypeStatements :: Builder -> Builder -> Builder
  }

primitiveFieldType :: Builder -> FieldType
primitiveFieldType type_ =
  FieldType
    type_
    (\_ ref -> [j|builder.append($ref);|])

booleanFieldType :: FieldType
booleanFieldType = primitiveFieldType "Boolean"

shortFieldType :: FieldType
shortFieldType = primitiveFieldType "Short"

intFieldType :: FieldType
intFieldType = primitiveFieldType "Integer"

longFieldType :: FieldType
longFieldType = primitiveFieldType "Long"

floatFieldType :: FieldType
floatFieldType = primitiveFieldType "Float"

doubleFieldType :: FieldType
doubleFieldType = primitiveFieldType "Double"

bigDecimalFieldType :: FieldType
bigDecimalFieldType =
  FieldType "BigDecimal" $ \name ref ->
    [j|
      builder.append('"');
      builder.append($ref.toPlainString());
      builder.append('"');
    |]

stringFieldType :: FieldType
stringFieldType =
  FieldType "String" $ \name ref ->
    [j|
      builder.append('"');
      builder.append($ref.replace("\\", "\\\\").replace("\"", "\\\"").replace("\b", "\\b").replace("\f", "\\f").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t"));
      builder.append('"');
    |]

dateFieldType :: FieldType
dateFieldType =
  FieldType "Date" $ \name ref ->
    [j|
      builder.append('"');
      builder.append($ref.toString());
      builder.append('"');
    |]

uuidFieldType :: FieldType
uuidFieldType =
  FieldType "UUID" $ \name ref ->
    [j|
      builder.append('"');
      builder.append($ref.toString());
      builder.append('"');
    |]

optionalFieldType :: FieldType -> FieldType
optionalFieldType (FieldType elementType elementStatements) =
  FieldType [j|Optional<$elementType>|] statements
  where
    statements name ref =
      [j|
        if ($ref.isPresent()) {
          $elementType $elementName = $ref.get();
          $appliedElementStatements
        } else {
          builder.append("null");
        }
      |]
      where
        elementName =
          name <> "Present"
        appliedElementStatements =
          elementStatements elementName elementName

optionalIntFieldType :: FieldType
optionalIntFieldType =
  FieldType "OptionalInt" statements
  where
    statements name ref =
      [j|
        if ($ref.isPresent()) {
          builder.append($ref.getAsInt());
        } else {
          builder.append("null");
        }
      |]

optionalLongFieldType :: FieldType
optionalLongFieldType =
  FieldType "OptionalLong" statements
  where
    statements name ref =
      [j|
        if ($ref.isPresent()) {
          builder.append($ref.getAsLong());
        } else {
          builder.append("null");
        }
      |]

optionalDoubleFieldType :: FieldType
optionalDoubleFieldType =
  FieldType "OptionalDouble" statements
  where
    statements name ref =
      [j|
        if ($ref.isPresent()) {
          builder.append($ref.getAsDouble());
        } else {
          builder.append("null");
        }
      |]

customToStringFieldType :: Builder -> FieldType
customToStringFieldType signature =
  FieldType signature $ \name ref ->
    [j|
      builder.append('"');
      builder.append($ref.toString().replace("\\", "\\\\").replace("\"", "\\\"").replace("\b", "\\b").replace("\f", "\\f").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t"));
      builder.append('"');
    |]
