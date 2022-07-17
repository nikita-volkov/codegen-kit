module CodegenKit.ByLanguage.Java.Builders.ProductClass
  ( -- * --
    fileset,

    -- * --
    Class,
    class_,

    -- * --
    ClassName,
    className,

    -- * --
    Field,
    field,

    -- * --
    FieldName,
    fieldName,

    -- * --
    Type,

    -- ** Predefined
    booleanType,
    byteType,
    shortType,
    intType,
    longType,
    floatType,
    doubleType,
    bigDecimalType,
    stringType,
    dateType,
    timeType,
    timestampType,
    uuidType,
    customObjectType,
  )
where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.EvenSimplerPaths (Path)
import qualified Coalmine.EvenSimplerPaths as Paths
import Coalmine.MultilineTextBuilder
import qualified Coalmine.Name as Name
import qualified CodegenKit.ByLanguage.Java.Builders.Equals as EqualsBuilder
import qualified CodegenKit.ByLanguage.Java.Builders.HashCode as HashCodeBuilder
import qualified CodegenKit.ByLanguage.Java.Builders.ToJson as ToJsonBuilder
import CodegenKit.Fileset (Fileset)
import qualified CodegenKit.Fileset as Fileset
import CodegenKit.Prelude hiding (intercalate)
import qualified Data.Text.IO as TextIO

-- * --

fileset :: Path -> Builder -> Class -> Fileset
fileset dirPath namespace Class {..} =
  Fileset.file path (to code)
  where
    path =
      dirPath <> classPath
    code =
      [j|
        package $namespace;

        $imports
        $classCode
      |]
      where
        imports =
          classImports
            & foldMap (flip mappend ";\n" . mappend "import ")

-- * --

data Class = Class
  { classPath :: Path,
    classCode :: Builder,
    classImports :: Set Text
  }

class_ ::
  -- | Class name.
  ClassName ->
  -- | Product fields.
  [Field] ->
  Class
class_ ClassName {..} fields =
  Class
    classNamePath
    fieldsClassCode
    imports
  where
    fieldsClassCode =
      [i|
        public final class $classNameCode {
          $propertyDecls

          $classNameCode($constructorArgs) {
            $propertyAssignments
          }

          $equalsMethods

          $hashCodeSnippetsHashCodeMethodDecls

          /**
           * Serialize to compact JSON representation.
           */
          public String toString() {
            return toCompactJsonString();
          }

          $toJsonMethods
          
          $hashCodeSnippetsClassHashPropertyDecl
        }
      |]
      where
        propertyDecls =
          List.mapIntercalate fieldPropertyDecl "\n" fields
        propertyAssignments =
          List.mapIntercalate fieldAssignment "\n" fields
        constructorArgs =
          List.mapIntercalate fieldArg ", " fields
        toJsonSnippets =
          ToJsonBuilder.snippets
            (fmap fieldToJsonField fields)
        toJsonMethods =
          toJsonSnippets & ToJsonBuilder.snippetsMethodDecl
        className = classNameCode & to
        equalsMethods =
          EqualsBuilder.equalsMethodsForProduct
            className
            (fmap fieldEqualsField fields)
    HashCodeBuilder.HashCodeSnippets {..} =
      HashCodeBuilder.hashCodeSnippets
        classNameCode
        (fmap fieldHashCodeField fields)
    imports =
      hashCodeSnippetsImports
        <> foldMap (fromList . fieldImports) fields

-- * --

data ClassName = ClassName
  { classNameCode :: Builder,
    classNamePath :: Path
  }

instance IsString ClassName where
  fromString = className . fromString

className :: Name -> ClassName
className className =
  ClassName text path
  where
    text = fromNameIn #upperCamel className
    path = Paths.addExtension "java" (fromNameIn #upperCamel className)

-- * --

data Field = Field
  { fieldPropertyDecl :: Builder,
    fieldAssignment :: Builder,
    fieldArg :: Builder,
    fieldHashCodeField :: HashCodeBuilder.Field,
    fieldToJsonField :: ToJsonBuilder.Field,
    fieldEqualsField :: EqualsBuilder.Field,
    fieldImports :: [Text]
  }

field :: FieldName -> Type -> Field
field FieldName {..} Type {..} =
  Field
    [i|public final $typeCode $valueNameBuilder;|]
    [i|this.$valueNameBuilder = $valueNameBuilder;|]
    [i|$typeCode $valueNameBuilder|]
    (typeHashCodeField valueNameBuilder)
    (ToJsonBuilder.field valueNameBuilder typeToJsonFieldType)
    (typeEqualsField valueNameText)
    typeImports

-- * --

data FieldName = FieldName
  { valueNameBuilder :: Builder,
    valueNameText :: Text
  }

instance IsString FieldName where
  fromString = fieldName . fromString

fieldName :: Name -> FieldName
fieldName name =
  FieldName (to text) text
  where
    text = fromNameIn #lowerCamel name

-- * --

data Type = Type
  { typeCode :: Builder,
    typeEqualsField :: Text -> EqualsBuilder.Field,
    typeHashCodeField :: Builder -> HashCodeBuilder.Field,
    typeToJsonFieldType :: ToJsonBuilder.FieldType,
    typeImports :: [Text]
  }

booleanType :: Bool -> Type
booleanType = \case
  False ->
    Type
      "boolean"
      EqualsBuilder.primitiveField
      HashCodeBuilder.booleanField
      ToJsonBuilder.booleanFieldType
      []
  True ->
    Type
      "Optional<Boolean>"
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      (ToJsonBuilder.optionalFieldType ToJsonBuilder.booleanFieldType)
      ["java.util.Optional"]

byteType :: Bool -> Type
byteType = \case
  False ->
    Type
      "byte"
      EqualsBuilder.primitiveField
      HashCodeBuilder.byteField
      ToJsonBuilder.byteFieldType
      []
  True ->
    Type
      "Optional<Byte>"
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      (ToJsonBuilder.optionalFieldType ToJsonBuilder.byteFieldType)
      ["java.util.Optional"]

shortType :: Bool -> Type
shortType = \case
  False ->
    Type
      "short"
      EqualsBuilder.primitiveField
      HashCodeBuilder.shortField
      ToJsonBuilder.shortFieldType
      []
  True ->
    Type
      "Optional<Short>"
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      (ToJsonBuilder.optionalFieldType ToJsonBuilder.shortFieldType)
      ["java.util.Optional"]

intType :: Bool -> Type
intType = \case
  False ->
    Type
      "int"
      EqualsBuilder.primitiveField
      HashCodeBuilder.intField
      ToJsonBuilder.intFieldType
      []
  True ->
    Type
      "OptionalInt"
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      ToJsonBuilder.optionalIntFieldType
      ["java.util.OptionalInt"]

longType :: Bool -> Type
longType = \case
  False ->
    Type
      "long"
      EqualsBuilder.primitiveField
      HashCodeBuilder.longField
      ToJsonBuilder.longFieldType
      []
  True ->
    Type
      "OptionalLong"
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      ToJsonBuilder.optionalLongFieldType
      ["java.util.OptionalLong"]

floatType :: Bool -> Type
floatType = \case
  False ->
    Type
      "float"
      EqualsBuilder.primitiveField
      HashCodeBuilder.floatField
      ToJsonBuilder.floatFieldType
      []
  True ->
    Type
      "Optional<Float>"
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      (ToJsonBuilder.optionalFieldType ToJsonBuilder.floatFieldType)
      ["java.util.Optional"]

doubleType :: Bool -> Type
doubleType = \case
  False ->
    Type
      "double"
      EqualsBuilder.primitiveField
      HashCodeBuilder.doubleField
      ToJsonBuilder.doubleFieldType
      []
  True ->
    Type
      "OptionalDouble"
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      ToJsonBuilder.optionalDoubleFieldType
      ["java.util.OptionalDouble"]

stringType :: Bool -> Type
stringType = \case
  False ->
    Type
      "String"
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      ToJsonBuilder.stringFieldType
      []
  True ->
    Type
      "Optional<String>"
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      (ToJsonBuilder.optionalFieldType ToJsonBuilder.stringFieldType)
      ["java.util.Optional"]

dateType :: Bool -> Type
dateType =
  customObjectType
    "Date"
    ToJsonBuilder.dateFieldType
    ["java.sql.Date"]

timeType :: Bool -> Type
timeType =
  customObjectType
    "Time"
    ToJsonBuilder.timeFieldType
    ["java.sql.Time"]

timestampType :: Bool -> Type
timestampType =
  customObjectType
    "Timestamp"
    ToJsonBuilder.timestampFieldType
    ["java.sql.Timestamp"]

customObjectType ::
  -- | Signature.
  Builder ->
  -- | To JSON converter.
  ToJsonBuilder.FieldType ->
  -- | Imports.
  [Text] ->
  -- | Optional.
  Bool ->
  Type
customObjectType signature toJsonFieldType imports = \case
  False ->
    Type
      signature
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      toJsonFieldType
      imports
  True ->
    Type
      [j|Optional<$signature>|]
      EqualsBuilder.objectField
      HashCodeBuilder.objectField
      (ToJsonBuilder.optionalFieldType toJsonFieldType)
      ("java.util.Optional" : imports)

bigDecimalType :: Bool -> Type
bigDecimalType =
  customObjectType
    "BigDecimal"
    ToJsonBuilder.bigDecimalFieldType
    ["java.math.BigDecimal"]

uuidType :: Bool -> Type
uuidType =
  customObjectType
    "UUID"
    ToJsonBuilder.uuidFieldType
    ["java.util.UUID"]
