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

    -- ** Composite
    customObjectType,
    arrayType,
    optionalType,

    -- ** Specific
    booleanType,
    byteType,
    shortType,
    intType,
    longType,
    floatType,
    doubleType,
    stringType,
    dateType,
    timeType,
    timestampType,
    bigDecimalType,
    uuidType,

    -- ** Special optionals
    optionalIntType,
    optionalLongType,
    optionalDoubleType,
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
    [i|public final $typeSignature $valueNameBuilder;|]
    [i|this.$valueNameBuilder = $valueNameBuilder;|]
    [i|$typeSignature $valueNameBuilder|]
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
  { typeSignature :: Builder,
    typeBoxedSignature :: Builder,
    typeEqualsField :: Text -> EqualsBuilder.Field,
    typeHashCodeField :: Builder -> HashCodeBuilder.Field,
    typeToJsonFieldType :: ToJsonBuilder.FieldType,
    typeImports :: [Text]
  }

-- ** Combinators

customObjectType ::
  -- | Signature.
  Builder ->
  -- | To JSON converter.
  ToJsonBuilder.FieldType ->
  -- | Imports.
  [Text] ->
  Type
customObjectType signature toJsonFieldType imports =
  Type
    signature
    signature
    EqualsBuilder.objectField
    HashCodeBuilder.objectField
    toJsonFieldType
    imports

arrayType :: Type -> Type
arrayType Type {..} =
  Type
    [j|$typeSignature[]|]
    [j|$typeSignature[]|]
    EqualsBuilder.arrayField
    HashCodeBuilder.arrayField
    (ToJsonBuilder.arrayFieldType typeToJsonFieldType)
    typeImports

optionalType :: Type -> Type
optionalType Type {..} =
  Type
    [j|Optional<$typeBoxedSignature>|]
    [j|Optional<$typeBoxedSignature>|]
    EqualsBuilder.objectField
    HashCodeBuilder.objectField
    (ToJsonBuilder.optionalFieldType typeToJsonFieldType)
    ("java.util.Optional" : typeImports)

-- ** Specific

booleanType :: Type
booleanType =
  Type
    "boolean"
    "Boolean"
    EqualsBuilder.primitiveField
    HashCodeBuilder.booleanField
    ToJsonBuilder.booleanFieldType
    []

byteType :: Type
byteType =
  Type
    "byte"
    "Byte"
    EqualsBuilder.primitiveField
    HashCodeBuilder.byteField
    ToJsonBuilder.byteFieldType
    []

shortType :: Type
shortType =
  Type
    "short"
    "Short"
    EqualsBuilder.primitiveField
    HashCodeBuilder.shortField
    ToJsonBuilder.shortFieldType
    []

intType :: Type
intType =
  Type
    "int"
    "Integer"
    EqualsBuilder.primitiveField
    HashCodeBuilder.intField
    ToJsonBuilder.intFieldType
    []

longType :: Type
longType =
  Type
    "long"
    "Long"
    EqualsBuilder.primitiveField
    HashCodeBuilder.longField
    ToJsonBuilder.longFieldType
    []

floatType :: Type
floatType =
  Type
    "float"
    "Float"
    EqualsBuilder.primitiveField
    HashCodeBuilder.floatField
    ToJsonBuilder.floatFieldType
    []

doubleType :: Type
doubleType =
  Type
    "double"
    "Double"
    EqualsBuilder.primitiveField
    HashCodeBuilder.doubleField
    ToJsonBuilder.doubleFieldType
    []

stringType :: Type
stringType =
  Type
    "String"
    "String"
    EqualsBuilder.objectField
    HashCodeBuilder.objectField
    ToJsonBuilder.stringFieldType
    []

dateType :: Type
dateType =
  customObjectType
    "Date"
    ToJsonBuilder.dateFieldType
    ["java.sql.Date"]

timeType :: Type
timeType =
  customObjectType
    "Time"
    ToJsonBuilder.timeFieldType
    ["java.sql.Time"]

timestampType :: Type
timestampType =
  customObjectType
    "Timestamp"
    ToJsonBuilder.timestampFieldType
    ["java.sql.Timestamp"]

bigDecimalType :: Type
bigDecimalType =
  customObjectType
    "BigDecimal"
    ToJsonBuilder.bigDecimalFieldType
    ["java.math.BigDecimal"]

uuidType :: Type
uuidType =
  customObjectType
    "UUID"
    ToJsonBuilder.uuidFieldType
    ["java.util.UUID"]

-- ** Special optionals

optionalIntType :: Type
optionalIntType =
  Type
    "OptionalInt"
    "OptionalInt"
    EqualsBuilder.objectField
    HashCodeBuilder.objectField
    ToJsonBuilder.optionalIntFieldType
    ["java.util.OptionalInt"]

optionalLongType :: Type
optionalLongType =
  Type
    "OptionalLong"
    "OptionalLong"
    EqualsBuilder.objectField
    HashCodeBuilder.objectField
    ToJsonBuilder.optionalLongFieldType
    ["java.util.OptionalLong"]

optionalDoubleType :: Type
optionalDoubleType =
  Type
    "OptionalDouble"
    "OptionalDouble"
    EqualsBuilder.objectField
    HashCodeBuilder.objectField
    ToJsonBuilder.optionalDoubleFieldType
    ["java.util.OptionalDouble"]
