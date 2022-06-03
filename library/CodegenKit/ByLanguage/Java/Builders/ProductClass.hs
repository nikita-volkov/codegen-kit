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
    intType,
    longType,
    floatType,
    doubleType,
    stringType,
    dateType,
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

        import java.sql.Date;
        import java.sql.*;
        import java.util.*;

        $classCode
      |]

-- * --

data Class = Class
  { classPath :: Path,
    classCode :: Builder
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
          List.intercalateMap fieldPropertyDecl "\n" fields
        propertyAssignments =
          List.intercalateMap fieldAssignment "\n" fields
        constructorArgs =
          List.intercalateMap fieldArg ", " fields
        HashCodeBuilder.HashCodeSnippets {..} =
          HashCodeBuilder.hashCodeSnippets
            classNameCode
            (fmap fieldHashCodeField fields)
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
    fieldEqualsField :: EqualsBuilder.Field
  }

field :: FieldName -> Type -> Field
field FieldName {..} Type {..} =
  Field
    [i|public final $typeCode $valueNameBuilder;|]
    [i|this.$valueNameBuilder = $valueNameBuilder;|]
    [i|$typeCode $valueNameBuilder|]
    (typeHashCodeField valueNameBuilder)
    (typeToJsonField valueNameBuilder)
    (typeEqualsField valueNameText)

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
    typeToJsonField :: Builder -> ToJsonBuilder.Field
  }

intType :: Bool -> Type
intType = \case
  False -> Type "int" EqualsBuilder.primitiveField HashCodeBuilder.intField ToJsonBuilder.intField
  True -> Type "OptionalInt" EqualsBuilder.objectField HashCodeBuilder.objectField ToJsonBuilder.optionalIntField

longType :: Bool -> Type
longType = \case
  False -> Type "long" EqualsBuilder.primitiveField HashCodeBuilder.longField ToJsonBuilder.longField
  True -> Type "OptionalLong" EqualsBuilder.objectField HashCodeBuilder.objectField ToJsonBuilder.optionalLongField

floatType :: Bool -> Type
floatType = \case
  False -> Type "float" EqualsBuilder.primitiveField HashCodeBuilder.floatField ToJsonBuilder.floatField
  True -> Type "Optional<Float>" EqualsBuilder.objectField HashCodeBuilder.objectField (ToJsonBuilder.optionalField ToJsonBuilder.floatField)

doubleType :: Bool -> Type
doubleType = \case
  False -> Type "double" EqualsBuilder.primitiveField HashCodeBuilder.doubleField ToJsonBuilder.doubleField
  True -> Type "OptionalDouble" EqualsBuilder.objectField HashCodeBuilder.objectField ToJsonBuilder.optionalDoubleField

stringType :: Bool -> Type
stringType = \case
  False -> Type "String" EqualsBuilder.objectField HashCodeBuilder.objectField ToJsonBuilder.stringField
  True -> Type "Optional<String>" EqualsBuilder.objectField HashCodeBuilder.objectField (ToJsonBuilder.optionalField ToJsonBuilder.stringField)

dateType :: Bool -> Type
dateType = \case
  False -> Type "Date" EqualsBuilder.objectField HashCodeBuilder.objectField ToJsonBuilder.dateField
  True -> Type "Optional<Date>" EqualsBuilder.objectField HashCodeBuilder.objectField (ToJsonBuilder.optionalField ToJsonBuilder.dateField)
