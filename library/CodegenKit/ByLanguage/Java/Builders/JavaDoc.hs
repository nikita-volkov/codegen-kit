module CodegenKit.ByLanguage.Java.Builders.JavaDoc
  ( -- * Types
    Param,
    Return,
    Content,
    Throws,

    -- * JavaDoc Renderings
    methodJavaDoc,
    fieldJavaDoc,

    -- * Content
    plainTextContent,
    boldTextContent,
    italicTextContent,
    linkContent,
    inlineCodeContent,
    preformattedCodeContent,

    -- * Params
    param,
  )
where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.MultilineTextBuilder
import CodegenKit.Prelude
import qualified Data.HashSet as HashSet

-- * Types

data Param

data Return

data Content

data Throws

-- * Builders

methodJavaDoc :: [Content] -> [Param] -> Maybe Return -> [Throws] -> Builder
methodJavaDoc =
  error "TODO"

fieldJavaDoc :: [Content] -> Builder
fieldJavaDoc contents =
  methodJavaDoc contents [] Nothing []

-- * Content

plainTextContent :: Text -> Content
plainTextContent =
  error "TODO"

boldTextContent :: Text -> Content
boldTextContent =
  error "TODO"

italicTextContent :: Text -> Content
italicTextContent =
  error "TODO"

linkContent :: Text -> Content
linkContent =
  error "TODO"

inlineCodeContent :: Text -> Content
inlineCodeContent =
  error "TODO"

preformattedCodeContent :: Text -> Content
preformattedCodeContent =
  error "TODO"

-- * Params

param ::
  -- | Name.
  Text ->
  -- | Description.
  Text ->
  Param
param =
  error "TODO"
