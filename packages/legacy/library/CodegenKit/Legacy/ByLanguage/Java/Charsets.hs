module CodegenKit.Legacy.ByLanguage.Java.Charsets where

import CodegenKit.Legacy.Prelude
import StructureKit.Charset

namespaceSegmentHead :: Charset
namespaceSegmentHead =
  charRange 'a' 'z'

namespaceSegmentTail :: Charset
namespaceSegmentTail =
  mconcat
    [ charRange 'a' 'z',
      charRange 'A' 'Z',
      charRange '0' '9',
      "_"
    ]
