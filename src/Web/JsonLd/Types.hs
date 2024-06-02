module Web.JsonLd.Types
    ( Input(..)
    , Record
    , Context(..)
    , ContextElem(..)
    , defaultContext
    , Options(..)
    , ProcessingMode(..)
    , RdfDirection(..)
    , LoadDocumentCallback
    , LoadDocumentOptions(..)
    , RemoteDocument(..)
    , defaultOptions
    ) where

import Codec.MIME.Type qualified as MIME
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Vector (Vector)
import Text.URI (URI(..))

data Input
    = InputRecord !Record
    | InputArray !(Vector Aeson.Object)
    | InputIri !URI
    | InputDoc !(RemoteDocument Aeson.Value)

type Record = Aeson.Object

data ContextElem
    = ContextRecord !Record
    | ContextIri !URI

data Context
    = ContextElem !ContextElem
    | ContextArray !(Vector ContextElem)

data Options = Options
    { base :: Maybe URI
    , compactArrays :: Bool
    , compactToRelative :: Bool
    , documentLoader :: Maybe LoadDocumentCallback
    , expandContext :: Maybe ContextElem
    , extractAllScripts :: Bool
    , frameExpansion :: Bool
    , ordered :: Bool
    , processingMode :: ProcessingMode
    , produceGeneralizedRdf :: Bool
    , rdfDirection :: Maybe RdfDirection
    , useNativeTypes :: Bool
    , useRdfType :: Bool
    }

data ProcessingMode = JsonLd10 | JsonLd11

data RdfDirection = I18nDatatype | CompoundLiteral

type LoadDocumentCallback =
    Maybe LoadDocumentOptions -> URI -> IO (RemoteDocument Aeson.Value)

data LoadDocumentOptions = LoadDocumentOptions
    { loadExtractAllScripts :: Bool
    , loadProfile :: URI
    , requestProfile :: NE.NonEmpty URI
    }

defaultOptions :: Options
defaultOptions = Options
    { base = Nothing
    , compactArrays = True
    , compactToRelative = True
    , documentLoader = Nothing
    , expandContext = Nothing
    , extractAllScripts = False
    , frameExpansion = False
    , ordered = False
    , processingMode = JsonLd11
    , produceGeneralizedRdf = True
    , rdfDirection = Nothing
    , useNativeTypes = False
    , useRdfType = False
    }

data RemoteDocument a = RemoteDocument
    { contentType :: MIME.Type
    , contextUrl :: URI
    , document :: a
    , documentUrl :: URI
    , profile :: Text
    }

defaultContext :: Context
defaultContext = undefined
