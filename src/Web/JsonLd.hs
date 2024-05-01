module Web.JsonLd
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
    , compact
    , compactWithContext
    , compactWithOptions
    , expand
    , expandWithContext
    , expandWithOptions
    , flatten
    , flattenWithContext
    , flattenWithOptions
    , fromRdf
    , fromRdfWithOptions
    , toRdf
    , toRdfWithOptions
    ) where

import Codec.MIME.Type qualified as MIME
import Data.Aeson qualified as Aeson
import Data.RDF (RDF)
import Data.RDF.IRI (IRIRef)
import Data.Text (Text)
import Data.Vector (Vector)

data Input
    = InputRecord !Record
    | InputArray !(Vector Aeson.Object)
    | InputIri !IRIRef
    | InputDoc !(RemoteDocument Aeson.Value)

type Record = Aeson.Object

data ContextElem
    = ContextRecord !Record
    | ContextIri !IRIRef

data Context
    = ContextElem !ContextElem
    | ContextArray !(Vector ContextElem)

data Options = Options
    { base :: Maybe IRIRef
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

type LoadDocumentCallback = Maybe LoadDocumentOptions -> IRIRef -> RemoteDocument Aeson.Value

data LoadDocumentOptions = LoadDocumentOptions
    { loadExtractAllScripts :: Bool
    , loadProfile :: IRIRef
    , requestProfile :: Either IRIRef [IRIRef]
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
    , contextUrl :: IRIRef
    , document :: a
    , documentUrl :: IRIRef
    , profile :: Text
    }

defaultContext :: Context
defaultContext = undefined

compact :: Input -> Record
compact = compactWithContext defaultContext

compactWithContext :: Context -> Input -> Record
compactWithContext = compactWithOptions defaultOptions

compactWithOptions :: Options -> Context -> Input -> Record
compactWithOptions = undefined

expand :: Input -> Record
expand = expandWithContext defaultContext

expandWithContext :: Context -> Input -> Record
expandWithContext = expandWithOptions defaultOptions

expandWithOptions :: Options -> Context -> Input -> Record
expandWithOptions = undefined

flatten :: Input -> Record
flatten = flattenWithContext defaultContext

flattenWithContext :: Context -> Input -> Record
flattenWithContext = flattenWithOptions defaultOptions

flattenWithOptions :: Options -> Context -> Input -> Record
flattenWithOptions = undefined

fromRdf :: RDF a -> [Record]
fromRdf = fromRdfWithOptions defaultOptions

fromRdfWithOptions :: Options -> RDF a -> [Record]
fromRdfWithOptions = undefined

toRdf :: Input -> RDF a
toRdf = toRdfWithOptions defaultOptions

toRdfWithOptions :: Options -> Input -> RDF a
toRdfWithOptions = undefined