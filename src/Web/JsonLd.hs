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

type LoadDocumentCallback = Maybe LoadDocumentOptions -> URI -> IO (RemoteDocument Aeson.Value)

data LoadDocumentOptions = LoadDocumentOptions
    { loadExtractAllScripts :: Bool
    , loadProfile :: URI
    , requestProfile :: Either URI [URI]
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

compact :: Input -> IO Record
compact = compactWithContext defaultContext

compactWithContext :: Context -> Input -> IO Record
compactWithContext = compactWithOptions defaultOptions

compactWithOptions :: Options -> Context -> Input -> IO Record
compactWithOptions = undefined

expand :: Input -> IO Record
expand = expandWithContext defaultContext

expandWithContext :: Context -> Input -> IO Record
expandWithContext = expandWithOptions defaultOptions

expandWithOptions :: Options -> Context -> Input -> IO Record
expandWithOptions = undefined

flatten :: Input -> IO Record
flatten = flattenWithContext defaultContext

flattenWithContext :: Context -> Input -> IO Record
flattenWithContext = flattenWithOptions defaultOptions

flattenWithOptions :: Options -> Context -> Input -> IO Record
flattenWithOptions = undefined

fromRdf :: RDF a -> IO [Record]
fromRdf = fromRdfWithOptions defaultOptions

fromRdfWithOptions :: Options -> RDF a -> IO [Record]
fromRdfWithOptions = undefined

toRdf :: Input -> IO (RDF a)
toRdf = toRdfWithOptions defaultOptions

toRdfWithOptions :: Options -> Input -> IO (RDF a)
toRdfWithOptions = undefined
