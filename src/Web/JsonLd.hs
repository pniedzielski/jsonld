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

import Data.RDF (RDF)

import Web.JsonLd.Types

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
