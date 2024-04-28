module Web.JsonLd
    ( Input
    , Record
    , Context
    , defaultContext
    , Options
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

type Input = ()
type Record = ()
type Context = ()
type Options = ()

defaultContext :: Context
defaultContext = ()

defaultOptions :: Options
defaultOptions = ()

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
