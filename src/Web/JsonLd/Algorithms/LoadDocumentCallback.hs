{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Web.JsonLd.Algorithms.LoadDocumentCallback
    ( LoadDocumentCallback
    , LoadDocumentOptions(..)
    , defaultLoadDocumentCallback
    ) where

import Codec.MIME.Type qualified as MIME
import Codec.MIME.Parse qualified as MIMEParse
import Control.Exception
import Control.Lens
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NE
import Data.Maybe (maybeToList, fromJust)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Wreq qualified as Wreq
import Text.URI qualified as URI
import Web.JsonLd.Types (LoadDocumentCallback, LoadDocumentOptions(..), RemoteDocument(..))

newtype JsonLdError = JsonLdError T.Text
    deriving Show

instance Exception JsonLdError

defaultLoadDocumentCallback :: LoadDocumentCallback
defaultLoadDocumentCallback opts uri = do
    wreqOpts <- wreqOptsOf opts
    -- Make the request, chasing redirects, and fail if we don't get a
    -- 2xx status code.
    r <- Wreq.customHistoriedMethodWith "GET" wreqOpts (URI.renderStr uri)
    let status = r
          ^. Wreq.hrFinalResponse
           . Wreq.responseStatus
           . Wreq.statusCode
    when (status < 200 || status >= 300) $
        throwIO (JsonLdError "loading document failed")
    -- Look at the Location from the last of our redirects, or the
    -- initial URL if there were no redirects.
    documentUrl <- maybe (return uri) URI.mkURI $ r
        ^? Wreq.hrRedirects
         . _last
         . _2
         . Wreq.responseHeader "Location"
         . to TE.decodeUtf8
    -- Decode JSON
    documentJson <- r
        ^. Wreq.hrFinalResponse
         . Wreq.responseBody
         . to Aeson.throwDecode
    -- Find the content-type of the response
    rContentType <- respContentType r
    case rContentType of
        -- Malformed HTTP response
        Nothing -> throwIO (JsonLdError "loading document failed")
        -- Not application/json or application/*+json.  Check for
        -- rel=alternate JSON-LD link.
        Just contentMimeType | not . isJsonBased $ contentMimeType -> do
            let alternate = r
                  ^? Wreq.hrFinalResponse
                  . Wreq.responseLink "rel" "alternate"
                  . (filtered . has
                      $ Wreq.linkParams
                      . folded
                      . filtered (== ("type", "application/ld+json")))
                  . Wreq.linkURL
            case alternate of
                Nothing -> throwIO (JsonLdError "loading document failed")
                Just a -> do
                    relUri <- URI.mkURI . TE.decodeUtf8Lenient $ a
                    let nextUri = fromJust $ relUri `URI.relativeTo` uri
                    defaultLoadDocumentCallback opts nextUri
        -- application/json or application/*+json, but not
        -- application/ld+json.  Look for a single context link to
        -- process the JSON as JSON-LD.
        Just contentMimeType | isNonLdJson contentMimeType -> do
            let contextUrls = r
                  ^.. Wreq.hrFinalResponse
                    . Wreq.responseLink "rel" contextRel
                    . Wreq.linkURL
            contextUrl <- case contextUrls of
                [] -> return Nothing
                _ : _ : _ ->
                    throwIO (JsonLdError "multiple context link headers")
                [c] -> fmap return . URI.mkURI . TE.decodeUtf8Lenient $ c
            return $ buildRemoteDocument
                documentJson
                documentUrl
                contentMimeType
                contextUrl
        -- application/ld+json
        Just contentMimeType -> do
            return $ buildRemoteDocument
                documentJson
                documentUrl
                contentMimeType
                Nothing

wreqOptsOf :: Maybe LoadDocumentOptions -> IO Wreq.Options
wreqOptsOf opts = do
    let profileParameter = opts >>= Just
          . T.unwords
          . fmap URI.render
          . NE.toList
          . requestProfile
    let jsonLdMime = MIME.Type
          { MIME.mimeType = jsonLdType
          , MIME.mimeParams = q 1.0 : (maybeToList
                                           . fmap profileParam
                                           $ profileParameter)
          }
    let jsonMime = MIME.Type
          { MIME.mimeType = jsonType
          , MIME.mimeParams = [q 0.5]
          }
    let acceptHeader = T.intercalate ","
          [ MIME.showType jsonLdMime
          , MIME.showType jsonMime
          , "*/*;q=0.1"
          ]
    return $ Wreq.defaults
           & Wreq.header "Accept"
           .~ [TE.encodeUtf8 acceptHeader]

buildRemoteDocument
    :: a
    -> URI.URI
    -> MIME.Type
    -> Maybe URI.URI
    -> RemoteDocument a
buildRemoteDocument
    document
    documentUrl
    contentMimeType
    contextUrl = RemoteDocument{ .. }
  where
    contentType = MIME.mimeType contentMimeType
    profile = lookup "profile"
        . map (\(MIME.MIMEParam name value) -> (name, value))
        . MIME.mimeParams
        $ contentMimeType

respContentType :: Wreq.HistoriedResponse a -> IO (Maybe MIME.Type)
respContentType r = return
    $ r ^?  Wreq.hrFinalResponse
        .   Wreq.responseHeader "Content-Type"
        .   to TE.decodeUtf8
        >>= MIMEParse.parseMIMEType

q :: Float -> MIME.MIMEParam
q weight = MIME.MIMEParam
    { MIME.paramName = "q"
    , MIME.paramValue = T.pack . show $ weight
    }

profileParam :: T.Text -> MIME.MIMEParam
profileParam p = MIME.MIMEParam
    { MIME.paramName = "profile"
    , MIME.paramValue = p
    }

jsonType :: MIME.MIMEType
jsonType = MIME.Application "json"

jsonLdType :: MIME.MIMEType
jsonLdType = MIME.Application "ld+json"

isJsonBased :: MIME.Type -> Bool
isJsonBased t = case MIME.mimeType t of
    MIME.Multipart{} -> False
    _ -> "+json" `T.isSuffixOf` MIME.subTypeString t

isJson :: MIME.Type -> Bool
isJson t = MIME.mimeType t == jsonType || isJsonBased t

isNonLdJson :: MIME.Type -> Bool
isNonLdJson t = isJson t && MIME.mimeType t /= jsonLdType

contextRel :: IsString a => a
contextRel = "http://www.w3.org/ns/json-ld#context"
