{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
    r <- Wreq.customHistoriedMethodWith "GET" wreqOpts (URI.renderStr uri)
    let status = r ^. Wreq.hrFinalResponse . Wreq.responseStatus . Wreq.statusCode
    when (status < 200 || status >= 300) $ throwIO (JsonLdError "loading document failed")
    -- Look at the Location from the last of our redirects, or the
    -- initial URL if there were no redirects.
    documentUrl <- maybe (return uri) URI.mkURI
        $ r ^? Wreq.hrRedirects
             . _last
             . _2
             . Wreq.responseHeader "Location"
             . to TE.decodeUtf8
    rContentType <- respContentType r
    case rContentType of
        Nothing -> throwIO (JsonLdError "loading document failed")
        Just t | not (isJsonBased t) -> do
            let alternate = r ^? Wreq.hrFinalResponse . Wreq.responseLink "rel" "alternate" . filtered (has (Wreq.linkParams . folded . filtered (== ("type", "application/ld+json")))) . Wreq.linkURL
            case alternate of
                Nothing -> throwIO (JsonLdError "loading document failed")
                Just a -> do
                    relUri <- URI.mkURI . TE.decodeUtf8Lenient $ a
                    let nextUri = fromJust $ relUri `URI.relativeTo` uri
                    defaultLoadDocumentCallback opts nextUri
        Just t | isNonLdJson t -> do
            let contexts = r ^.. Wreq.hrFinalResponse . Wreq.responseLink "rel" "http://www.w3.org/ns/json-ld#context" . Wreq.linkURL
            contextUrl <- case contexts of
                [] -> return Nothing
                _ : _ : _ -> throwIO (JsonLdError "multiple context link headers")
                [c] -> fmap return . URI.mkURI . TE.decodeUtf8Lenient $ c
            document <- r ^. Wreq.hrFinalResponse . Wreq.responseBody . to Aeson.throwDecode
            let contentType = MIME.mimeType t
            let profile = lookup "profile" . map (\(MIME.MIMEParam name value) -> (name, value)) . MIME.mimeParams $ t
            return $ RemoteDocument
                { contentType
                , contextUrl
                , document
                , documentUrl
                , profile
                }
        Just t -> do
            document <- r ^. Wreq.hrFinalResponse . Wreq.responseBody . to Aeson.throwDecode
            let contentType = MIME.mimeType t
            let profile = lookup "profile" . map (\(MIME.MIMEParam name value) -> (name, value)) . MIME.mimeParams $ t
            return $ RemoteDocument
                { contentType
                , contextUrl=Nothing
                , document
                , documentUrl
                , profile
                }

wreqOptsOf :: Maybe LoadDocumentOptions -> IO Wreq.Options
wreqOptsOf opts = do
    let profileParameter = opts >>= Just
          . T.unwords
          . fmap URI.render
          . NE.toList
          . requestProfile
    let jsonLdMime = MIME.Type
          { MIME.mimeType = jsonLdType
          , MIME.mimeParams = q 1.0 : (maybeToList . fmap profileParam $ profileParameter)
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
    return $ Wreq.defaults & Wreq.header "Accept" .~ [TE.encodeUtf8 acceptHeader]

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
