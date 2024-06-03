{-# LANGUAGE ExistentialQuantification #-}

module Web.JsonLd.Error
    ( Error(Error)
    ) where

import Control.Exception

data Error = forall e . Exception e => Error e

instance Show Error where
    show (Error e) = show e

instance Exception Error

