{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.PDF.AST
  ()
where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Word                      ( Word8 )
import           Data.Map.Strict                ( Map )
import           Data.ByteString                ( ByteString )
import           Data.PDF.EntrieObject

type NameID = Int

data PdfCtx = PdfCtx { ctxVersion :: Int
                     , ctxNameCtx :: Map NameID [Word8]
                     }

newtype AST m a = AST (StateT PdfCtx (ReaderT ByteString m) a)
 deriving ( Functor
          , Applicative
          , Monad
          , MonadReader ByteString
          , MonadState PdfCtx
          )
