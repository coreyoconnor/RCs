module GHCITermPrelude ( module Prelude
                       , module Control.Monad
                       , broadcast_env
                       )
where

import Prelude hiding ( (=<<) 
                      , mapM
                      , mapM_
                      , sequence_
                      , sequence
                      , fmap
                      , (>>)
                      , (>>=)
                      , fail
                      , Monad(..)
                      , Functor(..)
                      )
import Control.Monad

import Codec.Binary.UTF8.String ( encode )

broadcast_env :: String -> IO ()
broadcast_env _ = return ()

