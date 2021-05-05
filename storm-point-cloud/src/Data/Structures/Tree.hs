{-# LANGUAGE DeriveFunctor #-}
module Data.Structures.Tree where

import Data.Structures.Image

data PCTree a = Node {silhoutte :: ImageSparse
                   , left :: PCTree a
                   , right :: PCTree a}
             | Nil
             deriving (Eq, Show, Functor)