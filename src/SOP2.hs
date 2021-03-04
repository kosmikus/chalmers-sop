{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module SOP2 where

import Control.Monad
import Data.ByteString
import Data.Csv
import Data.Foldable
import Data.Kind
import Data.SOP
import Data.SOP.NP
import Data.SOP.NS
import Generics.SOP
import Generics.SOP.Metadata
import qualified GHC.Generics as GHC

{-
class Generic a where

  type Code a :: [[Type]]

  from :: a -> SOP I (Code a)
  to :: SOP I (Code a) -> a
-}

data Person =
  MkPerson
    { name              :: String
    , favouriteLanguage :: String
    , favouriteNumber   :: Int
    }
  deriving (Show, GHC.Generic, Generic)

data Language =
    Haskell
  | OCaml
  | Agda
  deriving (Show, GHC.Generic, Generic, HasDatatypeInfo)

andres :: Person
andres =
  MkPerson "Andres" "Haskell" 42

{-
instance Generic Person where

  type Code Person = '[ '[ String, String, Int ] ]

  from (MkPerson name lang nr) = SOP (Z (I name :* I lang :* I nr :* Nil))
  to (SOP (Z (I name :* I lang :* I nr :* Nil))) = MkPerson name lang nr
-}

gToRecord :: (Generic a, Code a ~ '[ xs ], All ToField xs) => a -> Record
gToRecord =
  record . collapse_NP . cmap_NP (Proxy @ToField) (K . toField . unI) . unZ . unSOP . from

instance ToRecord Person where
  toRecord = gToRecord

indices :: All Top xs => NP (K Int) xs
indices =
  ana_NP (\ (K i) -> (K i, K (i + 1))) (K 0)

gParseRecord :: (Generic a, Code a ~ '[ xs ], All FromField xs) => Record -> Parser a
gParseRecord v =
  fmap (to . SOP . Z) (sequence_NP (cmap_NP (Proxy @FromField) ((v .!) . unK) indices))

instance FromRecord Person where
  parseRecord = gParseRecord

elements :: (Generic a, All ((~) '[]) (Code a)) => NP (K a) (Code a)
elements =
  map_NP (K . to . SOP . unK) (apInjs'_NP (cpure_NP (Proxy @((~) '[])) Nil))

constructorNames :: (Generic a, HasDatatypeInfo a) => Proxy a -> NP (K Field) (Code a)
constructorNames p =
  map_NP (K . toField . constructorName) (constructorInfo (datatypeInfo p))

gParseField ::
  forall a . (Generic a, All ((~) '[]) (Code a)) =>
  NP (K Field) (Code a) -> Field -> Parser a
gParseField names s =
  let
    parsers :: NP (K (Parser a)) (Code a)
    parsers =
      zipWith_NP
        (\ (K name) (K elt) -> K (guard (s == name) >> pure elt))
        names elements
  in
    asum (collapse_NP parsers)

gToField ::
  forall a . (Generic a, All ((~) '[]) (Code a)) =>
  NP (K Field) (Code a) -> a -> Field
gToField names x =
  collapse_NS (hzipWith const names (unSOP (from x)))
