{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module SOP2 where

import Control.Monad
import Data.Csv
import Data.Foldable
import Data.Kind
import Data.SOP
import Data.SOP.NP
import Data.SOP.NS
-- import qualified GHC.Generics as GHC
-- import Generics.SOP

class Generic a where

  type Code a :: [[Type]]

  from :: a -> SOP I (Code a)
  to :: SOP I (Code a) -> a

data Person =
  MkPerson
    { name              :: String
    , favouriteLanguage :: String
    , favouriteNumber   :: Int
    }
  deriving (Show)

data Language =
    Haskell
  | OCaml
  | Agda
  deriving (Show)

andres :: Person
andres =
  MkPerson "Andres" "Haskell" 8

instance Generic Person where

  type Code Person = '[ '[ String, String, Int ] ]

  from :: Person -> SOP I (Code Person)
  from (MkPerson name lang nr) = SOP (Z (I name :* I lang :* I nr :* Nil))

  to :: SOP I (Code Person) -> Person
  to (SOP (Z (I name :* I lang :* I nr :* Nil))) = MkPerson name lang nr

instance Generic Language where

  type Code Language = '[ '[], '[], '[] ]

  from :: Language -> SOP I (Code Language)
  from Haskell = SOP (Z Nil)
  from OCaml   = SOP (S (Z Nil))
  from Agda    = SOP (S (S (Z Nil)))

  to :: SOP I (Code Language) -> Language
  to (SOP (Z Nil))         = Haskell
  to (SOP (S (Z Nil)))     = OCaml
  to (SOP (S (S (Z Nil)))) = Agda

gToRecord :: (Generic a, Code a ~ '[ xs ], All ToField xs) => a -> Record
gToRecord =
  record . collapse_NP . cmap_NP (Proxy @ToField) (\ (I x) -> K (toField x)) . unZ . unSOP . from

instance ToRecord Person where
  toRecord = gToRecord

indices :: All Top xs => NP (K Int) xs
indices =
  ana_NP (\ (K i) -> (K i, K (i + 1))) (K 0)

gParseRecord :: (Generic a, Code a ~ '[ xs ], All FromField xs) => Record -> Parser a
gParseRecord r =
  fmap (to . SOP . Z) (sequence_NP (cmap_NP (Proxy @FromField) (\ (K i) -> r .! i) indices))

instance FromRecord Person where
  parseRecord = gParseRecord

data Tree = Leaf | Node Tree Tree

data Perfect a = Zero a | Succ (Perfect (a, a))

