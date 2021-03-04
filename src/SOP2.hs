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
import qualified GHC.Generics as GHC
import Generics.SOP

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

{-
instance Generic Person where

  type Code Person = '[ '[ String, String, Int ] ]

  from :: Person -> SOP I (Code Person)
  from (MkPerson name lang nr) = SOP (Z (I name :* I lang :* I nr :* Nil))

  to :: SOP I (Code Person) -> Person
  to (SOP (Z (I name :* I lang :* I nr :* Nil))) = MkPerson name lang nr
-}

{-
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
-}

