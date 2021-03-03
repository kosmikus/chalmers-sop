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
module SOP where

import Data.Kind
import GHC.TypeLits

data NP :: (a -> Type) -> [a] -> Type where
  Nil  :: NP f '[]
  (:*) :: f x -> NP f xs -> NP f (x : xs)

infixr 5 :*

instance Show (NP f '[]) where show Nil = "Nil"
deriving instance (Show (f x), Show (NP f xs)) => Show (NP f (x : xs))

data NS :: (a -> Type) -> [a] -> Type where
  Z :: f x -> NS f (x : xs)
  S :: NS f xs -> NS f (x : xs)

instance Show (NS f '[]) where show _ = error "impossible"
deriving instance (Show (f x), Show (NS f xs)) => Show (NS f (x : xs))

newtype I a = I a
  deriving Show

unI :: I a -> a
unI (I x) = x

newtype K a b = K a
  deriving Show

map_NP :: (forall x . f x -> g x) -> NP f xs -> NP g xs
map_NP _ Nil       = Nil
map_NP f (x :* xs) = f x :* map_NP f xs

type family AllF (c :: a -> Constraint) (xs :: [a]) :: Constraint where
  AllF c '[]      = ()
  AllF c (x : xs) = (c x, AllF c xs)

class AllF c xs => All (c :: a -> Constraint) (xs :: [a])
instance AllF c xs => All (c :: a -> Constraint) (xs :: [a])

cmap_NP :: forall c f g xs . All c xs => (forall x . c x => f x -> g x) -> NP f xs -> NP g xs
cmap_NP _ Nil       = Nil
cmap_NP f (x :* xs) = f x :* cmap_NP @c f xs

map_NS :: (forall x . f x -> g x) -> NS f xs -> NS g xs
map_NS f (Z x)     = Z (f x)
map_NS f (S s)     = S (map_NS f s)

cmap_NS :: forall c f g xs . All c xs => (forall x . c x => f x -> g x) -> NS f xs -> NS g xs
cmap_NS f (Z x)     = Z (f x)
cmap_NS f (S s)     = S (cmap_NS @c f s)

example_NP :: NP I '[Bool, Char, Int]
example_NP = I True :* I 'x' :* I 42 :* Nil

example_NS :: NS I '[Bool, Char, Int]
example_NS = S (Z (I 'x'))

newtype SOP f xss = SOP (NS (NP f) xss)

example_SOP :: SOP I '[ '[ Int, Bool ], '[ Char ] ]
example_SOP = SOP (Z (I 3 :* I False :* Nil))

cmap_SOP :: forall c f g xss . All (All c) xss => (forall x . c x => f x -> g x) -> SOP f xss -> SOP g xss
cmap_SOP f (SOP sop) = SOP (cmap_NS @(All c) (cmap_NP @c f) sop)

collapse_NP :: NP (K a) xs -> [a]
collapse_NP Nil = []
collapse_NP (K x :* xs) = x : collapse_NP xs

collapse_NS :: NS (K a) xs -> a
collapse_NS (Z (K x)) = x
collapse_NS (S s)     = collapse_NS s

class Top a
instance Top a

collapse_SOP :: SOP (K a) xss -> [a]
collapse_SOP (SOP sop) = collapse_NS (map_NS (K . collapse_NP) sop)

-- cpure_NP :: All c xs => (forall x . c x => f x) => NP f xs
