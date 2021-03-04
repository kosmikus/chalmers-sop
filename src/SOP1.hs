{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module SOP1 where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

data NP :: (a -> Type) -> [a] -> Type where
  Nil  :: NP f '[]
  (:*) :: f x -> NP f xs -> NP f (x : xs)

infixr 5 :*

deriving instance All (Compose Show f) xs => Show (NP f xs)

type family All f xs :: Constraint where
  All f '[]      = ()
  All f (x : xs) = (f x, All f xs)

class (f (g x)) => Compose f g x
instance (f (g x)) => Compose f g x

newtype I a = I a
  deriving Show

newtype K a b = K a
  deriving Show

example1 :: NP I '[ Int, Char, Bool ]
example1 = I 3 :* I 'x' :* I True :* Nil

example2 :: NP (K String) '[ Int, Char, Bool ]
example2 = K "foo" :* K "bar" :* K "baz" :* Nil

map_NP :: (forall x . f x -> g x) -> NP f xs -> NP g xs
map_NP f Nil       = Nil
map_NP f (x :* xs) = f x :* map_NP f xs

cmap_NP :: All c xs => Proxy c -> (forall x . c x => f x -> g x) -> NP f xs -> NP g xs
cmap_NP _ f Nil       = Nil
cmap_NP p f (x :* xs) = f x :* cmap_NP p f xs

collapse_NP :: NP (K a) xs -> [a]
collapse_NP Nil = []
collapse_NP (K x :* xs) = x : collapse_NP xs

data NS :: (a -> Type) -> [a] -> Type where
  Z :: f x -> NS f (x : xs)
  S :: NS f xs -> NS f (x : xs)

deriving instance All (Compose Show f) xs => Show (NS f xs)

example3 :: NS I '[ Int, Char, Bool ]
example3 = S (Z (I 'a'))

map_NS :: (forall x . f x -> g x) -> NS f xs -> NS g xs
map_NS f (Z x) = Z (f x)
map_NS f (S s) = S (map_NS f s)

cmap_NS :: All c xs => Proxy c -> (forall x . c x => f x -> g x) -> NS f xs -> NS g xs
cmap_NS _ f (Z x) = Z (f x)
cmap_NS p f (S s) = S (cmap_NS p f s)

newtype SOP f xss = SOP (NS (NP f) xss)

example4 :: SOP I '[ '[ Int ], '[ Char, Bool ] ]
example4 = SOP (S (Z (I 'x' :* I True :* Nil)))

map_SOP :: (forall x . f x -> g x) -> SOP f xss -> SOP g xss
map_SOP f (SOP sop) = SOP (map_NS (map_NP f) sop)

-- cmap_SOP :: All (All c) xss => Proxy c -> (forall x . c x => f x -> g x) -> SOP f xss -> SOP g xss
-- cmap_SOP = undefined
