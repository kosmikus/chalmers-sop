{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SOP where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Data.SOP
import Data.SOP.NP
import Data.SOP.NS

{-
data NP :: (a -> Type) -> [a] -> Type where
  Nil  :: NP f '[]
  (:*) :: f x -> NP f xs -> NP f (x : xs)

infixr 5 :*

deriving instance All (Compose Show f) xs => Show (NP f xs)

type family AllF (f :: a -> Constraint) (xs :: [a]) :: Constraint where
  AllF f '[]      = ()
  AllF f (x : xs) = (f x, AllF f xs)

class AllF f xs => All (f :: a -> Constraint) (xs :: [a])
instance AllF f xs => All f xs

class (f (g x)) => Compose (f :: b -> Constraint) (g :: a -> b) (x :: a)
instance (f (g x)) => Compose f g x

newtype I a = I a
  deriving Show

unI :: I a -> a
unI (I x) = x

newtype K a b = K a
  deriving Show
-}

example1 :: NP I '[ Int, Char, Bool ]
example1 = I 17 :* I 'x' :* I True :* Nil

example2 :: NP (K String) '[ Int, Char, Bool ]
example2 = K "foo" :* K "bar" :* K "baz" :* Nil

{-
collapse_NP :: NP (K a) xs -> [a]
collapse_NP Nil = []
collapse_NP (K x :* xs) = x : collapse_NP xs

map_NP :: (forall x . f x -> g x) -> NP f xs -> NP g xs
map_NP _ Nil = Nil
map_NP f (x :* xs) = f x :* map_NP f xs

cmap_NP :: All c xs => Proxy c -> (forall x . c x => f x -> g x) -> NP f xs -> NP g xs
cmap_NP _ _ Nil = Nil
cmap_NP p f (x :* xs) = f x :* cmap_NP p f xs

-- cpure_NP :: All c xs => Proxy c -> (forall x . c x => f x) -> NP f xs
-- czipWith_NP :: All c xs => Proxy c -> (forall x . c x => f x -> g x -> h x)
--                               -> NP f xs -> NP g xs -> NP h xs

data NS :: (a -> Type) -> [a] -> Type where
  Z :: f x -> NS f (x : xs)
  S :: NS f xs -> NS f (x : xs)

deriving instance All (Compose Show f) xs => Show (NS f xs)
-}

example3 :: NS I '[ Int, Char, Bool ]
example3 = S (Z (I 'x'))

example4 :: NS (K String) '[ Int, Char, Bool ]
example4 = S (Z (K "bar"))

{-
collapse_NS :: NS (K a) xs -> a
collapse_NS (Z (K x)) = x
collapse_NS (S s)     = collapse_NS s

map_NS :: (forall x . f x -> g x) -> NS f xs -> NS g xs
map_NS f (Z x) = Z (f x)
map_NS f (S s) = S (map_NS f s)

cmap_NS :: All c xs => Proxy c -> (forall x . c x => f x -> g x) -> NS f xs -> NS g xs
cmap_NS _ f (Z x) = Z (f x)
cmap_NS p f (S s) = S (cmap_NS p f s)

newtype SOP f xss = SOP (NS (NP f) xss)
  deriving Show

collapse_SOP :: SOP (K a) xs -> [a]
collapse_SOP (SOP sop) = collapse_NS (map_NS (K . collapse_NP) sop)

map_SOP :: (forall x . f x -> g x) -> SOP f xss -> SOP g xss
map_SOP f (SOP sop) = SOP (map_NS (map_NP f) sop)

cmap_SOP :: forall c xss f g . All (All c) xss => Proxy c -> (forall x . c x => f x -> g x) -> SOP f xss -> SOP g xss
cmap_SOP p f (SOP sop) = SOP (cmap_NS (Proxy @(All c)) (cmap_NP (Proxy @c) f) sop)
-}

example5 :: SOP I '[ '[ Int ], '[ Char, Bool ] ]
example5 = SOP (S (Z (I 'x' :* I True :* Nil)))
