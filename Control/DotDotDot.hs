-- | Haskell operator @g ... f = \x1 .. xn -> g (f x1 .. xn)@. Compose
-- functions such that all arguments are applied. Obviates @(.).(.)@ and
-- similar patterns in some cases.
--
-- Examples:
--
-- @
-- > ((+) ... (+) ... (+)) (1 :: Int) 2 3 4
-- 10
-- @
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.DotDotDot
( DotDotDot((...))
, (…)
, IsFun
, Return
, Replace
)
where

import Data.Bool
  ( Bool(True, False)
  )
--

import Prelude ()

-- | @IsFun f@ is @'True@ if @f@ has form @_ -> _@ and @'False@ otherwise.
--
type family IsFun (a :: *) :: Bool where
  IsFun (_ -> _) = 'True
  IsFun _        = 'False
--

-- | Class for defining '...' recursively on function types.
--
class b ~ IsFun f => DotDotDot f (b :: Bool) where
  type Return (f :: *) (b :: Bool) :: *
  type Replace (f :: *) (r :: *) (b :: Bool) :: *
  (...) :: (Return f b -> r) -> f -> Replace f r b
  infixr 9 ...
--

instance DotDotDot b (IsFun b) => DotDotDot (a -> b) 'True where
  type Return (a -> b) 'True = Return b (IsFun b)
  type Replace (a -> b) r 'True = a -> Replace b r (IsFun b)
  (...) g f x = g ... f x
--

instance 'False ~ IsFun a => DotDotDot a 'False where
  type Return a 'False = a
  type Replace a r 'False = r
  (...) f x = f x
--

-- | Alias for '...'.
--
(…) ::
  (b ~ IsFun f, DotDotDot f b) =>
  (Return f b -> r) -> f -> Replace f r b
(…) = (...)
infixr 9 …


