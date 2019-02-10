module Pos where

data Pos = Pos String Int Int
  deriving Eq

instance Show Pos where
  show (Pos f l c) = f ++ ":" ++ show l ++ ":" ++ show c

data Tag x = Tag Pos x
  deriving Eq

tag f l c = Tag (Pos f l c)
unwrapTag (Tag _ x) = x

instance Show x => Show (Tag x) where
  show (Tag p x) = show p ++ ":" ++ show x

instance Functor Tag where
  fmap f (Tag p x) = Tag p (f x)
