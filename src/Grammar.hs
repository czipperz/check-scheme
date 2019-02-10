module Grammar where

import Pos

type Program = [Tag Expression]

data Expression =
  Constant Constant |
  Variable String |
  List [Tag Expression]
  deriving Eq

data Constant =
  Boolean Bool |
  Integer Int |
  Character Char |
  String String
  deriving Eq

instance Show Expression where
  show (Constant c) = show c
  show (Variable v) = v
  show (List l) = '(' : showList l
    where showList [] = ")"
          showList [x] = show x ++ showList []
          showList (x:xs) = show x ++ " " ++ showList xs

instance Show Constant where
  show (Boolean b) = show b
  show (Integer i) = show i
  show (Character c) = show c
  show (String s) = show s
