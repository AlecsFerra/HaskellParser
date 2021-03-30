
module Types
    ( Type (..),
      TypeVar,
      TypeName
    )
where

import Ourlude

type TypeVar = String
type TypeName = String 

infixr 2 :->

data Type
    = StringT
    | IntT
    | BoolT
    | CustomType TypeName [Type] -- Type constructor
    | TVar TypeVar
    | Type :-> Type
    deriving (Eq, Show)

