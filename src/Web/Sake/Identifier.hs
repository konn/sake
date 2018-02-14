{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Sake.Identifier (Identifier(..)) where
import Data.Hashable (Hashable)
import Data.String   (IsString)

newtype Identifier = Identifier { runIdentifier :: String }
                   deriving (Read, Show, Eq, Ord, IsString, Hashable)
