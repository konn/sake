{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Web.Sake.Identifier (Identifier(..)) where
import Data.Hashable (Hashable)
import Data.String   (IsString)
import GHC.Generics  (Generic)

newtype Identifier = Identifier { runIdentifier :: String }
                   deriving stock (Read, Show, Eq, Ord, Generic)
                   deriving newtype (Hashable, IsString)
