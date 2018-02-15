{-# LANGUAGE GADTs, TypeFamilies #-}
module Web.Sake.Utils where
import Text.StringLike

readM :: (StringLike str, Read a) => str -> Maybe a
readM str =
  case reads $ castString  str of
    [(a, "")] -> Just a
    _         -> Nothing
