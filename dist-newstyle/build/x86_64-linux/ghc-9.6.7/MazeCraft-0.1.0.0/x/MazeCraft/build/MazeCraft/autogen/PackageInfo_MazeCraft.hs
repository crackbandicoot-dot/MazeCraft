{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_MazeCraft (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "MazeCraft"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A maze generation and solving application"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
