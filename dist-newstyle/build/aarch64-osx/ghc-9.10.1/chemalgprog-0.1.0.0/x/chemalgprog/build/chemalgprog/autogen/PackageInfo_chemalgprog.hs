{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_chemalgprog (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "chemalgprog"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Chemical Algorithmic Programming"
copyright :: String
copyright = "2025 Oliver Goldstein"
homepage :: String
homepage = ""
