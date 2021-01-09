{-# LANGUAGE TemplateHaskell #-}

module Curve.TH where

import Control.Monad.IO.Class (liftIO)
import Distribution.Types.PackageDescription (package)
import Distribution.Types.PackageId (pkgVersion)
import Distribution.Types.GenericPackageDescription (packageDescription)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Verbosity (normal)
import Language.Haskell.TH

cabalVersion :: Q Exp
cabalVersion = do
  desc <- liftIO $ readGenericPackageDescription normal "./curve.cabal"
  let ver = prettyShow . pkgVersion . package . packageDescription $ desc
  [| ver |]
