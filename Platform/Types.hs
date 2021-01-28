{-# LANGUAGE TemplateHaskell #-}

module Platform.Types where

import Language.Haskell.TH.Syntax
import ClassyPrelude
import Data.Aeson.TH (fieldLabelModifier, deriveJSON, defaultOptions)
  
commonJSONDeriveMany :: [Name] -> Q [Dec]
commonJSONDeriveMany names =
  ClassyPrelude.concat <$> mapM commonJSONDerive names

commonJSONDerive :: Name -> Q [Dec]
commonJSONDerive name =
  let lowerCaseFirst (y:ys) = toLower [y] <> ys 
      lowerCaseFirst "" = ""
      structName = fromMaybe "" ClassyPrelude.. lastMay ClassyPrelude.. splitElem '.' ClassyPrelude.. show $ name
  in deriveJSON defaultOptions{fieldLabelModifier = lowerCaseFirst ClassyPrelude.. ClassyPrelude.drop (ClassyPrelude.length structName)} name
  