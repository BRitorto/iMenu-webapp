module Platform.JSONUtil where

import ClassyPrelude
import Data.Aeson.TH
import Language.Haskell.TH.Syntax

commonJSONDeriveMany :: [Name] -> Q [Dec]
commonJSONDeriveMany names =
  ClassyPrelude.concat <$> mapM commonJSONDerive names

commonJSONDerive :: Name -> Q [Dec]
commonJSONDerive name =
  let lowerCaseFirst (y:ys) = toLower [y] <> ys 
      lowerCaseFirst "" = ""
      structName = fromMaybe "" ClassyPrelude.. lastMay ClassyPrelude.. splitElem '.' ClassyPrelude.. show $ name
  in deriveJSON defaultOptions{fieldLabelModifier = lowerCaseFirst ClassyPrelude.. ClassyPrelude.drop (ClassyPrelude.length structName)} name