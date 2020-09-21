module Parser.Halogen.Utils where

import Language.PS.SmartCST (Expr(..), ProperName(..), SmartQualifiedName(..), SmartQualifiedNameConstructor(..), mkModuleName)
import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (fold)
import Data.List (List)
import Data.String (Pattern(..), Replacement(..)) as String
import Text.HTML.Parser (Attribute(..), HTML(..))
import Data.String.Common (null, replaceAll, split) as String

fromHalogenHH :: forall a. a -> SmartQualifiedName a
fromHalogenHH = SmartQualifiedName__Custom (mkModuleName $ NonEmptyArray.cons' "Halogen" ["HTML"]) (mkModuleName $ NonEmptyArray.cons' "HH" [])

fromHalogenHP :: forall a. a -> SmartQualifiedName a
fromHalogenHP = SmartQualifiedName__Custom (mkModuleName $ NonEmptyArray.cons' "Halogen" ["HTML", "Properties"]) (mkModuleName $ NonEmptyArray.cons' "HP" [])

fromHalogenHPAria :: forall a. a -> SmartQualifiedName a
fromHalogenHPAria = SmartQualifiedName__Custom (mkModuleName $ NonEmptyArray.cons' "Halogen" ["HTML", "Properties", "ARIA"]) (mkModuleName $ NonEmptyArray.cons' "HP" ["ARIA"])

exprClassNameConstructor :: Expr
exprClassNameConstructor = ExprConstructor $
  SmartQualifiedNameConstructor__Simple
  (mkModuleName $ NonEmptyArray.cons' "Halogen" ["HTML"])
  (ProperName "ClassName")
  (ProperName "ClassName")

stringToClasses :: String -> Array String
stringToClasses = Array.filter (not String.null) <<< String.split (String.Pattern " ")

collectClassNames :: List HTML -> Array String
collectClassNames =
  let
    collectClassNamesHtml :: HTML -> Array String
    collectClassNamesHtml = case _ of
      (Element _name attrs children) -> (fold $ map collectClassNamesAttr attrs) <> (fold $ map collectClassNamesHtml children)
      (VoidElement _name attrs) -> fold $ map collectClassNamesAttr attrs
      _ -> []

    collectClassNamesAttr :: Attribute -> Array String
    collectClassNamesAttr = case _ of
      Attribute "class" val -> stringToClasses val
      _ -> []
  in Array.nub <<< fold <<< map collectClassNamesHtml

-- "--" is BAM style modifier
classNameToFunctionName :: String -> String
classNameToFunctionName = String.replaceAll (String.Pattern "--") (String.Replacement "____") >>> String.replaceAll (String.Pattern "-") (String.Replacement "_")
