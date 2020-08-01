module Parser.Halogen where

import Language.PS.SmartCST
import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.Foldable (fold)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Debug.Trace (spy, trace)
import Text.HTML.Parser (Attribute(..), HTML(..), parseHTML)
import Text.Parsing.StringParser (ParseError)
import Unsafe.Coerce (unsafeCoerce)

toHalogen :: String -> Either ParseError String
toHalogen content = parseHTML content <#> \htmls ->
  printModuleToString (outputModule htmls)

outputModule :: List HTML -> Module
outputModule htmls =
  Module
  { moduleName: mkModuleName $ NonEmptyArray.singleton "YourModule"
  , exports: []
  , declarations:
    (join $ map definedClassName $ spy "collectClassNames" (collectClassNames (spy "htmls" htmls)))
  }

definedClassName :: String -> Array Declaration
definedClassName className =
  [ DeclSignature
    { comments: Nothing
    , ident: Ident className
    , type_: TypeConstructor
      ( SmartQualifiedName__Simple
        (mkModuleName $ NonEmptyArray.cons' "Halogen" ["HTML"])
        (ProperName "ClassName")
      )
    }
  , DeclValue
    { comments: Nothing
    , valueBindingFields:
      { name: Ident className
      , binders: []
      , guarded: Unconditional
        { whereBindings: []
        , expr:
          ( ExprConstructor $
            SmartQualifiedNameConstructor__Simple
            (mkModuleName $ NonEmptyArray.cons' "Halogen" ["HTML"])
            (ProperName "ClassName")
            (ProperName "ClassName")
          )
          `ExprApp`
          ( ExprString className )
        }
      }
    }
  ]

collectClassNames :: List HTML -> Array String
collectClassNames =
  let
    collectClassNamesHtml :: HTML -> Array String
    collectClassNamesHtml = case _ of
      (Element _name attrs children) -> trace _name (const $ foldListToArray collectClassNamesAttr attrs <> spy "foldListToArray" (foldListToArray collectClassNamesHtml children))
      _ -> []

    collectClassNamesAttr :: Attribute -> Array String
    collectClassNamesAttr = case _ of
      Attribute "class" val -> Array.filter (not String.null) $ String.split (String.Pattern " ") (spy "val" val)
      _ -> []
  in Array.nub <<< foldListToArray collectClassNamesHtml

foldListToArray :: forall a . (a -> Array String) -> List a -> Array String
foldListToArray f = map f >>> fold

renderTree :: HTML -> String
renderTree d = unsafeCoerce unit
  -- | (VoidElement name attrs) ->
  -- |   "HH." <> name <> formatAttrs attrs

  -- | (TextNode content) ->
  -- |   case trim content of
  -- |     "" -> ""
  -- |     content' -> "HH.text \"" <> content' <> "\""

  -- | (CommentNode content) -> "-- " <> trim content

  -- | (Element name attrs children) ->
  -- |   let
  -- |     tagHtml = if length (fromFoldable attrs) > 0
  -- |       then "HH." <> name <> formatAttrs attrs
  -- |       else "HH." <> name <> "_"
  -- |     childHtml = indent 2 $ formatList true $ map (renderTree 0) children

  -- |   in
  -- |     tagHtml <> "\n" <> childHtml
  -- | where
  -- |   attrsOnNewLine = false
  -- |   formatAttrs attrs = if' attrsOnNewLine "\n  " " " <> renderAttrs attrs


-- | renderAttr :: Attribute -> String
-- | renderAttr = case _ of
-- |   Attribute "class" val ->
-- |     let
-- |       names = map (\v -> "\"" <> v <> "\"") $ split (Pattern " ") val
-- |     in
-- |       case names of
-- |         [] -> ""
-- |         [name] -> "HP.class_ $ H.ClassName " <> name
-- |         names' -> "HP.classes $ H.ClassName <$> " <> formatList false (toUnfoldable names')
-- |   Attribute "id" val ->
-- |     "HP.id_ " <> quote val
-- |   Attribute key val ->
-- |     "HP." <> key <> " " <> quote val
