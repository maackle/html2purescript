module Parser.Halogen where

import Prelude

import Data.Array (filter, fromFoldable, length, replicate, toUnfoldable)
import Data.Either (Either)
import Data.List (List)
import Data.String (Pattern(..), joinWith, split, trim)
import Data.String.CodeUnits (fromCharArray)
import Text.HTML.Parser (Attribute(..), HTML(..), parseHTML)
import Text.Parsing.StringParser (ParseError)

toHalogen :: String -> Either ParseError String
toHalogen =
  map (formatList true) <<< (map <<< map) (renderTree 0) <<< parseHTML

if' :: ∀ a. Boolean -> a -> a -> a
if' v a b = if v then a else b

onlyIf :: ∀ m. Monoid m => Boolean -> m -> m
onlyIf cond v = if cond then v else mempty

renderTree :: Int -> HTML -> String
renderTree d = case _ of
  (VoidElement name attrs) ->
    "HH." <> name <> formatAttrs attrs

  (TextNode content) ->
    case trim content of
      "" -> ""
      content' -> "HH.text \"" <> content' <> "\""

  (CommentNode content) -> "-- " <> trim content

  (Element name attrs children) ->
    let
      tagHtml = if length (fromFoldable attrs) > 0
        then "HH." <> name <> formatAttrs attrs
        else "HH." <> name <> "_"
      childHtml = indent 2 $ formatList true $ map (renderTree 0) children

    in
      tagHtml <> "\n" <> childHtml
  where
    attrsOnNewLine = false
    formatAttrs attrs = if' attrsOnNewLine "\n  " " " <> renderAttrs attrs


pad :: Int -> String
pad by =
  (fromCharArray $ replicate by ' ')

indent :: Int -> String -> String
indent by =
  split (Pattern "\n") >>> map (pad by <> _) >>> joinWith "\n"

quote :: String -> String
quote v = "\"" <> v <> "\""

formatList :: Boolean -> List String -> String
formatList nl ls =
  "[ " <> inner <> (o "\n" " ") <> "]"
  where
    arr = fromFoldable ls
    useNewline = nl && length arr > 1
    o = if' useNewline
    sep = o "\n" "" <> ", "
    inner = ls # fromFoldable # filter (_ /= "") # joinWith sep

renderAttrs :: List Attribute -> String
renderAttrs attrs =
  "[ " <> joinWith ", " (fromFoldable $ map renderAttr attrs) <> " ]"

renderAttr :: Attribute -> String
renderAttr = case _ of
  Attribute "class" val ->
    let
      names = map (\v -> "\"" <> v <> "\"") $ split (Pattern " ") val
    in
      case names of
        [] -> ""
        [name] -> "HP.class_ $ H.ClassName " <> name
        names' -> "HP.classes $ H.ClassName <$> " <> formatList false (toUnfoldable names')

  Attribute "id" val ->
    "HP.id_ " <> quote val
  Attribute key val ->
    "HP." <> key <> " " <> quote val
