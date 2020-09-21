module Parser.Halogen where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.Foldable (fold)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (stripPrefix)
import Data.String.Pattern (Pattern(..))
import Dodo as Dodo
import Language.PS.SmartCST (Declaration(..), Expr(..), Guarded(..), Ident(..), Module(..), ProperName(..), SmartQualifiedName(..), Type(..), TypeVarBinding(..), mkModuleName, printModule)
import Parser.Halogen.Utils (classNameToFunctionName, collectClassNames, exprClassNameConstructor, fromHalogenHH, fromHalogenHP, fromHalogenHPAria, stringToClasses)
import Text.HTML.Parser (Attribute(..), HTML(..), parseHTML)
import Text.Parsing.StringParser (ParseError)
import Data.String.Extra as Data.String.Extra

toHalogen :: String -> Either ParseError String
toHalogen content = parseHTML content <#> \htmls ->
  Dodo.print Dodo.plainText Dodo.twoSpaces $ printModule $ outputModule htmls

outputModule :: List HTML -> Module
outputModule htmls =
  Module
  { moduleName: mkModuleName $ NonEmptyArray.singleton "YourModule"
  , exports: []
  , declarations:
    (join $ map definedClassName $ collectClassNames htmls)
    <>
    [ DeclSignature
      { comments: Nothing
      , ident: Ident "html"
      , type_: TypeForall (NonEmptyArray.cons' (TypeVarName (Ident "w")) [(TypeVarName (Ident "i"))])
        ( TypeConstructor
          ( SmartQualifiedName__Custom
            (mkModuleName $ NonEmptyArray.cons' "Halogen" ["HTML"])
            (mkModuleName $ NonEmptyArray.cons' "HH" [])
            (ProperName "HTML")
          )
          `TypeApp`
          (TypeVar $ Ident "w")
          `TypeApp`
          (TypeVar $ Ident "i")
        )
      }
    , DeclValue
      { comments: Nothing
      , valueBindingFields:
        { name: Ident "html"
        , binders: []
        , guarded: Unconditional
          { whereBindings: []
          , expr: renderTree htmls
          }
        }
      }
    ]
  }

definedClassName :: String -> Array Declaration
definedClassName className =
  [ DeclSignature
    { comments: Nothing
    , ident: Ident (classNameToFunctionName className)
    , type_: TypeConstructor
      ( SmartQualifiedName__Simple
        (mkModuleName $ NonEmptyArray.cons' "Halogen" ["HTML"])
        (ProperName "ClassName")
      )
    }
  , DeclValue
    { comments: Nothing
    , valueBindingFields:
      { name: Ident (classNameToFunctionName className)
      , binders: []
      , guarded: Unconditional
        { whereBindings: []
        , expr:
          exprClassNameConstructor
          `ExprApp`
          (ExprString className)
        }
      }
    }
  ]

renderTree :: List HTML -> Expr
renderTree =
  let
    renderAttr :: Attribute -> Array Expr
    renderAttr = case _ of
      Attribute "class" val ->
        case stringToClasses val of
          [] -> []
          [name] -> Array.singleton $
            (ExprIdent (fromHalogenHP (Ident "class_")))
            `ExprApp`
            (ExprVar (Ident (classNameToFunctionName name)))
          names' -> Array.singleton $
            (ExprIdent (fromHalogenHP (Ident "classes")))
            `ExprApp`
            (ExprArray $ names' <#> (\name -> ExprVar (Ident (classNameToFunctionName name))))
      Attribute key val ->
        let
          name =
            case stripPrefix (Pattern "aria-") key of
                 Nothing ->
                   let
                     appendUnderscore = Array.elem key ["id"]
                     key' = if appendUnderscore then key <> "_" else key
                   in fromHalogenHP (Ident key')
                 Just "labelledby" -> fromHalogenHPAria $ Ident "labelledBy"
                 Just key' -> fromHalogenHPAria $ Ident $ Data.String.Extra.camelCase key'
         in
            Array.singleton $
            (ExprIdent name)
            `ExprApp`
            (ExprString val)

    renderHtml :: HTML -> Array Expr
    renderHtml =
      case _ of
        Element name attrs children -> Array.singleton $
          (ExprIdent (fromHalogenHH (Ident name)))
          `ExprApp`
          (ExprArray (fold $ map renderAttr attrs))
          `ExprApp`
          ExprArray (fold $ map renderHtml children)
        VoidElement name attrs -> Array.singleton $
          (ExprIdent (fromHalogenHH (Ident name)))
          `ExprApp`
          (ExprArray (fold $ map renderAttr attrs))
        TextNode content ->
          case String.trim content of
            "" -> []
            content' -> Array.singleton $ (ExprIdent (fromHalogenHH (Ident "text"))) `ExprApp` (ExprString content')
        CommentNode content -> []
   in \htmls ->
     case fold $ map renderHtml htmls of
          [a] -> a
          other -> ExprArray other
