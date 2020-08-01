module Parser.Halogen where

import Language.PS.SmartCST
import Prelude

import Parser.Halogen.Utils
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
          exprClassNameConstructor
          `ExprApp`
          ( ExprString className )
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
            (ExprVar (Ident name))
          names' -> Array.singleton $
            (ExprIdent (fromHalogenHP (Ident "classes")))
            `ExprApp`
            (ExprArray $ names' <#> (\name -> ExprVar (Ident name)))
      Attribute key val -> Array.singleton $
        (ExprIdent (fromHalogenHP (Ident key)))
        `ExprApp`
        (ExprString val)

    renderHtml :: HTML -> Array Expr
    renderHtml =
      case _ of
        Element name attrs children -> Array.singleton $
          (ExprIdent (fromHalogenHH (Ident name)))
          `ExprApp`
          (ExprArray [])
          `ExprApp`
          ExprArray (fold $ map renderHtml children)
        VoidElement name attrs -> Array.singleton $
          (ExprIdent (fromHalogenHH (Ident name)))
          -- | `ExprApp`
          -- | (ExprArray [])
        TextNode content ->
          case String.trim content of
            "" -> []
            content' -> Array.singleton $ (ExprIdent (fromHalogenHH (Ident "text"))) `ExprApp` (ExprString content')
        CommentNode content -> []
   in \htmls ->
     case fold $ map renderHtml htmls of
          [a] -> a
          other -> ExprArray other
