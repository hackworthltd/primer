module Primer.Pretty (
  prettyExpr,
  prettyPrintExpr,
  PrettyExprOptions (..),
  defaultPrettyExprOptions,
) where

import Foreword

import Prettyprinter (
  Doc,
  Pretty (pretty),
  annotate,
  indent,
  line,
  space,
  (<+>),
 )
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (..),
  color,
  putDoc,
 )
import Primer.Core (
  Bind' (Bind),
  CaseBranch' (CaseBranch),
  Expr,
  Expr' (..),
  GlobalName (baseName, qualifiedModule),
  LocalName (unLocalName),
  ModuleName (unModuleName),
  PrimCon (..),
  TmVarRef (GlobalVarRef, LocalVarRef),
  Type',
 )
import Primer.Name (Name (unName))

newtype PrettyExprOptions = PrettyExprOptions
  { fullyQualify :: Bool
  }

defaultPrettyExprOptions :: PrettyExprOptions
defaultPrettyExprOptions =
  PrettyExprOptions
    { fullyQualify = False
    }

prettyExpr :: PrettyExprOptions -> Expr' a b -> Doc AnsiStyle
prettyExpr opts expr = case expr of
  Hole _ e -> annotate (color Red) "{" <+> pE e <+> annotate (color Red) "}"
  EmptyHole _ -> annotate (color Red) "?"
  Con _ n -> gname n
  Var _ v -> case v of
    GlobalVarRef n -> gname n
    LocalVarRef n -> lname n
  Lam _ n e ->
    annotate (color Magenta) "λ" <> lname n <> annotate (color Magenta) "."
      <+> pE e
  LAM _ n e ->
    annotate (color Magenta) "Λ" <> lname n <> annotate (color Magenta) "."
      <+> pE e
  Case _ e bs ->
    annotate (color Yellow) "match"
      <+> pE e
      <+> annotate (color Yellow) "with"
      <+> line
      <+> indent
        2
        ( mconcat
            ( intersperse
                line
                $ map
                  ( \(CaseBranch n bs' e') ->
                      gname n
                        <+> mconcat
                          ( intersperse space $
                              map
                                (\(Bind _ n') -> lname n')
                                bs'
                          )
                        <+> annotate (color Yellow) "→"
                        <+> pE e'
                  )
                  bs
            )
        )
  Ann _ e t -> typeann e t
  App _ e e' -> brac (pE e) <+> brac (pE e')
  APP _ e t -> brac (pE e) <+> annotate (color Yellow) "@" <> prettyType t
  Let _ v e e' ->
    annotate (color Yellow) "let"
      <+> lname v
      <+> annotate (color Yellow) "="
      <+> pE e
      <+> annotate (color Yellow) "in"
      <+> pE e'
  LetType _ v t e ->
    annotate (color Yellow) "let type"
      <+> lname v
      <+> annotate (color Yellow) "="
      <+> prettyType t
      <+> annotate (color Yellow) "in"
      <+> pE e
  Letrec _ v e t e' ->
    annotate (color Yellow) "let rec"
      <+> lname v
      <+> annotate (color Yellow) "="
      <+> typeann e t
      <+> annotate (color Yellow) "in"
      <+> pE e'
  PrimCon _ p -> case p of
    PrimChar c -> "Char" <+> pretty @Text (show c)
    PrimInt n -> "Int" <+> pretty @Text (show n)
  where
    typeann e t = brac (pE e) <+> annotate (color Green) "::" <+> prettyType t
    pE = prettyExpr opts
    gname n =
      annotate
        (color Green)
        $ (if fullyQualify opts then mconcat (module_ $ qualifiedModule n) <> "." else mempty)
          <> pretty (unName (baseName n))
    module_ = intersperse "." . toList . map (pretty . unName) . unModuleName
    lname = annotate (color Cyan) . pretty . unName . unLocalName
    brac doc = "(" <> doc <> ")"

prettyType :: Type' b -> Doc AnsiStyle
prettyType _ = "unimplemented"

prettyPrintExpr :: Expr -> IO ()
prettyPrintExpr e = do
  putDoc $ prettyExpr defaultPrettyExprOptions e
  putStrLn ("" :: Text)
