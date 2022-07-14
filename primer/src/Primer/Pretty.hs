module Primer.Pretty (
  prettyExpr,
  prettyPrintExpr,
  PrettyOptions (..),
  defaultPrettyOptions,
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
  Type' (..),
 )
import Primer.Name (Name (unName))

newtype PrettyOptions = PrettyOptions
  { fullyQualify :: Bool
  }

defaultPrettyOptions :: PrettyOptions
defaultPrettyOptions =
  PrettyOptions
    { fullyQualify = False
    }

-- | Pretty prints Expr' using Prettyprinter library
prettyExpr :: PrettyOptions -> Expr' a b -> Doc AnsiStyle
prettyExpr opts expr = case expr of
  Hole _ e -> col Red "{" <> pE e <> col Red "}"
  EmptyHole _ -> col Red "?"
  Con _ n -> gname opts n
  Var _ v -> case v of
    GlobalVarRef n -> gname opts n
    LocalVarRef n -> lname n
  Lam _ n e ->
    col Magenta "λ"
      <> lname n
      <> col Magenta "."
      <> line
      <> indent 2 (pE e)
  LAM _ n e ->
    col Magenta "Λ"
      <> lname n
      <> col Magenta "."
      <> line
      <> indent 2 (pE e)
  Case _ e bs ->
    col Yellow "match"
      <+> pE e
      <+> col Yellow "with"
      <+> line
      <+> indent
        2
        ( mconcat
            ( intersperse
                line
                $ map
                  ( \(CaseBranch n bs' e') ->
                      gname opts n
                        <+> mconcat
                          ( intersperse space $
                              map
                                (\(Bind _ n') -> lname n')
                                bs'
                          )
                        <+> col Yellow "→"
                        <+> pE e'
                  )
                  bs
            )
        )
  Ann _ e t -> typeann e t
  App _ e e' -> brac (pE e) <+> brac (pE e')
  APP _ e t -> brac (pE e) <+> col Yellow "@" <> pT t
  Let _ v e e' ->
    col Yellow "let"
      <+> lname v
      <+> col Yellow "="
        <> line
        <> indent 2 (pE e)
        <> line
        <> col Yellow "in"
        <> line
        <> indent 2 (pE e')
  LetType _ v t e ->
    col Yellow "let type"
      <+> lname v
      <+> col Yellow "="
        <> line
        <> indent 2 (pT t)
        <> line
        <> col Yellow "in"
        <> line
        <> indent 2 (pE e)
  Letrec _ v e t e' ->
    col Yellow "let rec"
      <+> lname v
      <+> col Yellow "="
        <> line
        <> indent 2 (typeann e t)
        <> line
        <> col Yellow "in"
        <> line
        <> indent 2 (pE e')
  PrimCon _ p -> case p of
    PrimChar c -> "Char" <+> pretty @Text (show c)
    PrimInt n -> "Int" <+> pretty @Text (show n)
  where
    typeann e t = brac (pE e) <+> col Green "::" <+> pT t
    pT = prettyType opts
    pE = prettyExpr opts

-- Unwraps global variable names as Doc type. First argument is options
--  fullyQualify = True then "Module.BaseName"
gname :: PrettyOptions -> GlobalName k -> Doc AnsiStyle
gname opts n =
  annotate
    (color Green)
    $ (if fullyQualify opts then mconcat (module_ $ qualifiedModule n) <> "." else mempty)
      <> pretty (unName (baseName n))
  where
    module_ = intersperse "." . toList . map (pretty . unName) . unModuleName

-- Unwraps local variable name as Doc
lname :: LocalName k -> Doc AnsiStyle
lname = col Cyan . pretty . unName . unLocalName

-- Adds curly brackets
brac :: (Semigroup a, IsString a) => a -> a
brac doc = "(" <> doc <> ")"

col :: Color -> Doc AnsiStyle -> Doc AnsiStyle
col = annotate . color

-- | Pretty prints Type' using Prettyprinter library
prettyType :: PrettyOptions -> Type' b -> Doc AnsiStyle
prettyType opts typ = case typ of
  TEmptyHole _ -> col Red "?"
  THole _ t -> col Red "{" <> pT t <> col Red "}"
  TCon _ n -> gname opts n
  TFun _ t1 t2 -> case t1 of
    TFun{} -> brac (pT t1) <+> col Yellow "->" <+> pT t2
    _ -> pT t1 <+> col Yellow "->" <+> pT t2
  TVar _ n -> lname n
  TApp _ t1 t2 -> pT t1 <+> brac (pT t2)
  TForall _ n _ t -> col Yellow "∀" <+> lname n <> col Yellow "." <+> pT t
  where
    pT = prettyType opts

prettyPrintExpr :: Expr -> IO ()
prettyPrintExpr e = do
  putDoc $ prettyExpr defaultPrettyOptions e
  putStrLn ("" :: Text)
