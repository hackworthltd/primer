module Primer.Pretty (
  prettyExpr,
  prettyType,
  prettyPrintExpr,
  prettyPrintType,
  PrettyOptions (..),
  sparse,
  compact,
) where

import Foreword hiding (group)

import Prettyprinter (
  Doc,
  Pretty (pretty),
  annotate,
  flatAlt,
  group,
  indent,
  line,
  line',
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
  Type,
  Type' (..),
 )
import Primer.Name (Name (unName))

data PrettyOptions = PrettyOptions
  { fullyQualify :: Bool
  -- ^ Global variable names are printed with the parent module
  , inlineHoles :: Bool
  -- ^ Nonempty holes are printed on one line
  , inlineLet :: Bool
  -- ^ Attempt to print @let x = expr in@ on one line
  , inlineLambda :: Bool
  -- ^ Attempt to print λs and Λs on one line
  , inlineForAll :: Bool
  -- ^ Attempt to print @for all@ and associated type sig on one line
  , inlineMatch :: Bool
  -- ^ Attempt to print each case on one line
  }

-- | Default PrettyOptions - makes no attempt to group text
sparse :: PrettyOptions
sparse =
  PrettyOptions
    { fullyQualify = False
    , inlineHoles = False
    , inlineLet = False
    , inlineLambda = False
    , inlineForAll = False
    , inlineMatch = False
    }

-- | Groups whenever possible
compact :: PrettyOptions
compact =
  PrettyOptions
    { fullyQualify = False
    , inlineHoles = True
    , inlineLet = True
    , inlineLambda = True
    , inlineForAll = True
    , inlineMatch = True
    }

-- | Pretty prints @Expr'@ using Prettyprinter library
prettyExpr :: PrettyOptions -> Expr' a b -> Doc AnsiStyle
prettyExpr opts = \case
  Hole _ e -> (if inlineHoles opts then group else identity) (brac Curly Red (pE e))
  EmptyHole _ -> col Red "?"
  Con _ n -> col Green (gname opts n)
  Var _ v -> case v of
    GlobalVarRef n -> col Blue (gname opts n)
    LocalVarRef n -> lname n
  Lam _ n e ->
    (if inlineLambda opts then group else identity)
      ( col Magenta "λ"
          <> lname n
          <> col Magenta "."
          <> line
          <> indent 2 (pE e)
      )
  LAM _ n e ->
    (if inlineLambda opts then group else identity)
      ( col Magenta "Λ"
          <> lname n
          <> col Magenta "."
          <> line
          <> indent 2 (pE e)
      )
  Case _ e bs ->
    col Yellow "match"
      <+> pE e
      <+> col Yellow "with"
        <> line
      <+> indent'
        2
        ( mconcat
            ( intersperse
                line
                $ map
                  ( \(CaseBranch n bs' e') ->
                      col Green (gname opts n)
                        <+> mconcat
                          ( intersperse space $
                              map
                                (\(Bind _ n') -> lname n')
                                bs'
                          )
                        <+> col Yellow "→"
                          <> (if inlineMatch opts then group else identity)
                            ( line
                                <> indent' 2 (pE e')
                            )
                  )
                  bs
            )
        )
  Ann _ e t -> typeann e t
  App _ e e' -> brac Round White (pE e) <> line <> brac Round White (pE e')
  APP _ e t -> brac Round Yellow (pE e) <+> col Yellow "@" <> pT t
  Let _ v e e' ->
    col Yellow "let"
      <+> lname v
      <+> col Yellow "="
        <> inlineblock opts (pE e)
        <> col Yellow "in"
        <> line
        <> indent 2 (pE e')
  LetType _ v t e ->
    col Yellow "let type"
      <+> lname v
      <+> col Yellow "="
        <> inlineblock opts (pT t)
        <> col Yellow "in"
        <> line
        <> indent 2 (pE e)
  Letrec _ v e t e' ->
    col Yellow "let rec"
      <+> lname v
      <+> col Yellow "="
        <> inlineblock opts (typeann e t)
        <> col Yellow "in"
        <> line
        <> indent 2 (pE e')
  PrimCon _ p -> case p of
    PrimChar c -> "Char" <+> pretty @Text (show c)
    PrimInt n -> "Int" <+> pretty @Text (show n)
  where
    typeann e t = brac Round Yellow (pE e) <+> col Yellow "::" <> line <> brac Round Yellow (pT t)
    pT = prettyType opts
    pE = prettyExpr opts

-- When grouped: " x "
-- When ungrouped: "\n\tx\n"
inlineblock :: PrettyOptions -> Doc AnsiStyle -> Doc AnsiStyle
inlineblock opts x =
  (if inlineLet opts then group else identity)
    ( line
        <> indent' 2 x
        <> line
    )

-- When grouped: ""
-- When ungrouped: "\t"
indent' :: Int -> Doc AnsiStyle -> Doc AnsiStyle
indent' n x = flatAlt (indent n x) x

-- Unwraps global variable names as Doc type.
gname :: PrettyOptions -> GlobalName k -> Doc AnsiStyle
gname opts n =
  (if fullyQualify opts then mconcat (module_ $ qualifiedModule n) <> "." else mempty)
    <> pretty (unName (baseName n))
  where
    module_ = intersperse "." . toList . map (pretty . unName) . unModuleName

-- Unwraps local variable name as Doc
lname :: LocalName k -> Doc AnsiStyle
lname = col Cyan . pretty . unName . unLocalName

data BracketType = Round | Curly

lBrac :: BracketType -> Doc AnsiStyle
lBrac Round = "("
lBrac Curly = "{?"

rBrac :: BracketType -> Doc AnsiStyle
rBrac Round = ")"
rBrac Curly = "?}"

-- Adds brackets of type b around "doc" with color c
brac :: BracketType -> Color -> Doc AnsiStyle -> Doc AnsiStyle
brac b c doc = col c (lBrac b) <> line' <> flatAlt (indent 2 doc) doc <> line' <> col c (rBrac b)

col :: Color -> Doc AnsiStyle -> Doc AnsiStyle
col = annotate . color

-- | Pretty prints @Type'@ using Prettyprinter library
prettyType :: PrettyOptions -> Type' b -> Doc AnsiStyle
prettyType opts typ = case typ of
  TEmptyHole _ -> col Red "?"
  THole _ t -> (if inlineHoles opts then group else identity) (brac Curly Red (pT t))
  TCon _ n -> col Green (gname opts n)
  TFun _ t1 t2 -> case t1 of
    TFun{} -> brac Round Yellow (pT t1) <+> col Yellow "->" <+> pT t2
    _ -> pT t1 <+> col Yellow "->" <+> pT t2
  TVar _ n -> lname n
  TApp _ t1 t2 -> brac Round White (pT t1) <> line <> brac Round White (pT t2)
  TForall _ n _ t ->
    (if inlineLambda opts then group else identity)
      ( col Yellow "∀"
          <+> lname n <> col Yellow "." <> line <> indent' 2 (pT t)
      )
  where
    pT = prettyType opts

prettyPrintExpr :: Expr -> IO ()
prettyPrintExpr e = do
  putDoc $ prettyExpr sparse e
  putStrLn ("" :: Text)

prettyPrintType :: Type -> IO ()
prettyPrintType t = do
  putDoc $ prettyType sparse t
  putStrLn ("" :: Text)
