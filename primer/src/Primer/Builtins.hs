-- | This module defines some builtin types that are used to seed initial programs.
--   The definitions here are no different than ones than a user can create, except
--   for the fact that some of the primitive functions (see "Primer.Primitives")
--   refer to these types.
module Primer.Builtins (
  tBool,
  cTrue,
  cFalse,
  boolDef,
  tNat,
  cZero,
  cSucc,
  natDef,
  tList,
  cNil,
  cCons,
  listDef,
  tMaybe,
  cNothing,
  cJust,
  maybeDef,
  tPair,
  cMakePair,
  pairDef,
  tEither,
  cLeft,
  cRight,
  eitherDef,
  builtinModuleName,
) where

import Primer.Core.Meta (
  GlobalName,
  ModuleName,
  TyConName,
  ValConName,
  mkSimpleModuleName,
  qualifyName,
 )
import Primer.Core.Type (
  Kind (KType),
  Type' (TApp, TCon, TVar),
 )
import Primer.Name (Name)
import Primer.TypeDef (ASTTypeDef (..), ValCon (ValCon))

builtinModuleName :: ModuleName
builtinModuleName = mkSimpleModuleName "Builtins"

builtin :: Name -> GlobalName k
builtin = qualifyName builtinModuleName

tBool :: TyConName
tBool = builtin "Bool"
cTrue, cFalse :: ValConName
cTrue = builtin "True"
cFalse = builtin "False"

tNat :: TyConName
tNat = builtin "Nat"
cZero, cSucc :: ValConName
cZero = builtin "Zero"
cSucc = builtin "Succ"

tList :: TyConName
tList = builtin "List"
cNil, cCons :: ValConName
cNil = builtin "Nil"
cCons = builtin "Cons"

tMaybe :: TyConName
tMaybe = builtin "Maybe"
cNothing :: ValConName
cNothing = builtin "Nothing"
cJust :: ValConName
cJust = builtin "Just"

tPair :: TyConName
tPair = builtin "Pair"
cMakePair :: ValConName
cMakePair = builtin "MakePair"

tEither :: TyConName
tEither = builtin "Either"
cLeft, cRight :: ValConName
cLeft = builtin "Left"
cRight = builtin "Right"

-- | A definition of the Bool type
boolDef :: ASTTypeDef ()
boolDef =
  ASTTypeDef
    { astTypeDefParameters = []
    , astTypeDefConstructors =
        [ ValCon cTrue []
        , ValCon cFalse []
        ]
    , astTypeDefNameHints = ["p", "q"]
    }

-- | A definition of the Nat type
natDef :: ASTTypeDef ()
natDef =
  ASTTypeDef
    { astTypeDefParameters = []
    , astTypeDefConstructors =
        [ ValCon cZero []
        , ValCon cSucc [TCon () tNat]
        ]
    , astTypeDefNameHints = ["i", "j", "n", "m"]
    }

-- | A definition of the List type
listDef :: ASTTypeDef ()
listDef =
  ASTTypeDef
    { astTypeDefParameters = [("a", KType)]
    , astTypeDefConstructors =
        [ ValCon cNil []
        , ValCon cCons [TVar () "a", TApp () (TCon () tList) (TVar () "a")]
        ]
    , astTypeDefNameHints = ["xs", "ys", "zs"]
    }

-- | A definition of the Maybe type
maybeDef :: ASTTypeDef ()
maybeDef =
  ASTTypeDef
    { astTypeDefParameters = [("a", KType)]
    , astTypeDefConstructors =
        [ ValCon cNothing []
        , ValCon cJust [TVar () "a"]
        ]
    , astTypeDefNameHints = ["mx", "my", "mz"]
    }

-- | A definition of the Pair type
pairDef :: ASTTypeDef ()
pairDef =
  ASTTypeDef
    { astTypeDefParameters = [("a", KType), ("b", KType)]
    , astTypeDefConstructors = [ValCon cMakePair [TVar () "a", TVar () "b"]]
    , astTypeDefNameHints = []
    }

-- | A definition of the Either type
eitherDef :: ASTTypeDef ()
eitherDef =
  ASTTypeDef
    { astTypeDefParameters = [("a", KType), ("b", KType)]
    , astTypeDefConstructors = [ValCon cLeft [TVar () "a"], ValCon cRight [TVar () "b"]]
    , astTypeDefNameHints = []
    }
