-- | This module defines some builtin types that are used to seed initial programs.
--   The definitions here are no different than ones than a user can create, except
--   for the fact that some of the primitive functions (see "Primer.Primitives")
--   refer to these types.
module Primer.Builtins (
  boolDef,
  natDef,
  listDef,
  maybeDef,
  pairDef,
  eitherDef,
) where

import Primer.Core (
  ASTTypeDef (
    ASTTypeDef,
    astTypeDefConstructors,
    astTypeDefName,
    astTypeDefNameHints,
    astTypeDefParameters
  ),
  Kind (KType),
  TyConName,
  Type' (TApp, TCon, TVar),
  ValCon (ValCon),
  ValConName,
 )

tBool :: TyConName
tBool = "Bool"
cTrue, cFalse :: ValConName
cTrue = "True"
cFalse = "False"

tNat :: TyConName
tNat = "Nat"
cZero, cSucc :: ValConName
cZero = "Zero"
cSucc = "Succ"

tList :: TyConName
tList = "List"
cNil, cCons :: ValConName
cNil = "Nil"
cCons = "Cons"

tMaybe :: TyConName
tMaybe = "Maybe"
cNothing :: ValConName
cNothing = "Nothing"
cJust :: ValConName
cJust = "Just"

tPair :: TyConName
tPair = "Pair"
cMakePair :: ValConName
cMakePair = "MakePair"

tEither :: TyConName
tEither = "Either"
cLeft, cRight :: ValConName
cLeft = "Left"
cRight = "Right"

-- | A definition of the Bool type
boolDef :: ASTTypeDef
boolDef =
  ASTTypeDef
    { astTypeDefName = tBool
    , astTypeDefParameters = []
    , astTypeDefConstructors =
        [ ValCon cTrue []
        , ValCon cFalse []
        ]
    , astTypeDefNameHints = ["p", "q"]
    }

-- | A definition of the Nat type
natDef :: ASTTypeDef
natDef =
  ASTTypeDef
    { astTypeDefName = tNat
    , astTypeDefParameters = []
    , astTypeDefConstructors =
        [ ValCon cZero []
        , ValCon cSucc [TCon () tNat]
        ]
    , astTypeDefNameHints = ["i", "j", "n", "m"]
    }

-- | A definition of the List type
listDef :: ASTTypeDef
listDef =
  ASTTypeDef
    { astTypeDefName = tList
    , astTypeDefParameters = [("a", KType)]
    , astTypeDefConstructors =
        [ ValCon cNil []
        , ValCon cCons [TVar () "a", TApp () (TCon () tList) (TVar () "a")]
        ]
    , astTypeDefNameHints = ["xs", "ys", "zs"]
    }

-- | A definition of the Maybe type
maybeDef :: ASTTypeDef
maybeDef =
  ASTTypeDef
    { astTypeDefName = tMaybe
    , astTypeDefParameters = [("a", KType)]
    , astTypeDefConstructors =
        [ ValCon cNothing []
        , ValCon cJust [TVar () "a"]
        ]
    , astTypeDefNameHints = ["mx", "my", "mz"]
    }

-- | A definition of the Pair type
pairDef :: ASTTypeDef
pairDef =
  ASTTypeDef
    { astTypeDefName = tPair
    , astTypeDefParameters = [("a", KType), ("b", KType)]
    , astTypeDefConstructors = [ValCon cMakePair [TVar () "a", TVar () "b"]]
    , astTypeDefNameHints = []
    }

-- | A definition of the Either type
eitherDef :: ASTTypeDef
eitherDef =
  ASTTypeDef
    { astTypeDefName = tEither
    , astTypeDefParameters = [("a", KType), ("b", KType)]
    , astTypeDefConstructors = [ValCon cLeft [TVar () "a"], ValCon cRight [TVar () "b"]]
    , astTypeDefNameHints = []
    }
