-- | Action priorities. Smaller values are higher priority. They can
-- be indexed by 'Level', but we currently don't use that
-- functionality.
--
-- The values chosen here are relatively arbitrary, though we expect
-- the higher priority actions to be used more often than lower
-- priority actions. Exceptions to the rule are:
--
-- - 'makeCase': this is used very early in beginner lessons, so we
-- make it easy to find near the top of the list. See
-- https://github.com/hackworthltd/vonnegut/issues/786
--
-- - Delete: we always put this last, because it's destructive.
--
-- Expression actions.
--
-- For reference, the expectation is that variable,
-- constructor-with-argument, and nullary constructor uses will be
-- the most frequently used actions; followed by function
-- application; then special forms like @case@, @let@, etc. Note
-- that we put the @$@ variant of function application fairly low on
-- the list because most of the time we expect students to use our
-- special @f $ ?@ action, instead.
--
-- Besides the special case of 'delete' going last as mentioned
-- above, we also make one other exception: 'makeLambda' goes first,
-- because it's the first thing students will typically want to do
-- when building a new definition's expression. (Arguably we should
-- similarly promote 'makeTypeAbstraction' at intermediate and
-- expert levels, but we don't currently implement this.)
module Primer.Action.Priorities where

import Foreword

import Primer.Action (Level)

makeLambda :: Level -> Int
makeLambda _ = 5

useVar :: Level -> Int
useVar _ = 10

useValueCon :: Level -> Int
useValueCon _ = 11

makeCase :: Level -> Int
makeCase _ = 12

useSaturatedValueCon :: Level -> Int
useSaturatedValueCon _ = 13

useFunction :: Level -> Int
useFunction _ = 14

makeLet :: Level -> Int
makeLet _ = 21

makeLetRecursive :: Level -> Int
makeLetRecursive _ = 22

makeLetrec :: Level -> Int
makeLetrec _ = 23

applyFunction :: Level -> Int
applyFunction _ = 30

applyType :: Level -> Int
applyType _ = 40

makeTypeAbstraction :: Level -> Int
makeTypeAbstraction _ = 50

annotateExpr :: Level -> Int
annotateExpr _ = 51

removeAnnotation :: Level -> Int
removeAnnotation _ = 52

finishHole :: Level -> Int
finishHole _ = 500

enterHole :: Level -> Int
enterHole _ = 501

-- | Type actions.
constructFunction :: Level -> Int
constructFunction _ = 5

addInput :: Level -> Int
addInput _ = 6

useTypeVar :: Level -> Int
useTypeVar _ = 10

useTypeCon :: Level -> Int
useTypeCon _ = 11

constructTypeApp :: Level -> Int
constructTypeApp _ = 20

constructForall :: Level -> Int
constructForall _ = 40

-- | Generic actions.
rename :: Level -> Int
rename _ = 200

duplicate :: Level -> Int
duplicate _ = 201

raise :: Level -> Int
raise _ = 300

delete :: Level -> Int
delete _ = maxBound
