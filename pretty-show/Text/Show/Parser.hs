{-# OPTIONS_GHC -w #-}
-- We use these options because Happy generates code with a lot of warnings.
{-# LANGUAGE Trustworthy #-}
module Text.Show.Parser (parseValue) where

import Text.Show.Value
import Language.Haskell.Lexer
import Data.List(intercalate)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31
	= HappyTerminal (PosToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Value)
	| HappyAbsSyn5 ((String,Value))
	| HappyAbsSyn8 (String)
	| HappyAbsSyn10 ((Name,Value))
	| HappyAbsSyn11 ([Value])
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,264) ([0,0,57937,2663,0,8192,64586,332,0,0,16,0,0,0,52228,2,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,61480,1331,0,4096,65061,191,0,41472,53188,20,0,0,2176,0,0,648,21311,0,0,4608,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,32768,61736,1331,0,0,512,0,0,0,64,0,0,0,0,0,0,1024,0,0,0,128,0,0,0,160,0,0,32768,0,0,0,0,0,0,0,20512,0,0,0,4,0,0,32768,0,0,0,4096,0,0,0,512,0,0,0,64,0,0,0,0,0,0,0,0,1,0,0,0,0,0,41472,53188,20,0,0,0,0,0,16384,11456,0,0,0,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,64,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,4,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9488,42622,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,35140,10655,0,0,0,0,0,0,0,0,0,0,32,0,0,32768,0,0,0,0,0,0,0,8192,0,0,0,3072,0,0,0,16,0,0,0,0,0,0,0,0,0,0,50338,5327,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseValue","value","infixelem","app_value","avalue","con","infixcon","field","tuple","list1__avalue__","list1__infixelem__","prefix__CONSYM__","prefix__QCONSYM__","prefix__QVARSYM__","prefix__VARSYM__","sep__field__','__","sep__value__','__","sep1__value__','__","list__snd__','__value____","rev_list1__avalue__","rev_list1__infixelem__","sep1__field__','__","snd__','__value__","list__snd__','__field____","list1__snd__','__value____","snd__','__field__","list1__snd__','__field____","rev_list1__snd__','__value____","rev_list1__snd__','__field____","'='","'('","')'","'{'","'}'","'['","']'","'<'","'>'","','","'-'","'%'","'`'","':'","INT","FLOAT","STRING","CHAR","VARID","QVARID","VARSYM","QVARSYM","CONID","QCONID","CONSYM","QCONSYM","QQUOTE","RESOP","RESID","%eof"]
        bit_start = st Prelude.* 61
        bit_end = (st Prelude.+ 1) Prelude.* 61
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..60]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (33) = happyShift action_12
action_0 (37) = happyShift action_13
action_0 (39) = happyShift action_14
action_0 (42) = happyShift action_15
action_0 (46) = happyShift action_16
action_0 (47) = happyShift action_17
action_0 (48) = happyShift action_18
action_0 (49) = happyShift action_19
action_0 (50) = happyShift action_20
action_0 (51) = happyShift action_21
action_0 (54) = happyShift action_22
action_0 (55) = happyShift action_23
action_0 (58) = happyShift action_24
action_0 (60) = happyShift action_25
action_0 (4) = happyGoto action_26
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (12) = happyGoto action_6
action_0 (14) = happyGoto action_7
action_0 (15) = happyGoto action_8
action_0 (16) = happyGoto action_9
action_0 (17) = happyGoto action_10
action_0 (22) = happyGoto action_11
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (33) = happyShift action_12
action_1 (37) = happyShift action_13
action_1 (39) = happyShift action_14
action_1 (42) = happyShift action_15
action_1 (46) = happyShift action_16
action_1 (47) = happyShift action_17
action_1 (48) = happyShift action_18
action_1 (49) = happyShift action_19
action_1 (50) = happyShift action_20
action_1 (51) = happyShift action_21
action_1 (54) = happyShift action_22
action_1 (55) = happyShift action_23
action_1 (58) = happyShift action_24
action_1 (60) = happyShift action_25
action_1 (4) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (12) = happyGoto action_6
action_1 (14) = happyGoto action_7
action_1 (15) = happyGoto action_8
action_1 (16) = happyGoto action_9
action_1 (17) = happyGoto action_10
action_1 (22) = happyGoto action_11
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (43) = happyShift action_27
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (44) = happyShift action_48
action_3 (52) = happyShift action_49
action_3 (53) = happyShift action_50
action_3 (56) = happyShift action_51
action_3 (57) = happyShift action_52
action_3 (59) = happyShift action_53
action_3 (5) = happyGoto action_44
action_3 (9) = happyGoto action_45
action_3 (13) = happyGoto action_46
action_3 (23) = happyGoto action_47
action_3 _ = happyReduce_2

action_4 _ = happyReduce_54

action_5 (35) = happyShift action_43
action_5 _ = happyReduce_11

action_6 _ = happyReduce_5

action_7 _ = happyReduce_22

action_8 _ = happyReduce_23

action_9 _ = happyReduce_27

action_10 _ = happyReduce_26

action_11 (33) = happyShift action_12
action_11 (37) = happyShift action_13
action_11 (39) = happyShift action_14
action_11 (46) = happyShift action_16
action_11 (47) = happyShift action_17
action_11 (48) = happyShift action_18
action_11 (49) = happyShift action_19
action_11 (50) = happyShift action_20
action_11 (51) = happyShift action_21
action_11 (54) = happyShift action_22
action_11 (55) = happyShift action_23
action_11 (58) = happyShift action_24
action_11 (60) = happyShift action_25
action_11 (7) = happyGoto action_42
action_11 (8) = happyGoto action_5
action_11 (14) = happyGoto action_7
action_11 (15) = happyGoto action_8
action_11 (16) = happyGoto action_9
action_11 (17) = happyGoto action_10
action_11 _ = happyReduce_41

action_12 (33) = happyShift action_12
action_12 (37) = happyShift action_13
action_12 (39) = happyShift action_14
action_12 (42) = happyShift action_15
action_12 (46) = happyShift action_16
action_12 (47) = happyShift action_17
action_12 (48) = happyShift action_18
action_12 (49) = happyShift action_19
action_12 (50) = happyShift action_20
action_12 (51) = happyShift action_21
action_12 (52) = happyShift action_38
action_12 (53) = happyShift action_39
action_12 (54) = happyShift action_22
action_12 (55) = happyShift action_23
action_12 (56) = happyShift action_40
action_12 (57) = happyShift action_41
action_12 (58) = happyShift action_24
action_12 (60) = happyShift action_25
action_12 (4) = happyGoto action_36
action_12 (6) = happyGoto action_3
action_12 (7) = happyGoto action_4
action_12 (8) = happyGoto action_5
action_12 (11) = happyGoto action_37
action_12 (12) = happyGoto action_6
action_12 (14) = happyGoto action_7
action_12 (15) = happyGoto action_8
action_12 (16) = happyGoto action_9
action_12 (17) = happyGoto action_10
action_12 (22) = happyGoto action_11
action_12 _ = happyReduce_39

action_13 (33) = happyShift action_12
action_13 (37) = happyShift action_13
action_13 (39) = happyShift action_14
action_13 (42) = happyShift action_15
action_13 (46) = happyShift action_16
action_13 (47) = happyShift action_17
action_13 (48) = happyShift action_18
action_13 (49) = happyShift action_19
action_13 (50) = happyShift action_20
action_13 (51) = happyShift action_21
action_13 (54) = happyShift action_22
action_13 (55) = happyShift action_23
action_13 (58) = happyShift action_24
action_13 (60) = happyShift action_25
action_13 (4) = happyGoto action_33
action_13 (6) = happyGoto action_3
action_13 (7) = happyGoto action_4
action_13 (8) = happyGoto action_5
action_13 (12) = happyGoto action_6
action_13 (14) = happyGoto action_7
action_13 (15) = happyGoto action_8
action_13 (16) = happyGoto action_9
action_13 (17) = happyGoto action_10
action_13 (19) = happyGoto action_34
action_13 (20) = happyGoto action_35
action_13 (22) = happyGoto action_11
action_13 _ = happyReduce_50

action_14 (50) = happyShift action_31
action_14 (54) = happyShift action_32
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (33) = happyShift action_12
action_15 (37) = happyShift action_13
action_15 (39) = happyShift action_14
action_15 (46) = happyShift action_16
action_15 (47) = happyShift action_17
action_15 (48) = happyShift action_18
action_15 (49) = happyShift action_19
action_15 (50) = happyShift action_20
action_15 (51) = happyShift action_21
action_15 (54) = happyShift action_22
action_15 (55) = happyShift action_23
action_15 (58) = happyShift action_24
action_15 (60) = happyShift action_25
action_15 (7) = happyGoto action_30
action_15 (8) = happyGoto action_5
action_15 (14) = happyGoto action_7
action_15 (15) = happyGoto action_8
action_15 (16) = happyGoto action_9
action_15 (17) = happyGoto action_10
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (42) = happyShift action_28
action_16 (45) = happyShift action_29
action_16 _ = happyReduce_12

action_17 _ = happyReduce_13

action_18 _ = happyReduce_14

action_19 _ = happyReduce_15

action_20 _ = happyReduce_24

action_21 _ = happyReduce_25

action_22 _ = happyReduce_20

action_23 _ = happyReduce_21

action_24 _ = happyReduce_19

action_25 _ = happyReduce_30

action_26 (43) = happyShift action_27
action_26 (61) = happyAccept
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (33) = happyShift action_12
action_27 (37) = happyShift action_13
action_27 (39) = happyShift action_14
action_27 (42) = happyShift action_15
action_27 (46) = happyShift action_16
action_27 (47) = happyShift action_17
action_27 (48) = happyShift action_18
action_27 (49) = happyShift action_19
action_27 (50) = happyShift action_20
action_27 (51) = happyShift action_21
action_27 (54) = happyShift action_22
action_27 (55) = happyShift action_23
action_27 (58) = happyShift action_24
action_27 (60) = happyShift action_25
action_27 (6) = happyGoto action_79
action_27 (7) = happyGoto action_4
action_27 (8) = happyGoto action_5
action_27 (12) = happyGoto action_6
action_27 (14) = happyGoto action_7
action_27 (15) = happyGoto action_8
action_27 (16) = happyGoto action_9
action_27 (17) = happyGoto action_10
action_27 (22) = happyGoto action_11
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (46) = happyShift action_78
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (46) = happyShift action_77
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_6

action_31 (40) = happyShift action_76
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (40) = happyShift action_75
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (41) = happyShift action_74
action_33 (43) = happyShift action_27
action_33 (21) = happyGoto action_70
action_33 (25) = happyGoto action_71
action_33 (27) = happyGoto action_72
action_33 (30) = happyGoto action_73
action_33 _ = happyReduce_53

action_34 (38) = happyShift action_69
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_49

action_36 (34) = happyShift action_67
action_36 (41) = happyShift action_68
action_36 (43) = happyShift action_27
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (34) = happyShift action_66
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (34) = happyShift action_65
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (34) = happyShift action_64
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (34) = happyShift action_63
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (34) = happyShift action_62
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_55

action_43 (50) = happyShift action_61
action_43 (10) = happyGoto action_58
action_43 (18) = happyGoto action_59
action_43 (24) = happyGoto action_60
action_43 _ = happyReduce_48

action_44 _ = happyReduce_56

action_45 (33) = happyShift action_12
action_45 (37) = happyShift action_13
action_45 (39) = happyShift action_14
action_45 (42) = happyShift action_15
action_45 (46) = happyShift action_16
action_45 (47) = happyShift action_17
action_45 (48) = happyShift action_18
action_45 (49) = happyShift action_19
action_45 (50) = happyShift action_20
action_45 (51) = happyShift action_21
action_45 (54) = happyShift action_22
action_45 (55) = happyShift action_23
action_45 (58) = happyShift action_24
action_45 (60) = happyShift action_25
action_45 (6) = happyGoto action_57
action_45 (7) = happyGoto action_4
action_45 (8) = happyGoto action_5
action_45 (12) = happyGoto action_6
action_45 (14) = happyGoto action_7
action_45 (15) = happyGoto action_8
action_45 (16) = happyGoto action_9
action_45 (17) = happyGoto action_10
action_45 (22) = happyGoto action_11
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_3

action_47 (44) = happyShift action_48
action_47 (52) = happyShift action_49
action_47 (53) = happyShift action_50
action_47 (56) = happyShift action_51
action_47 (57) = happyShift action_52
action_47 (59) = happyShift action_53
action_47 (5) = happyGoto action_56
action_47 (9) = happyGoto action_45
action_47 _ = happyReduce_42

action_48 (54) = happyShift action_54
action_48 (55) = happyShift action_55
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_33

action_50 _ = happyReduce_34

action_51 _ = happyReduce_31

action_52 _ = happyReduce_32

action_53 _ = happyReduce_37

action_54 (44) = happyShift action_93
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (44) = happyShift action_92
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_57

action_57 _ = happyReduce_4

action_58 (41) = happyShift action_91
action_58 (26) = happyGoto action_87
action_58 (28) = happyGoto action_88
action_58 (29) = happyGoto action_89
action_58 (31) = happyGoto action_90
action_58 _ = happyReduce_61

action_59 (36) = happyShift action_86
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_47

action_61 (32) = happyShift action_85
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_44

action_63 _ = happyReduce_43

action_64 _ = happyReduce_45

action_65 _ = happyReduce_46

action_66 _ = happyReduce_9

action_67 _ = happyReduce_7

action_68 (33) = happyShift action_12
action_68 (37) = happyShift action_13
action_68 (39) = happyShift action_14
action_68 (42) = happyShift action_15
action_68 (46) = happyShift action_16
action_68 (47) = happyShift action_17
action_68 (48) = happyShift action_18
action_68 (49) = happyShift action_19
action_68 (50) = happyShift action_20
action_68 (51) = happyShift action_21
action_68 (54) = happyShift action_22
action_68 (55) = happyShift action_23
action_68 (58) = happyShift action_24
action_68 (60) = happyShift action_25
action_68 (4) = happyGoto action_33
action_68 (6) = happyGoto action_3
action_68 (7) = happyGoto action_4
action_68 (8) = happyGoto action_5
action_68 (12) = happyGoto action_6
action_68 (14) = happyGoto action_7
action_68 (15) = happyGoto action_8
action_68 (16) = happyGoto action_9
action_68 (17) = happyGoto action_10
action_68 (20) = happyGoto action_84
action_68 (22) = happyGoto action_11
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_8

action_70 _ = happyReduce_51

action_71 _ = happyReduce_65

action_72 _ = happyReduce_52

action_73 (41) = happyShift action_74
action_73 (25) = happyGoto action_83
action_73 _ = happyReduce_62

action_74 (33) = happyShift action_12
action_74 (37) = happyShift action_13
action_74 (39) = happyShift action_14
action_74 (42) = happyShift action_15
action_74 (46) = happyShift action_16
action_74 (47) = happyShift action_17
action_74 (48) = happyShift action_18
action_74 (49) = happyShift action_19
action_74 (50) = happyShift action_20
action_74 (51) = happyShift action_21
action_74 (54) = happyShift action_22
action_74 (55) = happyShift action_23
action_74 (58) = happyShift action_24
action_74 (60) = happyShift action_25
action_74 (4) = happyGoto action_82
action_74 (6) = happyGoto action_3
action_74 (7) = happyGoto action_4
action_74 (8) = happyGoto action_5
action_74 (12) = happyGoto action_6
action_74 (14) = happyGoto action_7
action_74 (15) = happyGoto action_8
action_74 (16) = happyGoto action_9
action_74 (17) = happyGoto action_10
action_74 (22) = happyGoto action_11
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_29

action_76 _ = happyReduce_28

action_77 (45) = happyShift action_81
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (42) = happyShift action_80
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_1

action_80 (46) = happyShift action_99
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (46) = happyShift action_97
action_81 (47) = happyShift action_98
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (43) = happyShift action_27
action_82 _ = happyReduce_59

action_83 _ = happyReduce_66

action_84 _ = happyReduce_40

action_85 (33) = happyShift action_12
action_85 (37) = happyShift action_13
action_85 (39) = happyShift action_14
action_85 (42) = happyShift action_15
action_85 (46) = happyShift action_16
action_85 (47) = happyShift action_17
action_85 (48) = happyShift action_18
action_85 (49) = happyShift action_19
action_85 (50) = happyShift action_20
action_85 (51) = happyShift action_21
action_85 (54) = happyShift action_22
action_85 (55) = happyShift action_23
action_85 (58) = happyShift action_24
action_85 (60) = happyShift action_25
action_85 (4) = happyGoto action_96
action_85 (6) = happyGoto action_3
action_85 (7) = happyGoto action_4
action_85 (8) = happyGoto action_5
action_85 (12) = happyGoto action_6
action_85 (14) = happyGoto action_7
action_85 (15) = happyGoto action_8
action_85 (16) = happyGoto action_9
action_85 (17) = happyGoto action_10
action_85 (22) = happyGoto action_11
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_10

action_87 _ = happyReduce_58

action_88 _ = happyReduce_67

action_89 _ = happyReduce_60

action_90 (41) = happyShift action_91
action_90 (28) = happyGoto action_95
action_90 _ = happyReduce_64

action_91 (50) = happyShift action_61
action_91 (10) = happyGoto action_94
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_36

action_93 _ = happyReduce_35

action_94 _ = happyReduce_63

action_95 _ = happyReduce_68

action_96 (43) = happyShift action_27
action_96 _ = happyReduce_38

action_97 _ = happyReduce_17

action_98 _ = happyReduce_18

action_99 _ = happyReduce_16

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Ratio happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (InfixCons happy_var_1 happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 ((happy_var_1,happy_var_2)
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn4
		 (mkValue happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  6 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Neg happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (List happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Tuple happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Rec happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (Con happy_var_1 []
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyTerminal ((IntLit,   (_,happy_var_1))))
	 =  HappyAbsSyn4
		 (Integer happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 (HappyTerminal ((FloatLit, (_,happy_var_1))))
	 =  HappyAbsSyn4
		 (Float happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 (HappyTerminal ((StringLit, (_,happy_var_1))))
	 =  HappyAbsSyn4
		 (String happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 (HappyTerminal ((CharLit,  (_,happy_var_1))))
	 =  HappyAbsSyn4
		 (Char happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 7 happyReduction_16
happyReduction_16 ((HappyTerminal ((IntLit,   (_,happy_var_5)))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((IntLit,   (_,happy_var_3)))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((IntLit,   (_,happy_var_1)))) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (mkDate happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 5 7 happyReduction_17
happyReduction_17 ((HappyTerminal ((IntLit,   (_,happy_var_5)))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((IntLit,   (_,happy_var_3)))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((IntLit,   (_,happy_var_1)))) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (mkTime happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 5 7 happyReduction_18
happyReduction_18 ((HappyTerminal ((FloatLit, (_,happy_var_5)))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((IntLit,   (_,happy_var_3)))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((IntLit,   (_,happy_var_1)))) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (mkTime happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  7 happyReduction_19
happyReduction_19 (HappyTerminal ((QQuote,   (_,happy_var_1))))
	 =  HappyAbsSyn4
		 (Quote happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  8 happyReduction_20
happyReduction_20 (HappyTerminal ((Conid,    (_,happy_var_1))))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  8 happyReduction_21
happyReduction_21 (HappyTerminal ((Qconid,   (_,happy_var_1))))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  8 happyReduction_22
happyReduction_22 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  8 happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  8 happyReduction_24
happyReduction_24 (HappyTerminal ((Varid,    (_,happy_var_1))))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  8 happyReduction_25
happyReduction_25 (HappyTerminal ((Qvarid,   (_,happy_var_1))))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  8 happyReduction_26
happyReduction_26 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  8 happyReduction_27
happyReduction_27 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  8 happyReduction_28
happyReduction_28 _
	(HappyTerminal ((Varid,    (_,happy_var_2))))
	_
	 =  HappyAbsSyn8
		 ("<" ++ happy_var_2 ++ ">"
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  8 happyReduction_29
happyReduction_29 _
	(HappyTerminal ((Conid,    (_,happy_var_2))))
	_
	 =  HappyAbsSyn8
		 ("<" ++ happy_var_2 ++ ">"
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  8 happyReduction_30
happyReduction_30 (HappyTerminal ((Reservedid, (_,happy_var_1))))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  9 happyReduction_31
happyReduction_31 (HappyTerminal ((Consym,   (_,happy_var_1))))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  9 happyReduction_32
happyReduction_32 (HappyTerminal ((Qconsym,  (_,happy_var_1))))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  9 happyReduction_33
happyReduction_33 (HappyTerminal ((Varsym,   (_,happy_var_1))))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  9 happyReduction_34
happyReduction_34 (HappyTerminal ((Qvarsym,  (_,happy_var_1))))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  9 happyReduction_35
happyReduction_35 _
	(HappyTerminal ((Conid,    (_,happy_var_2))))
	_
	 =  HappyAbsSyn8
		 (backtick happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  9 happyReduction_36
happyReduction_36 _
	(HappyTerminal ((Qconid,   (_,happy_var_2))))
	_
	 =  HappyAbsSyn8
		 (backtick happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  9 happyReduction_37
happyReduction_37 (HappyTerminal ((Reservedop, (_,happy_var_1))))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  10 happyReduction_38
happyReduction_38 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal ((Varid,    (_,happy_var_1))))
	 =  HappyAbsSyn10
		 ((happy_var_1,happy_var_3)
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  11 happyReduction_39
happyReduction_39  =  HappyAbsSyn11
		 ([]
	)

happyReduce_40 = happySpecReduce_3  11 happyReduction_40
happyReduction_40 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  12 happyReduction_41
happyReduction_41 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn12
		 (reverse happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  13 happyReduction_42
happyReduction_42 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn13
		 (reverse happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  14 happyReduction_43
happyReduction_43 _
	(HappyTerminal ((Consym,   (_,happy_var_2))))
	_
	 =  HappyAbsSyn14
		 ("(" ++ happy_var_2 ++ ")"
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  15 happyReduction_44
happyReduction_44 _
	(HappyTerminal ((Qconsym,  (_,happy_var_2))))
	_
	 =  HappyAbsSyn15
		 ("(" ++ happy_var_2 ++ ")"
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  16 happyReduction_45
happyReduction_45 _
	(HappyTerminal ((Qvarsym,  (_,happy_var_2))))
	_
	 =  HappyAbsSyn16
		 ("(" ++ happy_var_2 ++ ")"
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  17 happyReduction_46
happyReduction_46 _
	(HappyTerminal ((Varsym,   (_,happy_var_2))))
	_
	 =  HappyAbsSyn17
		 ("(" ++ happy_var_2 ++ ")"
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  18 happyReduction_47
happyReduction_47 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_0  18 happyReduction_48
happyReduction_48  =  HappyAbsSyn18
		 ([]
	)

happyReduce_49 = happySpecReduce_1  19 happyReduction_49
happyReduction_49 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_0  19 happyReduction_50
happyReduction_50  =  HappyAbsSyn19
		 ([]
	)

happyReduce_51 = happySpecReduce_2  20 happyReduction_51
happyReduction_51 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 : happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  21 happyReduction_52
happyReduction_52 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_0  21 happyReduction_53
happyReduction_53  =  HappyAbsSyn21
		 ([]
	)

happyReduce_54 = happySpecReduce_1  22 happyReduction_54
happyReduction_54 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  22 happyReduction_55
happyReduction_55 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_1
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  23 happyReduction_56
happyReduction_56 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  23 happyReduction_57
happyReduction_57 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_2 : happy_var_1
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  24 happyReduction_58
happyReduction_58 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 : happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  25 happyReduction_59
happyReduction_59 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  26 happyReduction_60
happyReduction_60 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_0  26 happyReduction_61
happyReduction_61  =  HappyAbsSyn26
		 ([]
	)

happyReduce_62 = happySpecReduce_1  27 happyReduction_62
happyReduction_62 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn27
		 (reverse happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  28 happyReduction_63
happyReduction_63 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  29 happyReduction_64
happyReduction_64 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn29
		 (reverse happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  30 happyReduction_65
happyReduction_65 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn30
		 ([happy_var_1]
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  30 happyReduction_66
happyReduction_66 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_2 : happy_var_1
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  31 happyReduction_67
happyReduction_67 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  31 happyReduction_68
happyReduction_68 (HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_2 : happy_var_1
	)
happyReduction_68 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 61 61 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	(Reservedop, (_,"=")) -> cont 32;
	(Special, (_,"(")) -> cont 33;
	(Special, (_,")")) -> cont 34;
	(Special, (_,"{")) -> cont 35;
	(Special, (_,"}")) -> cont 36;
	(Special, (_,"[")) -> cont 37;
	(Special, (_,"]")) -> cont 38;
	(Varsym, (_,"<")) -> cont 39;
	(Varsym, (_,">")) -> cont 40;
	(Special, (_,",")) -> cont 41;
	(Varsym,  (_,"-")) -> cont 42;
	(Varsym,  (_,"%")) -> cont 43;
	(Special, (_,"`")) -> cont 44;
	(Reservedop,  (_,":")) -> cont 45;
	(IntLit,   (_,happy_dollar_dollar)) -> cont 46;
	(FloatLit, (_,happy_dollar_dollar)) -> cont 47;
	(StringLit, (_,happy_dollar_dollar)) -> cont 48;
	(CharLit,  (_,happy_dollar_dollar)) -> cont 49;
	(Varid,    (_,happy_dollar_dollar)) -> cont 50;
	(Qvarid,   (_,happy_dollar_dollar)) -> cont 51;
	(Varsym,   (_,happy_dollar_dollar)) -> cont 52;
	(Qvarsym,  (_,happy_dollar_dollar)) -> cont 53;
	(Conid,    (_,happy_dollar_dollar)) -> cont 54;
	(Qconid,   (_,happy_dollar_dollar)) -> cont 55;
	(Consym,   (_,happy_dollar_dollar)) -> cont 56;
	(Qconsym,  (_,happy_dollar_dollar)) -> cont 57;
	(QQuote,   (_,happy_dollar_dollar)) -> cont 58;
	(Reservedop, (_,happy_dollar_dollar)) -> cont 59;
	(Reservedid, (_,happy_dollar_dollar)) -> cont 60;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 61 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Maybe a -> (a -> Maybe b) -> Maybe b
happyThen = ((>>=))
happyReturn :: () => a -> Maybe a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Maybe a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(PosToken)], [Prelude.String]) -> Maybe a
happyError' = (\(tokens, _) -> happyError tokens)
parseValue tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


backtick :: String -> String
backtick s = "`" ++ s ++ "`"

happyError :: [PosToken] -> Maybe a
happyError ((_,(p,_)) : _) = Nothing -- error ("Parser error at: " ++ show p)
happyError []              = Nothing -- error ("Parser error at EOF")

mkDate :: String -> String -> String -> Value
mkDate a b c = Date (intercalate "-" [a,b,c])

mkTime :: String -> String -> String -> Value
mkTime a b c = Time (intercalate ":" [a,b,c])



mkValue :: [Value] -> Value
mkValue [v]                 = v
mkValue (Con "" [] : vs)    = mkValue vs
mkValue (Con x as : vs)     = Con x (as ++ vs)
mkValue vs                  = mkFakeCon vs

mkFakeCon :: [Value] -> Value
mkFakeCon vs = Con "" (concatMap expand vs)
  where expand (Con "" vs) = vs
        expand v           = [v]
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
