{-# LANGUAGE TupleSections #-}

module Syntatic.Table where
    import Data.Maybe (fromJust, isJust, fromMaybe)

    import Syntatic.Automaton (State (..))
    import Token (Class (..), NumT (Null))
    import Syntatic.Grammar (Symbol (..), Deriv, follow, lv1, lv2, v, d, l2, l1, tipo1, tipo2, tipo3, p, a5, es1, a1, es2, arg1, arg2, arg3, a2, cmd, ld1, oprd1, oprd2, a3, cond, cp4, cab, expR, cp1, a4, r, cpr4, cabr, cpr1, cp2, cp3, cpr2, cpr3, ld2)

    data Action = Shift State | Reduce (Symbol, [Deriv]) | Accept | Error
        deriving (Show)

    actionTable :: [(State, [(Class, Action)])]
    actionTable = [
            (S 0, [
                (Inicio, Shift $ S 2)
            ]),
            (S 1, [
                (Eof, Accept)
            ]),
            (S 2, [
                (VarInicio, Shift $ S 6)
            ]),
            (S 3, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Id, Shift $ S_CMD 3),
                (Se, Shift $ S_COND 6),
                (Repita, Shift $ S_R 6),
                (Fim, Shift $ S 5)
            ]),
            (S 4, map (, Reduce p) (follow P)),
            (S 5, map (, Reduce a5) (follow A)),
            (S 6, [
                (VarFim, Shift $ S 61),
                (Inteiro, Shift $ S 17),
                (Real, Shift $ S 18),
                (Literal, Shift $ S 19)
            ]),
            (S 61, [
                (PtV, Shift $ S 62)
            ]),
            (S 62, map (, Reduce lv2) (follow LV)),
            (S 7, map (, Reduce v) (follow V)),
            (S 8, [
                (VarFim, Shift $ S 61),
                (Inteiro, Shift $ S 17),
                (Real, Shift $ S 18),
                (Literal, Shift $ S 19)
            ]),
            (S 9, map (, Reduce lv1) (follow LV)),
            (S 10, [
                (Id, Shift $ S 13)
            ]),
            (S 11, [
                (PtV, Shift $ S 12)
            ]),
            (S 12, map (, Reduce d) (follow D)),
            (S 13, map (, Reduce l2) (follow L) ++ [
                (Vir, Shift $ S 14)
            ]),
            (S 14, [
                (Id, Shift $ S 16)
            ]),
            (S 15, map (, Reduce l1) (follow L)),
            (S 16, map (, Reduce l2) (follow L) ++ [
                (Vir, Shift $ S 14)
            ]),
            (S 17, map (, Reduce tipo1) (follow TIPO)),
            (S 18, map (, Reduce tipo2) (follow TIPO)),
            (S 19, map (, Reduce tipo3) (follow TIPO)),

            (S_ES 1, [
                (Id, Shift $ S_CMD 3),
                (Se, Shift $ S_COND 6),
                (Repita, Shift $ S_R 6),
                (Fim, Shift $ S 5),
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6)
            ]),
            (S_ES 2, map (, Reduce a1) (follow A)),
            (S_ES 3, [
                (Id, Shift $ S_ES 4)
            ]),
            (S_ES 4, [
                (PtV, Shift $ S_ES 5)
            ]),
            (S_ES 5, map (, Reduce es1) (follow ES)),
            (S_ES 6, [
                (Literal, Shift $ S_ES 9),
                (Num Null, Shift $ S_ES 10),
                (Id, Shift $ S_ES 11)
            ]),
            (S_ES 7, [
                (PtV, Shift $ S_ES 8)
            ]),
            (S_ES 8, map (, Reduce es2) (follow ES)),
            (S_ES 9, map (, Reduce arg1) (follow ARG)),
            (S_ES 10, map (, Reduce arg2) (follow ARG)),
            (S_ES 11, map (, Reduce arg3) (follow ARG)),

            (S_CMD 1, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Se, Shift $ S_COND 6),
                (Repita, Shift $ S_R 6),
                (Fim, Shift $ S 5),
                (Id, Shift $ S_CMD 3)
            ]),
            (S_CMD 2, map (, Reduce a2) (follow A)),
            (S_CMD 3, [
                (Rcb, Shift $ S_CMD 4)
            ]),
            (S_CMD 4, [
                (Id, Shift $ S_CMD 10),
                (Num Null, Shift $ S_CMD 11)
            ]),
            (S_CMD 5, [
                (PtV, Shift $ S_CMD 6)
            ]),
            (S_CMD 6, map (, Reduce cmd) (follow CMD)),
            (S_CMD 7, map (, Reduce ld2) (follow LD) ++ [
                (Opm, Shift $ S_CMD 8)
            ]),
            (S_CMD 8, [
                (Id, Shift $ S_CMD 10),
                (Num Null, Shift $ S_CMD 11)
            ]),
            (S_CMD 9, map (, Reduce ld1) (follow LD)),
            (S_CMD 10, map (, Reduce oprd1) (follow OPRD)),
            (S_CMD 11, map (, Reduce oprd2) (follow OPRD)),

            (S_COND 1, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Se, Shift $ S_COND 6),
                (Repita, Shift $ S_R 6),
                (Fim, Shift $ S 5),
                (Id, Shift $ S_CMD 3)
            ]),
            (S_COND 2, map (, Reduce a3) (follow A)),
            (S_COND 3, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Id, Shift $ S_CMD 3),
                (FimSe, Shift $ S_COND 5),
                (Se, Shift $ S_COND 6)
            ]),
            (S_COND 4, map (, Reduce cond) (follow COND)),
            (S_COND 5, map (, Reduce cp4) (follow CP)),
            (S_COND 6, [
                (AbP, Shift $ S_COND 7)
            ]),
            (S_COND 7, [
                (Id, Shift $ S_COND 14),
                (Num Null, Shift $ S_COND 15)
            ]),
            (S_COND 8, [
                (FcP, Shift $ S_COND 9)
            ]),
            (S_COND 9, [
                (Entao, Shift $ S_COND 10)
            ]),
            (S_COND 10, map (, Reduce cab) (follow CAB)),
            (S_COND 11, [
                (Opr, Shift $ S_COND 12)
            ]),
            (S_COND 12, [
                (Id, Shift $ S_COND 14),
                (Num Null, Shift $ S_COND 15)
            ]),
            (S_COND 13, map (, Reduce expR) (follow EXP_R)),
            (S_COND 14, map (, Reduce oprd1) (follow OPRD)),
            (S_COND 15, map (, Reduce oprd2) (follow OPRD)),

            (S_COND_ES 1, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Id, Shift $ S_CMD 3),
                (FimSe, Shift $ S_COND 5)
            ]),
            (S_COND_ES 2, map (, Reduce cp1) (follow CP)),
            (S_COND_CMD 1, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Id, Shift $ S_CMD 3),
                (FimSe, Shift $ S_COND 5)
            ]),
            (S_COND_CMD 2, map (, Reduce cp2) (follow CP)),
            (S_COND_COND 1, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Id, Shift $ S_CMD 3),
                (FimSe, Shift $ S_COND 5)
            ]),
            (S_COND_COND 2, map (, Reduce cp3) (follow CP)),

            (S_R 1, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Id, Shift $ S_CMD 3),
                (Se, Shift $ S_COND 6),
                (Fim, Shift $ S 5)
            ]),
            (S_R 2, map (, Reduce a4) (follow A)),
            (S_R 3, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Id, Shift $ S_CMD 3),
                (FimRepita, Shift $ S_R 5)
            ]),
            (S_R 4, map (, Reduce r) (follow R)),
            (S_R 5, map (, Reduce cpr4) (follow CPR)),
            (S_R 6, [
                (AbP, Shift $ S_R 7)
            ]),
            (S_R 7, [
                (Id, Shift $ S_R 13),
                (Num Null, Shift $ S_R 14)
            ]),
            (S_R 8, [
                (FcP, Shift $ S_R 9)
            ]),
            (S_R 9, map (, Reduce cabr) (follow CABR)),
            (S_R 10, [
                (Opr, Shift $ S_R 11)
            ]),
            (S_R 11, [
                (Id, Shift $ S_R 13),
                (Num Null, Shift $ S_R 14)
            ]),
            (S_R 12, map (, Reduce expR) (follow EXP_R)),
            (S_R 13, map (, Reduce oprd1) (follow OPRD)),
            (S_R 14, map (, Reduce oprd2) (follow OPRD)),

            (S_R_ES 1, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Id, Shift $ S_CMD 3),
                (FimRepita, Shift $ S_R 5)
            ]),
            (S_R_ES 2, map (, Reduce cpr1) (follow CPR)),
            (S_R_CMD 1, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Id, Shift $ S_CMD 3),
                (FimRepita, Shift $ S_R 5)
            ]),
            (S_R_CMD 2, map (, Reduce cpr2) (follow CPR)),
            (S_R_COND 1, [
                (Leia, Shift $ S_ES 3),
                (Escreva, Shift $ S_ES 6),
                (Id, Shift $ S_CMD 3),
                (FimRepita, Shift $ S_R 5)
            ]),
            (S_R_COND 2, map (, Reduce cpr3) (follow CPR))
        ]

    findActionLine :: State -> [(State, [(Class, Action)])] -> [(Class, Action)]
    findActionLine s tab = fromJust $ lookup s tab

    findAction :: Class -> [(Class, Action)] -> Action
    findAction c xs =
        fromMaybe Error action
            where
                action = lookup c xs

    actionMap :: State -> Class -> Action
    actionMap s a = findAction a $ findActionLine s actionTable



    gotoTable :: [(State, [(Symbol, State)])]
    gotoTable = [
            (S 0, [
                (P, S 1)
            ]),
            (S 1, []),
            (S 2, [
                (V, S 3)
            ]),
            (S 3, [
                (A, S 4),
                (CAB, S_COND 3),
                (CABR, S_R 3),
                (ES, S_ES 1),
                (CMD, S_CMD 1),
                (COND, S_COND 1),
                (R, S_R 1)
            ]),
            (S 4, []),
            (S 5, []),
            (S 6, [
                (LV, S 7),
                (D, S 8),
                (TIPO, S 10)
            ]),
            (S 61, []),
            (S 62, []),
            (S 7, []),
            (S 8, [
                (LV, S 9),
                (D, S 8),
                (TIPO, S 10)
            ]),
            (S 9, []),
            (S 10, [
                (L, S 11)
            ]),
            (S 11, []),
            (S 12, []),
            (S 13, []),
            (S 14, [
                (L, S 15)
            ]),
            (S 15, []),
            (S 16, []),
            (S 17, []),
            (S 18, []),
            (S 19, []),

            (S_ES 1, [
                (ES, S_ES 1),
                (CMD, S_CMD 1),
                (COND, S_COND 1),
                (CAB, S_COND 3),
                (R, S_R 1),
                (CABR, S_R 3),
                (A, S_ES 2)
            ]),
            (S_ES 2, []),
            (S_ES 3, []),
            (S_ES 4, []),
            (S_ES 5, []),
            (S_ES 6, [
                (ARG, S_ES 7)
            ]),
            (S_ES 7, []),
            (S_ES 8, []),
            (S_ES 9, []),
            (S_ES 10, []),
            (S_ES 11, []),

            (S_CMD 1, [
                (ES, S_ES 1),
                (CMD, S_CMD 1),
                (COND, S_COND 1),
                (CAB, S_COND 3),
                (R, S_R 1),
                (CABR, S_R 3),
                (A, S_CMD 2)
            ]),
            (S_CMD 2, []),
            (S_CMD 3, []),
            (S_CMD 4, [
                (LD, S_CMD 5),
                (OPRD, S_CMD 7)
            ]),
            (S_CMD 5, []),
            (S_CMD 6, []),
            (S_CMD 7, []),
            (S_CMD 8, [
                (OPRD, S_CMD 9)
            ]),
            (S_CMD 9, []),
            (S_CMD 10, []),
            (S_CMD 11, []),

            (S_COND 1, [
                (ES, S_ES 1),
                (CMD, S_CMD 1),
                (COND, S_COND 1),
                (R, S_R 1),
                (CABR, S_R 3),
                (A, S_COND 2),
                (CAB, S_COND 3)
            ]),
            (S_COND 2, []),
            (S_COND 3, [
                (ES, S_COND_ES 1),
                (CMD, S_COND_CMD 1),
                (COND, S_COND_COND 1),
                (CAB, S_COND 3),
                (CP, S_COND 4)
            ]),
            (S_COND 4, []),
            (S_COND 5, []),
            (S_COND 6, []),
            (S_COND 7, [
                (OPRD, S_COND 11),
                (EXP_R, S_COND 8)
            ]),
            (S_COND 8, []),
            (S_COND 9, []),
            (S_COND 10, []),
            (S_COND 11, []),
            (S_COND 12, [
                (OPRD, S_COND 13)
            ]),
            (S_COND 13, []),
            (S_COND 14, []),
            (S_COND 15, []),

            (S_COND_ES 1, [
                (ES, S_COND_ES 1),
                (CMD, S_COND_CMD 1),
                (COND, S_COND_COND 1),
                (CAB, S_COND 3),
                (CP, S_COND_ES 2)
            ]),
            (S_COND_ES 2, []),
            (S_COND_CMD 1, [
                (ES, S_COND_ES 1),
                (CMD, S_COND_CMD 1),
                (COND, S_COND_COND 1),
                (CAB, S_COND 3),
                (CP, S_COND_CMD 2)
            ]),
            (S_COND_CMD 2, []),
            (S_COND_COND 1, [
                (ES, S_COND_ES 1),
                (CMD, S_COND_CMD 1),
                (COND, S_COND_COND 1),
                (CAB, S_COND 3),
                (CP, S_COND_COND 2)
            ]),
            (S_COND_COND 2, []),

            (S_R 1, [
                (ES, S_ES 1),
                (CMD, S_CMD 1),
                (COND, S_COND 1),
                (CAB, S_COND 3),
                (R, S_R 1),
                (CABR, S_R 3),
                (A, S_R 2)
            ]),
            (S_R 2, []),
            (S_R 3, [
                (ES, S_R_ES 1),
                (CMD, S_R_CMD 1),
                (COND, S_R_COND 1),
                (CAB, S_COND 3),
                (CPR, S_R 4)
            ]),
            (S_R 4, []),
            (S_R 5, []),
            (S_R 6, []),
            (S_R 7, [
                (OPRD, S_R 10),
                (EXP_R, S_R 8)
            ]),
            (S_R 8, []),
            (S_R 9, []),
            (S_R 10, []),
            (S_R 11, [
                (OPRD, S_R 12)
            ]),
            (S_R 12, []),
            (S_R 13, []),
            (S_R 14, []),

            (S_R_ES 1, [
                (ES, S_R_ES 1),
                (CMD, S_R_CMD 1),
                (COND, S_R_COND 1),
                (CAB, S_COND 3),
                (CPR, S_R_ES 2)
            ]),
            (S_R_ES 2, []),
            (S_R_CMD 1, [
                (ES, S_R_ES 1),
                (CMD, S_R_CMD 1),
                (COND, S_R_COND 1),
                (CAB, S_COND 3),
                (CPR, S_R_CMD 2)
            ]),
            (S_R_CMD 2, []),
            (S_R_COND 1, [
                (ES, S_R_ES 1),
                (CMD, S_R_CMD 1),
                (COND, S_R_COND 1),
                (CAB, S_COND 3),
                (CPR, S_R_COND 2)
            ]),
            (S_R_COND 2, [])
        ]

    findGotoLine :: State -> [(State, [(Symbol, State)])] -> [(Symbol, State)]
    findGotoLine s tab = fromJust $ lookup s tab

    findGoto :: Symbol -> [(Symbol, State)] -> State
    findGoto c xs = fromJust $ lookup c xs

    gotoMap :: State -> Symbol -> State
    gotoMap s a = findGoto a $ findGotoLine s gotoTable