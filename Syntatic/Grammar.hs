module Syntatic.Grammar where
    import Token
    
    data Symbol = PP | P | V | A | LV | D | TIPO | L | ES | ARG | 
        CMD | LD | OPRD | COND | CAB | CP | EXP_R | R | CABR | CPR
        deriving (Eq, Show)
    
    data Deriv = NonT Symbol | Term Class
    
    instance Show Deriv where
        show c = case c of
            NonT s -> show s
            Term c -> show c

    pp      =   (PP,    [NonT P])
    p       =   (P,     [Term Inicio, NonT V, NonT A])
    v       =   (V,     [Term VarInicio, NonT LV])
    lv1     =   (LV,    [NonT D, NonT LV])
    lv2     =   (LV,    [Term VarFim, Term PtV])
    d       =   (D,     [NonT TIPO, NonT L, Term PtV])
    l1      =   (L,     [Term Id, Term Vir, NonT L])
    l2      =   (L,     [Term Id])
    tipo1   =   (TIPO,  [Term Inteiro])
    tipo2   =   (TIPO,  [Term Real])
    tipo3   =   (TIPO,  [Term Literal])
    a1      =   (A,     [NonT ES, NonT A])
    a2      =   (A,     [NonT CMD, NonT A])
    a3      =   (A,     [NonT COND, NonT A])
    a4      =   (A,     [NonT R, NonT A])
    a5      =   (A,     [Term Fim])
    r       =   (R,     [NonT CABR, NonT CPR])
    es1     =   (ES,    [Term Leia, Term Id, Term PtV])
    es2     =   (ES,    [Term Escreva, NonT ARG, Term PtV])
    arg1    =   (ARG,   [Term Literal])
    arg2    =   (ARG,   [Term (Num Null)])
    arg3    =   (ARG,   [Term Id])
    cmd     =   (CMD,   [Term Id, Term Rcb, NonT LD, Term PtV])
    ld1     =   (LD,    [NonT OPRD, Term Opm, NonT OPRD])
    ld2     =   (LD,    [NonT OPRD])
    oprd1   =   (OPRD,  [Term Id])
    oprd2   =   (OPRD,  [Term (Num Null)])
    cond    =   (COND,  [NonT CAB, NonT CP])
    cab     =   (CAB,   [Term Se, Term AbP, NonT EXP_R, Term FcP, Term Entao])
    expR    =   (EXP_R, [NonT OPRD, Term Opr, NonT OPRD])
    cp1     =   (CP,    [NonT ES, NonT CP])
    cp2     =   (CP,    [NonT CMD, NonT CP])
    cp3     =   (CP,    [NonT COND, NonT CP])
    cp4     =   (CP,    [Term FimSe])
    cabr    =   (CABR,  [Term Repita, Term AbP, NonT EXP_R, Term FcP])
    cpr1    =   (CPR,   [NonT ES, NonT CPR])
    cpr2    =   (CPR,   [NonT CMD, NonT CPR])
    cpr3    =   (CPR,   [NonT COND, NonT CPR])
    cpr4    =   (CPR,   [Term FimRepita])

    
    first :: Symbol -> [Class]
    first PP    = [Inicio]
    first P     = [Inicio]
    first V     = [VarInicio]
    first LV    = [Inteiro, Real, Literal, VarFim]
    first D     = [Inteiro, Real, Literal]
    first L     = [Id]
    first TIPO  = [Inteiro, Real, Literal]
    first A     = [Leia, Escreva, Id, Se, Repita, Fim]
    first ES    = [Leia, Escreva]
    first ARG   = [Literal, Num Null, Id]
    first CMD   = [Id]
    first LD    = [Id, Num Null]
    first OPRD  = [Id, Num Null]
    first COND  = [Se]
    first CAB   = [Se]
    first EXP_R = [Id, Num Null]
    first CP    = [Leia, Escreva, Id, Se, FimSe]
    first R     = [Repita]
    first CABR  = [Repita]
    first CPR   = [Leia, Escreva, Id, Se, FimRepita]

    follow :: Symbol -> [Class]
    follow PP    = [Eof]
    follow P     = [Eof]
    follow V     = [Leia, Escreva, Id, Se, Repita, Fim]
    follow LV    = [Leia, Escreva, Id, Se, Repita, Fim]
    follow D     = [Inteiro, Real, Literal, VarFim]
    follow L     = [PtV]
    follow TIPO  = [Id]
    follow A     = [Eof]
    follow ES    = [Leia, Escreva, Id, Se, FimSe, Repita, FimRepita, Fim]
    follow ARG   = [PtV]
    follow CMD   = [Leia, Escreva, Id, Se, FimSe, Repita, FimRepita, Fim]
    follow LD    = [PtV]
    follow OPRD  = [Opm, PtV, Opr, FcP]
    follow COND  = [Leia, Escreva, Id, Se, FimSe, Repita, FimRepita, Fim]
    follow CAB   = [Leia, Escreva, Id, Se, FimSe]
    follow EXP_R = [FcP]
    follow CP    = [Leia, Escreva, Id, Se, FimSe, Repita, FimRepita, Fim]
    follow R     = [Leia, Escreva, Id, Se, Repita, Fim]
    follow CABR  = [Leia, Escreva, Id, Se, FimRepita]
    follow CPR   = [Leia, Escreva, Id, Se, Repita, Fim]

    