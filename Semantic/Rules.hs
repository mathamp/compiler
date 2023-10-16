module Semantic.Rules where
    import Syntatic.Mgol (Input, SynStack, lookupSlr)
    import Syntatic.Table (Action (..), gotoMap)
    import Syntatic.Grammar (Symbol(..), Deriv (..))
    import Token
    import Semantic.Mgol ( SemInfo(SemInfo, tipo), SemStack, SemSymbol(SSymbol) )
    
    semRule :: SemStack -> (Symbol, [Deriv]) -> (Either String String, SemStack)
    -- 1
    semRule (p:ss) (PP, [NonT P])                           = (Right "", SSymbol (PP, SemInfo Nothing Nothing) : ss)
    -- 2
    semRule (a:v:i:ss) (P, [Term Inicio, NonT V, NonT A])   = (Right "", SSymbol (P, SemInfo Nothing Nothing) : ss)
    -- 3
    semRule (lv:vi:ss) (V, [Term VarInicio, NonT LV])       = (Right "", SSymbol (V, SemInfo Nothing Nothing) : ss)
    -- 4
    semRule (lv:d:ss) (LV, [NonT D, NonT LV])               = (Right "", SSymbol (LV, SemInfo Nothing Nothing) : ss)
    -- 5
    semRule (pt:vf:ss) (LV, [Term VarFim, Term PtV])        = (Right "\n\n\n", SSymbol (LV, SemInfo Nothing Nothing) : ss)
    -- 6
    semRule (pt:l:t:ss) (D, [NonT TIPO, NonT L, Term PtV])  = (Right "", rule6 t l : ss)
        where
            rule6 :: SemSymbol -> SemSymbol -> SemSymbol
            rule6 (SSymbol(TIPO, SemInfo{tipo = x})) (SSymbol(L, info)) =
                SSymbol (L, info{tipo = x})
    -- -- 7
    -- semRule (l:v:i:ss) (L, [Term Id, Term Vir, NonT L])     = (Right "", SSymbol (L, SemInfo Nothing Nothing) : ss)
    --     where
    --         rule7 :: SemSymbol -> SemSymbol -> SemSymbol
    --         rule7 (SToken((lx, Id), SemInfo{})) (SSymbol(L, SemInfo{})) =

    -- 8
    -- semRule (i:ss) (L, [Term Id])                           = (Right "", SSymbol (L, SemInfo Nothing Nothing) : ss)

    -- rule7 :: SemSymbol -> SemSymbol -> SemSymbol
    -- rule7 (SToken((lx, Id), SemInfo{})) (SSymbol(L, SemInfo{tipo = x})) =
        -- SSymbol(Id)