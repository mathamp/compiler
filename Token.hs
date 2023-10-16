module Token where
    
    data ErrT = RealI | CienI | LitI | ComI | CharI | CharN | Unk
        deriving (Eq)
    
    data NumT = Null | Integer | Real2 | Cientific
        deriving (Eq)
    
    data Class = Num NumT | Id | Com | Eof | Opr | Rcb | Opm | AbP | FcP | PtV |
         Err ErrT | Vir | Ign | Inicio | VarInicio | VarFim | Escreva | Leia | Se | 
         Entao | FimSe | Repita | FimRepita | Fim | Inteiro | Real | Literal
         deriving (Eq)
        
    type Lexeme = String
    -- data Type = Null | Integer | Real | Cientific

    type Token = (Lexeme, Class)

    instance Show ErrT where
        show RealI = "Número real incompleto"
        show CienI = "Número científico incompleto"
        show LitI  = "Literal incompleto"
        show ComI  = "Comentário incompleto"
        show CharI = "Caractere Inválido"
        show CharN = "Caractere Inesperado"
        show Unk   = "Erro desconhecido"

    instance Show Class where
        show c = case c of
            Num _ -> "NUM"
            Inteiro -> "INT"
            Real -> "REA"
            Literal -> "LIT"
            Id  ->  "ID "
            Com -> "COM"
            Eof -> "EOF"
            Opr -> "OPR"
            Rcb -> "ATR"
            Opm -> "OPM"
            AbP -> "ABP"
            FcP -> "FCP"
            PtV -> "PTV"
            (Err e) -> "ERR - " ++ show e
            Vir -> "VIR"
            Ign -> "IGN"

            Inicio -> "Inicio"
            VarInicio -> "VarInicio"
            VarFim -> "VarFim"
            Escreva -> "Escreva"
            Leia -> "Leia"
            Se -> "Se"
            Entao -> "Entao"
            FimSe -> "Fimse"
            Repita -> "Repita"
            FimRepita -> "FimRepita"
            Fim -> "Fim"

    token2str :: String -> String -> String -> String
    token2str c l t = 
        "Classe: " ++ show c ++ ", " ++
        "Lexema: " ++ show l ++ ", " ++
        "Tipo: "   ++ show t

    showToken :: Token -> String
    showToken (l, c)= case (c, l) of
            (Inicio, _)      -> token2str "inicio" "inicio" "inicio"
            (VarInicio, _)   -> token2str "varinicio" "varinicio" "varinicio"
            (VarFim, _)      -> token2str "varinicio" "varinicio" "varinicio"
            (Escreva, _)     -> token2str "escreva" "escreva" "escreva"
            (Leia, _)        -> token2str "leia" "leia" "leia"
            (Se, _)          -> token2str "se" "se" "se"
            (Entao, _)       -> token2str "entao" "entao" "entao"
            (FimSe, _)       -> token2str "fimse" "fimse" "fimse"
            (Repita, _)      -> token2str "repita" "repita" "repita"
            (FimRepita, _)   -> token2str "fimrepita" "fimrepita" "fimrepita"
            (Fim, _)         -> token2str "fim" "fim" "fim"

            (Err e, _)    -> "Erro lexico - " ++ show e

            (Literal, l) -> token2str (show Literal) l "literal"
            (c, l) -> token2str (show c) l "null"
    
    isErr :: Token -> Bool
    isErr (_, Err e) = True
    isErr _1         = False

    isIgn :: Token -> Bool
    isIgn (_, Ign) = True
    isIgn _       = False