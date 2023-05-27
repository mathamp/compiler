module Token where
    
    data ErrT = RealI | CienI | LitI | ComI | CharI | CharN | Unk
    
    data Class = Num | Lit | Id | Com | Eof | Opr | Rcb | Opm | AbP | FcP | PtV |
         Err ErrT | Vir | Ign | Inicio | VarInicio | VarFim | Escreva | Leia | Se | 
         Entao | FimSe | Repita | FimRepita | Fim
        
    type Lexeme = String
    data Type = Null | Integer | Real | Cientific

    type Token = (Lexeme, (Class, Type))

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
            Num -> "NUM"
            Lit -> "LIT"
            Id ->  "ID "
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

            Inicio -> "inicio"
            VarInicio -> "varinicio"
            VarFim -> "varfim"
            Escreva -> "escreva"
            Leia -> "leia"
            Se -> "se"
            Entao -> "entao"
            FimSe -> "fimse"
            Repita -> "repita"
            FimRepita -> "fimrepita"
            Fim -> "fim"
    
    instance Show Type where
        show t = case t of
            Null -> "nulo"
            Integer -> "inteiro"
            Real -> "real"
            Cientific -> "cientifico"

    token2str :: String -> String -> String -> String
    token2str c l t = 
        "Classe: " ++ show c ++ ", " ++
        "Lexema: " ++ show l ++ ", " ++
        "Tipo: "   ++ show t

    showToken :: Token -> String
    showToken (l, (c, t))= case (c, l, t) of
            (Inicio, _, _)      -> token2str "inicio" "inicio" "inicio"
            (VarInicio, _, _)   -> token2str "varinicio" "varinicio" "varinicio"
            (VarFim, _, _)      -> token2str "varinicio" "varinicio" "varinicio"
            (Escreva, _, _)     -> token2str "escreva" "escreva" "escreva"
            (Leia, _, _)        -> token2str "leia" "leia" "leia"
            (Se, _, _)          -> token2str "se" "se" "se"
            (Entao, _, _)       -> token2str "entao" "entao" "entao"
            (FimSe, _, _)       -> token2str "fimse" "fimse" "fimse"
            (Repita, _, _)      -> token2str "repita" "repita" "repita"
            (FimRepita, _, _)   -> token2str "fimrepita" "fimrepita" "fimrepita"
            (Fim, _, _)         -> token2str "fim" "fim" "fim"

            (Err e, _, _)    -> "Erro lexico - " ++ show e

            (Lit, l, t) -> token2str (show Lit) l "literal"
            (c,   l, t) -> token2str (show c) l   (show t)