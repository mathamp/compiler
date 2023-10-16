module Lexical where
    
    import Data.Maybe
    import Token
    import Lexical.Automaton
    import Lexical.Mgol
    
    type Line = Int
    type Column = Int
    type Position = (Line, Column)

    calculateOffset :: Position -> Token -> Position
    calculateOffset pos ([], _) = pos
    calculateOffset (x,y) ('\n':cs, t) = calculateOffset (1,y+1) (cs, t)
    calculateOffset (x,y) (c:cs, t)    = calculateOffset (x+1,y) (cs, t)
    
    readAndCollect :: String -> (String, String)
    readAndCollect w = runAndCollect ([], w) 0 mgolTransitions
        where
            runAndCollect (cs1, [])    _ _ = (reverse cs1, [])
            runAndCollect (cs1, c:cs2) s f = if isNothing res
                                             then (reverse cs1, c:cs2)
                                             else runAndCollect (c:cs1, cs2) (fromJust res) f
                where
                    res = f s c
    
    scanner :: String -> (Token, String)
    scanner [] = (("", Eof), "")
    scanner w  = ((lexeme, token), remaining)
        where
            split       = readAndCollect w
            lexeme      = fst split
            remaining   = snd split
            token       = mgolToken $ either id id $ evalDFA mgolDFA w

    parse :: String -> [(Position, Token)]
    parse s = updateKeywords $ posTokenList $ reverse $ generateTokens [] s
        where
            generateTokens ts s = case token of
                (_, Eof)    -> token:ts
                _           -> generateTokens (token:ts) remaining
                where
                    (token, remaining) = scanner s
            
            posTokenList [] = []
            posTokenList toks = posTokenList (1, 1) toks
                where
                    posTokenList pos []     = []
                    posTokenList pos (t:ts) = (pos, t) : posTokenList (calculateOffset pos t) ts
    
    updateKeywordInToken :: (Position, Token) -> (Position, Token)
    updateKeywordInToken (pos, (lex, cla)) 
        | lex == "inicio"       = (pos, (lex, Inicio))
        | lex == "varinicio"    = (pos, (lex, VarInicio))
        | lex == "varfim"       = (pos, (lex, VarFim))
        | lex == "escreva"      = (pos, (lex, Escreva))
        | lex == "leia"         = (pos, (lex, Leia))
        | lex == "se"           = (pos, (lex, Se))
        | lex == "entao"        = (pos, (lex, Entao))
        | lex == "fimse"        = (pos, (lex, FimSe))
        | lex == "repita"       = (pos, (lex, Repita))
        | lex == "fimrepita"    = (pos, (lex, FimRepita))
        | lex == "fim"          = (pos, (lex, Fim))
        | lex == "inteiro"      = (pos, (lex, Inteiro))
        | lex == "real"         = (pos, (lex, Real))
        | lex == "literal"      = (pos, (lex, Literal))
        | otherwise             = (pos, (lex, cla)) 

    updateKeywords :: [(Position, Token)] -> [(Position, Token)]
    updateKeywords = Prelude.map updateKeywordInToken