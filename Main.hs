module Main where

    import System.Environment
    import Lexical
    import SymbolTable
    import Token
    import qualified Data.Map as Map
    import Data.Maybe
    import Syntatic.Table (Action(..))
    import Syntatic.Mgol ( stepSlr, Input, SynStack )
    import Syntatic.Automaton (State(..))
    import Syntatic.Grammar (Symbol (..), Deriv (..))
    import Semantic.Mgol (SemStack, SemSymbol (..), SemInfo (..))

    codeHeader =
        "#include <stdio.h>\n" ++
        "typedef char literal[256];\n" ++
        "void main(void) {\n"

    codeFooter = "}\n";


    getInitialSymbolTable =
        Map.fromList [
            ("inicio", Inicio),
            ("varinicio", VarInicio),
            ("varfim", VarFim),
            ("escreva", Escreva),
            ("leia", Leia),
            ("se", Se),
            ("entao", Entao),
            ("fimse", FimSe),
            ("repita", Repita),
            ("fimrepita", FimRepita),
            ("fim", Fim)
        ]

    printPosToken :: (Position, Token) -> IO ()
    printPosToken ( _    , (_, Ign)) = return ()
    printPosToken ((x, y), tok)           = putStrLn $ "Linha: " ++ show y ++
        "\tColuna: " ++ show x ++ "\tToken: " ++ showToken tok

    printTokenList :: [(Position, Token)] -> IO ()
    printTokenList [] = return ()
    printTokenList (x:xs) = do
        printPosToken x
        printTokenList xs

    fromPosTokenList :: [(Position, Token)] -> SymbolTable -> SymbolTable
    fromPosTokenList [] st = st
    fromPosTokenList xs st =
        Map.union st $ Map.fromList [snd x | x <- xs, (snd .snd) x == Id]

    runParser :: Input -> SynStack -> [Action] -> SemStack -> Int -> Maybe String -> IO (Maybe String)
    runParser ts ss as semS count code = do
        let (lin, col) = fst $ head ts
        let tok = snd $ head ts

        -- putStrLn $ fromJust code

        if isErr tok
        then do
            let (_, Err t) = tok
            print $ "Erro lexico em ("++ show col ++ ", " ++ show lin ++ "): " ++ show t
            runParser (tail ts) ss as semS 0 Nothing
        else do
            case stepSlr ts ss of
                (ts2, ss2, Shift t) -> do
                    -- print $ "Consumiu o token: " ++ show (head ts)
                    runParser ts2 ss2 (Shift t :as) (SToken (tok, SemInfo Nothing Nothing) : semS) count code
                (ts2, ss2, Reduce (aa, bb)) -> do
                    -- print $ "Reduziu: " ++ show aa ++ " -> " ++ show bb
                    -- print $ "Stack: " ++ show semS
                    -- print $ "Reduce: " ++ show (aa, bb)
                    (result, stack, count) <- semRule semS (aa, bb) count
                    case result of
                        Left str -> do
                            print $ "Erro semântico em (" ++ show col ++ ", " ++ show lin ++ "): " ++
                                str
                            runParser ts2 ss2 (Reduce (aa, bb) :as) stack count Nothing
                        Right str -> runParser ts2 ss2 (Reduce (aa, bb) :as) stack count ((++) <$> code <*> Just str)
                (ts2, ss2, Accept) -> do
                    print "Aceitou"
                    return code
                (ts2, ss2, Error) -> do
                    print $ "Erro Sintatico em (" ++ show col ++ ", " ++ show lin ++ ")"
                    runParser (tail ts2) ss2 as semS count Nothing

    main = do
        args <- getArgs

        case args of
            [] -> do
                putStrLn "No file supplied as argument"
            [arg] -> do
                contents <- readFile arg
                let tokenList = parse contents
                let symTab = fromPosTokenList (filter (\x -> (snd . snd) x == Id) tokenList) getInitialSymbolTable

                let tokenList2 = filter (\x -> (snd . snd) x /= Ign) tokenList

                result <- runParser tokenList2 [S 0] [] [] 0 $ Just codeHeader

                case result of
                    Nothing -> print "O código contém falhas"
                    Just s  -> do
                        let code = s ++ codeFooter
                        let out = take (length arg - 5) arg ++ ".c"
                        writeFile out code

            _ -> do
                putStrLn "Too many arguments"



    semRule :: SemStack -> (Symbol, [Deriv]) -> Int -> IO (Either String String, SemStack, Int)
    -- 1
    semRule (p:ss) (PP, [NonT P]) count = do
        return (Right "", SSymbol (PP, SemInfo Nothing Nothing) : ss, count)
    -- 2
    semRule (a:v:i:ss) (P, [Term Inicio, NonT V, NonT A]) count = do
        return (Right "", SSymbol (P, SemInfo Nothing Nothing) : ss, count)
    -- 3
    semRule (lv:vi:ss) (V, [Term VarInicio, NonT LV]) count = do
        return (Right "", SSymbol (V, SemInfo Nothing Nothing) : ss, count)
    -- 4
    semRule (lv:d:ss) (LV, [NonT D, NonT LV]) count = do
        return (Right "", SSymbol (LV, SemInfo Nothing Nothing) : ss, count)
    -- 5
    semRule (pt:vf:ss) (LV, [Term VarFim, Term PtV]) count = do
        return (Right "\n\n\n", SSymbol (LV, SemInfo Nothing Nothing) : ss, count)
    -- 6
    semRule (pt:l:t:ss) (D, [NonT TIPO, NonT L, Term PtV]) count = do
        return (Right ";\n", SSymbol (D, SemInfo Nothing Nothing) : ss, count)
    -- 7
    semRule (l:vir:id:ss) (L, [Term Id, Term Vir, NonT L]) count = do
        return (Right "", SSymbol (L, SemInfo Nothing Nothing) : ss, count)
    -- 8
    semRule (SToken ((lx, cl), _):ss) (L, [Term Id]) count = do
        let first = head ss
        let second = ss !! 2

        case (first, second) of
            (SSymbol (TIPO, SemInfo{tipo=x}), _) -> do
                return (Right lx, SSymbol (L, SemInfo{tipo=x, lexema=Just lx}) : ss, count)
            (_, SSymbol (TIPO, SemInfo{tipo=x})) -> do
                let neighbour = ss !! 1
                case neighbour of
                    SToken((name, cl), info) -> do
                        return (Right $ name ++ ", " ++ lx, SSymbol (L, SemInfo{tipo=x, lexema= Just $ name ++ ", " ++ lx}) : ss, count)
                    _ -> error "Algo de errado nn está certo"

    -- 9
    semRule (SToken((lx, Inteiro), info):ss) (TIPO, [Term Inteiro]) count = do
        return (Right "int ", SSymbol (TIPO, SemInfo{tipo=Just "int", lexema=Nothing}):ss, count)
    -- 10
    semRule (SToken((lx, Real), info):ss) (TIPO, [Term Real]) count = do
        return (Right "float ", SSymbol (TIPO, SemInfo{tipo=Just "float", lexema=Nothing}):ss, count)
    -- 11
    semRule (SToken((lx, Literal), info):ss) (TIPO, [Term Literal]) count = do
        return (Right "literal ", SSymbol (TIPO, SemInfo{tipo=Just "literal", lexema=Nothing}):ss, count)
    -- 12
    semRule (a:es:ss) (A, [NonT ES, NonT A]) count = do
        return (Right "", SSymbol (A, SemInfo Nothing Nothing):ss, count)
    -- 13
    semRule (pt:SToken((lex, cl), info):leia:ss) (ES, [Term Leia, Term Id, Term PtV]) count = do
        return (Right $ "scanf(\"%d\", &" ++ lex ++ ");\n", SSymbol (ES, SemInfo Nothing Nothing):ss, count)
    -- 14
    semRule (pt:SSymbol(ARG, SemInfo{lexema=x}):esc:ss) (ES, [Term Escreva, NonT ARG, Term PtV]) count = do
        return (Right $ "printf(\"" ++ fromJust x ++ "\");\n", SSymbol (ES, SemInfo Nothing Nothing):ss, count)
    -- 15
    semRule (SToken((lex, cl), info):ss) (ARG, [Term Literal]) count = do
        let lex2 = (tail . init) lex
        return (Right "", SSymbol (ARG, SemInfo{lexema=Just lex2, tipo=Nothing}):ss, count)
    -- 16
    semRule (SToken((lex, cl), info):ss) (ARG, [Term (Num _)]) count = do
        return (Right "", SSymbol (ARG, SemInfo{lexema=Just lex, tipo=Nothing}):ss, count)
    -- 17
    semRule (SToken((lex, cl), info):ss) (ARG, [Term Id]) count = do
        return (Right "", SSymbol (ARG, SemInfo{lexema=Just lex, tipo=Nothing}):ss, count)
    -- 18
    semRule (a:cmd:ss) (A, [NonT CMD, NonT A]) count = do
        return (Right "", SSymbol (A, SemInfo Nothing Nothing):ss, count)
    -- 19
    semRule (pt:ld:rcb:id:ss) (CMD, [Term Id, Term Rcb, NonT LD, Term PtV]) count = do
        let SToken((lx, cl), info) = id
        let SSymbol(LD, SemInfo{lexema=x}) = ld
        return (Right $ lx ++ "<-" ++ fromJust x ++ ";\n", SSymbol (CMD, SemInfo Nothing Nothing):ss, count)
    -- 20
    semRule (oprd2:opm:oprd1:ss) (LD, [NonT OPRD, Term Opm, NonT OPRD]) count = do
        let SSymbol(OPRD, SemInfo{lexema=x1}) = oprd1
        let SSymbol(OPRD, SemInfo{lexema=x2}) = oprd2
        let SToken((lx, cl), info) = opm

        let var = "T" ++ show count
        let exp = fromJust x1 ++ lx ++ fromJust x2

        return (Right $ "int " ++ var ++ " = " ++ exp ++ ";\n", SSymbol (LD, SemInfo{lexema=Just var, tipo=Nothing}):ss, count + 1)
    -- 21
    semRule (oprd:ss) (LD, [NonT OPRD]) count = do
        let SSymbol(OPRD, SemInfo{lexema=x}) = oprd

        return (Right "", SSymbol (LD, SemInfo{lexema=x, tipo=Nothing}):ss, count)
    -- 22
    semRule (id:ss) (OPRD, [Term Id]) count = do
        let SToken((lx, cl), info) = id

        return (Right "", SSymbol (OPRD, SemInfo{lexema=Just lx, tipo=Nothing}):ss, count)
    -- 23
    semRule (num:ss) (OPRD,  [Term (Num _)]) count = do
        let SToken((lx, cl), info) = num

        return (Right "", SSymbol (OPRD, SemInfo{lexema=Just lx, tipo=Nothing}):ss, count)
    -- 24
    semRule (a:cond:ss) (A, [NonT COND, NonT A]) count = do
        return (Right "", SSymbol (A, SemInfo Nothing Nothing):ss, count)
    -- 25
    semRule (cp:cab:ss) (COND, [NonT CAB, NonT CP]) count = do
        return (Right "}\n", SSymbol (COND, SemInfo Nothing Nothing):ss, count)
    -- 26
    semRule (ent:fc:exp:ab:se:ss) (CAB, [Term Se, Term AbP, NonT EXP_R, Term FcP, Term Entao]) count = do
        let SSymbol(EXP_R, SemInfo{lexema=x}) = exp
        return (Right $ "if (" ++ fromJust x ++ ") {\n", SSymbol (CAB, SemInfo Nothing Nothing):ss, count)
    -- 27
    semRule (oprd2:opr:oprd1:ss) (EXP_R, [NonT OPRD, Term Opr, NonT OPRD]) count = do
        let SSymbol(OPRD, SemInfo{lexema=x1}) = oprd1
        let SSymbol(OPRD, SemInfo{lexema=x2}) = oprd2
        let SToken((lx, cl), info) = opr

        let var = "T" ++ show count
        let exp = fromJust x1 ++ lx ++ fromJust x2

        return (Right $ "int " ++ var ++ " = " ++ exp ++ ";\n", SSymbol (EXP_R, SemInfo{lexema=Just var, tipo=Nothing}):ss, count + 1)
    -- 28
    semRule (cp:es:ss) (CP, [NonT ES, NonT CP]) count = do
        return (Right "", SSymbol (CP, SemInfo Nothing Nothing):ss, count)
    -- 29
    semRule (cp:cmd:ss) (CP, [NonT CMD, NonT CP]) count = do
        return (Right "", SSymbol (CP, SemInfo Nothing Nothing):ss, count)
    -- 30
    semRule (cp:cond:ss) (CP, [NonT COND, NonT CP]) count = do
        return (Right "", SSymbol (CP, SemInfo Nothing Nothing):ss, count)
    -- 31
    semRule (fim:ss) (CP, [Term FimSe]) count = do
        return (Right "", SSymbol (CP, SemInfo Nothing Nothing):ss, count)

    -- 39
    semRule (fim:ss) (A, [Term Fim]) count = do
        return (Right "", SSymbol (A, SemInfo Nothing Nothing):ss, count)

    semRule ss _ count = do
        return (Left "Regra semântica não encontrada.", ss, count)