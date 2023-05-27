module Main where

    import System.Environment
    import Lexical
    import SymbolTable
    import Token
    import Data.Map
    import Data.Maybe
    
    getInitialSymbolTable = 
        Data.Map.fromList [
            ("inicio", (Inicio, Null)),
            ("varinicio", (VarInicio, Null)),
            ("varfim", (VarFim, Null)),
            ("escreva", (Escreva, Null)),
            ("leia", (Leia, Null)),
            ("se", (Se, Null)),
            ("entao", (Entao, Null)),
            ("fimse", (FimSe, Null)),
            ("repita", (Repita, Null)),
            ("fimRepita", (FimRepita, Null)),
            ("fim", (Fim, Null))
        ]

    printPosToken :: (Position, Token) -> IO ()
    printPosToken ( _    , (_, (Ign, _))) = return ()
    printPosToken ((x, y), tok)           = putStrLn $ "Linha: " ++ show y ++ 
        "\tColuna: " ++ show x ++ "\tToken: " ++ showToken tok

    printTokenList :: [(Position, Token)] -> IO ()
    printTokenList [] = return ()
    printTokenList (x:xs) = do
        printPosToken x
        printTokenList xs

    fromPosTokenList :: [(Position, Token)] -> SymbolTable -> SymbolTable
    fromPosTokenList [] st = st
    fromPosTokenList (x:xs) st = 
        if isJust $ findToken lex st 
        then fromPosTokenList xs st
        else fromPosTokenList xs $ addToken lex typ st
            where
                (_, (lex, typ)) = x

    main = do
        args <- getArgs

        case args of
            [] -> do
                putStrLn "No file supplied as argument"
            [arg] -> do
                contents <- readFile arg
                let tokenList = parse contents
                let symTab = fromPosTokenList tokenList getInitialSymbolTable
                printTokenList tokenList

            _ -> do
                putStrLn "Too many arguments"