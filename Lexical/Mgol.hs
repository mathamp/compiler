module Lexical.Mgol where
    import Lexical.Automaton ( Transition, DFA, State)
    import Token
    
    mgolDFA :: DFA
    mgolDFA = ([-3..27], alphabet, mgolTransitions, 0, 
        [1, 3, 6, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
         25, 26, 27, -1])

    mgolToken :: State -> Class
    mgolToken (-1) = Ign
    mgolToken 1    = Num Null
    mgolToken 3    = Num Null
    mgolToken 6    = Num Null
    mgolToken 8    = Literal
    mgolToken 9    = Id
    mgolToken 11   = Com
    mgolToken 12   = Eof
    mgolToken 13   = Opr
    mgolToken 14   = Opr
    mgolToken 15   = Opr
    mgolToken 16   = Opr
    mgolToken 17   = Opr
    mgolToken 18   = Opr
    mgolToken 19   = Rcb
    mgolToken 20   = Opm
    mgolToken 21   = Opm
    mgolToken 22   = Opm
    mgolToken 23   = Opm
    mgolToken 24   = AbP
    mgolToken 25   = FcP
    mgolToken 26   = PtV
    mgolToken 27   = Vir

    mgolToken (-3) = Err CharN
    mgolToken (-2) = Err CharI
    mgolToken 0    = Err CharN
    mgolToken 2    = Err RealI
    mgolToken 4    = Err CienI
    mgolToken 5    = Err CienI
    mgolToken 7    = Err LitI
    mgolToken 10   = Err ComI
    mgolToken _    = Err Unk
    

    nums    = ['0'..'9']
    letters = ['a'..'z'] ++ ['A'..'Z']
    others  = [',', ';', ':', '.', '!', '?', '\\', '+', '-', '*', '/', '(', ')',
               '{', '}', '[', ']', '<', '>', '=', '\'', '"', '_', ' ']
    blanks  = ['\n', '\t', ' ']
    
    alphabet = nums ++ letters ++ others

    mgolTransitions :: Transition
    mgolTransitions (-1) c | c `elem` blanks = Just (-1)

    mgolTransitions 0 c | c `elem` blanks  = Just (-1)
    mgolTransitions 0 c | c `elem` nums    = Just 1
    mgolTransitions 0 '"'                  = Just 7
    mgolTransitions 0 c | c `elem` letters = Just 9
    mgolTransitions 0 '{'                  = Just 10
    mgolTransitions 0 '<'                  = Just 13
    mgolTransitions 0 '>'                  = Just 15
    mgolTransitions 0 '='                  = Just 17
    mgolTransitions 0 ','                  = Just 27
    mgolTransitions 0 '+'                  = Just 20
    mgolTransitions 0 '-'                  = Just 21
    mgolTransitions 0 '*'                  = Just 22
    mgolTransitions 0 '/'                  = Just 23
    mgolTransitions 0 '('                  = Just 24
    mgolTransitions 0 ')'                  = Just 25
    mgolTransitions 0 ';'                  = Just 26
    mgolTransitions 0 c | c `elem` alphabet = Just (-3)
    mgolTransitions 0 _                    = Just (-2)

    mgolTransitions 1 c | c `elem` nums       = Just 1
    mgolTransitions 1 '.'                     = Just 2
    mgolTransitions 1 c | c `elem` ['e', 'E'] = Just 4

    mgolTransitions 2 c | c `elem` nums = Just 3

    mgolTransitions 3 c | c `elem` nums       = Just 3
    mgolTransitions 3 c | c `elem` ['e', 'E'] = Just 4

    mgolTransitions 4 c | c `elem` ['+', '-'] = Just 5
    mgolTransitions 4 c | c `elem` nums       = Just 6

    mgolTransitions 5 c | c `elem` nums = Just 6

    mgolTransitions 6 c | c `elem` nums = Just 6

    mgolTransitions 7 '"'                    = Just 8
    mgolTransitions 7 c | c `elem` alphabet  = Just 7

    mgolTransitions 9 c | c `elem` ('_' : nums ++ letters) = Just 9
    
    mgolTransitions 10 '}'                   = Just 11
    mgolTransitions 10 c | c `elem` alphabet = Just 10

    mgolTransitions 13 '=' = Just 14
    mgolTransitions 13 '-' = Just 19
    mgolTransitions 13 '>' = Just 18

    mgolTransitions 15 '=' = Just 16
    
    mgolTransitions _ _ = Nothing