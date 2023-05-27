module Lexical.Mgol where
    import Lexical.Automaton ( Transition, DFA, State)
    import Token
    
    mgolDFA :: DFA
    mgolDFA = ([-3..27], alphabet, mgolTransitions, 0, 
        [1, 3, 6, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
         25, 26, 27, -1])

    mgolToken :: State -> (Class, Type)
    mgolToken (-1) = (Ign, Null)
    mgolToken 1    = (Num, Integer)
    mgolToken 3    = (Num, Real)
    mgolToken 6    = (Num, Cientific)
    mgolToken 8    = (Lit, Null)
    mgolToken 9    = (Id, Null)
    mgolToken 11   = (Com, Null)
    mgolToken 12   = (Eof, Null)
    mgolToken 13   = (Opr, Null)
    mgolToken 14   = (Opr, Null)
    mgolToken 15   = (Opr, Null)
    mgolToken 16   = (Opr, Null)
    mgolToken 17   = (Opr, Null)
    mgolToken 18   = (Opr, Null)
    mgolToken 19   = (Rcb, Null)
    mgolToken 20   = (Opm, Null)
    mgolToken 21   = (Opm, Null)
    mgolToken 22   = (Opm, Null)
    mgolToken 23   = (Opm, Null)
    mgolToken 24   = (AbP, Null)
    mgolToken 25   = (FcP, Null)
    mgolToken 26   = (PtV, Null)
    mgolToken 27   = (Vir, Null)

    mgolToken (-3) = (Err CharN, Null)
    mgolToken (-2) = (Err CharI, Null)
    mgolToken 0    = (Err CharN, Null)
    mgolToken 2    = (Err RealI, Null)
    mgolToken 4    = (Err CienI, Null)
    mgolToken 5    = (Err CienI, Null)
    mgolToken 7    = (Err LitI, Null)
    mgolToken 10   = (Err ComI, Null)
    mgolToken _    = (Err Unk, Null)
    

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