module Syntatic.Automaton where
    import Prelude hiding (Word)
    import Syntatic.Grammar ( Deriv, Deriv (Term))
    import Token ( Token, Class(Eof) )
    
    data State = S Int | S_ES Int | S_CMD Int | 
        S_COND Int | S_COND_ES Int | S_COND_CMD Int | S_COND_COND Int |
        S_R Int | S_R_ES Int | S_R_CMD Int | S_R_COND Int
        deriving (Eq, Show)
    
    type InputA = Token
    type StackA = Deriv
    type Initial = State
    type Final = State
    type Word = [InputA]
    type Stack = [StackA]
    type Transition = State -> InputA -> Stack -> (State, Stack)

    type PA = ([State], [Deriv], [StackA], Transition, Initial, StackA, [Final])

    evalPA :: PA -> Word -> Bool
    evalPA (qs, sigma, gamma, delta, starti, stacki, fs) ws = transition starti ws [Term Eof]
        where
            transition :: State -> Word -> Stack -> Bool
            transition q [] _ | q `elem` fs = True
                              | otherwise   = False
            
            transition q (x:xs) st = transition nq xs (nst ++ st)
                where
                    nq = fst $ delta q x st
                    nst = snd $ delta q x st
    