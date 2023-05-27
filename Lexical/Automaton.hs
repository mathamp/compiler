module Lexical.Automaton where
    import Prelude hiding (Word)
    import Data.Maybe (isJust, fromJust)

    type State = Int
    type Symbol = Char
    type Initial = State
    type Final = State
    type Word = String
    type Transition = State -> Symbol -> Maybe State

    type DFA = ([State], [Symbol], Transition, Initial, [Final])

    evalDFA :: DFA -> Word -> Either State State
    evalDFA (qs, sigma, delta, s, fs) = transition s
        where
            transition :: State -> Word -> Either State State
            transition q [] | q `elem` fs = Right q
                            | otherwise   = Left q
            
            transition q (c:cs) | isJust res = transition (fromJust res) cs
                                | otherwise  = Left q
                where
                    res = delta q c