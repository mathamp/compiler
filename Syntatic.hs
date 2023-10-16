module Syntatic where
    import Token (Token)
    import Syntatic.Table (Action (..))
    import Syntatic.Mgol (stepSlr, Input, SynStack)
    import Syntatic.Automaton (State(S))
    
    -- parser :: Input -> [Action]
    parser ts = parser2 ts [S 0] []

    -- parser2 :: Input -> Stack -> [Action] -> (Input, Stack, [Action])
    parser2 _ [] _ = error "Pilha vazia"
    parser2 ts ss as =

        case stepSlr ts ss of
            (ts2, ss2, Shift t) -> parser2 ts2 ss2 (Shift t :as)
            (ts2, ss2, Reduce (aa, bb)) -> parser2 ts2 ss2 (Reduce (aa, bb) :as)
            (ts2, ss2, Accept) -> (ts2, ss2, Accept :as)
            (ts2, ss2, Error) -> (ts2, ss2, Error :as)
        
    