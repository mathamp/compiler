module Syntatic.Mgol where
    import Syntatic.Grammar (Deriv)
    import Token (Token, Class (..))
    import Syntatic.Automaton ( State(..) )
    import Data.List ( find )
    import Data.Maybe ( fromJust )

    import qualified Data.List.NonEmpty as NE
    import Data.List.NonEmpty (NonEmpty((:|)))

    import Syntatic.Table (gotoMap, actionMap, Action (..))

    import qualified Data.Map as Map
    import Lexical (Position)
    

    type Input = [(Position, Token)]
    type SynStack = [State]

    lookupSlr :: Class -> State -> Action
    lookupSlr cl s =
        actionMap s cl

    stepSlr :: Input -> SynStack -> (Input, SynStack, Action)
    stepSlr ((p, (lx, a)) : as) (s : ss) = case lookupSlr a s of
        Shift t         -> (as, t : s : ss, Shift t)
        Reduce (aa, bb) -> ((p, (lx, a)) : as, newStack2, Reduce (aa, bb))
            where
                newStack    = drop (length bb) (s:ss)
                t           = head newStack
                newStack2   = gotoMap t aa : newStack
        Accept          -> ((p, (lx, a)) : as, s : ss, Accept)
        Error           -> ((p, (lx, a)) : as, s : ss, Error)


    allStates = sStates ++ esStates ++ cmdStates ++ condStates ++ condesStates ++
        condcmdStates ++ condcondStates ++ rStates ++ resStates ++ rcmdStates ++ rcond

    sStates = map S [1..19]
    esStates = map S_ES [1..11]
    cmdStates = map S_CMD [1..11]
    condStates = map S_COND [1..15]
    condesStates = map S_COND_ES [1..2]
    condcmdStates = map S_COND_CMD [1..2]
    condcondStates = map S_COND_COND [1..2]
    rStates = map S_R [1..14]
    resStates = map S_R_ES [1..2]
    rcmdStates = map S_R_CMD [1..2]
    rcond = map S_R_COND [1..2]
