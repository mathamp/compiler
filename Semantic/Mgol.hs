module Semantic.Mgol where
    import Syntatic.Mgol (Input, SynStack, lookupSlr)
    import Syntatic.Table (Action (..), gotoMap)
    import Syntatic.Grammar (Symbol(..), Deriv (..))
    import Token
    -- import Semantic.Rules (semRule)
    
    data SemInfo = SemInfo {
        tipo :: Maybe String,
        lexema :: Maybe String
    }
        deriving (Show)

    data SemSymbol = SSymbol (Symbol, SemInfo) | SToken (Token, SemInfo)
        deriving (Show)

    type SemStack = [SemSymbol]

    stepSlr2 :: Input -> SynStack -> (Input, SynStack, Action)
    stepSlr2 ((p, (lx, a)) : as) (s : ss) = case lookupSlr a s of
            Shift t         -> (as, t : s : ss, Shift t)
            Reduce (aa, bb) -> ((p, (lx, a)) : as, newStack', Reduce (aa, bb))
                where
                    newStack    = drop (length bb) (s:ss)
                    t           = head newStack
                    newStack'   = gotoMap t aa : newStack

            Accept          -> ((p, (lx, a)) : as, s : ss, Accept)
            Error           -> ((p, (lx, a)) : as, s : ss, Error)