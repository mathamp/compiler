module SymbolTable where
    import Prelude hiding (lookup)
    import Data.Map
    import Token

    type Key = Lexeme
    type Value = (Class, Type)

    type SymbolTable = Map Key Value

    findToken :: Key -> SymbolTable -> Maybe Value
    findToken = lookup

    addToken :: Key -> Value -> SymbolTable -> SymbolTable
    addToken = insert

    updateToken :: (Value -> Value) -> Key -> SymbolTable -> SymbolTable
    updateToken = adjust