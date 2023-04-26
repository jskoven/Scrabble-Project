
module internal ScrabbleBot.Types

    open StateMonad
    type Error = 
            | VarExists of string
            | VarNotFound of string
            | IndexOutOfBounds of int
            | DivisionByZero 
            | ReservedName of string
    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type placedTile = char * int
    type square = Map<int, squareFun>
    type placedWord = ((int * int) * (uint32 * placedTile)) list