
module Types
    type Error = 
            | VarExists of string
            | VarNotFound of string
            | IndexOutOfBounds of int
            | DivisionByZero 
            | ReservedName of string
    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type tile = char * int
    type square = Map<int, squareFun>