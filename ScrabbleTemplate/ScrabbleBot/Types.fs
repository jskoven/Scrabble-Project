module internal ScrabbleBot.Types

open StateMonad
    
    type coord  = int * int 
    type word   = (char * int) list
    
    type squareFun = word -> int -> int -> Result<int, Error>
    
    type square = Map<int, squareFun>
    
    type tile = char * int
    
    type boardFun = coord -> Result<square option, Error>
    
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }