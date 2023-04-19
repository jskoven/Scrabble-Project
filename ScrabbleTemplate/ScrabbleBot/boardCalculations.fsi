module internal ScrabbleBot.boardCalculations
open Types
open ScrabbleUtil

    val updateTilesOnBoard : Map<coord,placedTile> -> placedWord -> Map<coord,(char * int)>
