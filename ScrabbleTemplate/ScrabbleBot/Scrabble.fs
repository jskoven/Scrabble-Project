namespace JohnBuks

open System
open System.Diagnostics
open Eval
open ScrabbleBot
open ScrabbleBot.Types
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
open StateMonad
open Types
open handCalculations
open boardCalculations
open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    open Types
    
    type state = {
        //We can (and should) add stuff we wish to keep track of in here. Lasse for example mentioned
        //that his group added a way to keep track of already placed tiles in here (which of course
        //is needed). He said his was mostly just a coordinate system, being an x,y system and
        //whichever tile is places.
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        playerTurn    : uint32
        nrOfTurns     : uint32
        nrOfPlayers   : uint32
        myPoints      : uint32
        hand          : MultiSet.MultiSet<uint32>
        // Use the types.fs types in the map, such that the key is a coord type, and the
        // value is option<chartype*square> where chartype is a type we need to define ourself.
        // We need to define it such that char contains both the current char but also said char's
        // point value.
        tilesOnBoard  : Map<coord, placedTile>
        //Tilføj new pieces, tilføj tilesonboard
        tiles         : Map<uint32, tile>
        isServerOutOfTiles : bool
    }

    let mkState b d pn h t pt nop = {board = b; dict = d; playerNumber = pn; myPoints = 0u; hand = h; tilesOnBoard = Map.empty; tiles = t; nrOfTurns = 0u; playerTurn = pt; nrOfPlayers = nop; isServerOutOfTiles = false }


    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    


module Scrabble =
    open System.Threading
        let updatePlayerTurn (playerTurn : uint32) (nrOfPlayers : uint32) =
        if playerTurn+1u > nrOfPlayers
            then 1u
            else playerTurn+1u

    open State
    open Dictionary
    
    
    
    type direction =
        | Right
        | Down
    
    let nextCoord (x,y) direction =
        match direction with
        |Right -> (x+1,y)
        |Down -> (x, y+1)
      
    let prevCoord (x,y) direction =
        match direction with
        |Right -> (x-1,y)
        |Down -> (x, y-1)  

  
    
    
    let rec isHorizontalLineWord (x,y) (tiles: Map<coord, placedTile>) (dict: Dict) originalCoord originalChar=
        //Need to add original tile that we are "checking from" in here as well.
        match (x,y) with
        |(x,y) when (x,y) = originalCoord ->
            match step originalChar dict with
                | None -> false
                | Some (b, child) ->
                    isHorizontalLineWord (x+1,y) tiles child originalCoord originalChar
        |(_,_) ->
            match Map.tryFind (x,y) tiles with
            | Some (cv, pv) ->
                match step cv dict with
                | None -> false
                | Some (b, child) ->
                    isHorizontalLineWord (x+1,y) tiles child originalCoord originalChar
            | None -> true

    let rec placedWordToWord (placeWord: (coord * (uint32 * (char * int))) list) =
        List.fold (fun acc (_,(_,(c,_))) -> acc + (string c)) "" placeWord

    let checkAdjecent st dir (x,y) =
        match dir with
        |Right ->
            match Map.tryFind (x,y-1) st with
            |Some (cv, pv) -> true
            |_ ->
                match Map.tryFind (x,y+1) st with
                |Some (cv, pv) -> true
                |_ -> false
        |Down ->
            match Map.tryFind (x-1,y) st with
            |Some(cv,pv) -> true
            |_ ->
                match Map.tryFind (x+1,y) st with
                |Some(cv, pv ) -> true
                |_ -> false
                    
   
    let rec isExtendedWord (x,y) (tiles: Map<coord, placedTile>) (dict: Dict) originalCoord originalChar (direction:direction)=
        //Need to add original tile that we are "checking from" in here as well.
        match direction with
        | Right ->
            match (x,y) with
            |(x,y) when (x,y) = originalCoord ->
                match step originalChar dict with
                    | None -> false
                    | Some (b, child) ->
                        isExtendedWord (x+1,y) tiles child originalCoord originalChar direction
            |(_,_) ->
                match Map.tryFind (x,y) tiles with
                | Some (cv, pv) ->
                    match step cv dict with
                    | None -> false
                    | Some (b, child) ->
                        isExtendedWord (x+1,y) tiles child originalCoord originalChar direction
                | None -> true
        | Down ->
            match (x,y) with
            |(x,y) when (x,y) = originalCoord ->
                match step originalChar dict with
                    | None -> false
                    | Some (b, child) ->
                        isExtendedWord (x,y+1) tiles child originalCoord originalChar direction
            |(_,_) ->
                match Map.tryFind (x,y) tiles with
                | Some (cv, pv) ->
                    match step cv dict with
                    | None -> false
                    | Some (b, child) ->
                        isExtendedWord (x,y+1) tiles child originalCoord originalChar direction
                | None -> true
    
    let rec findLeftMostTile (x,y) (tiles: Map<coord, placedTile>) (st: state) originalCoord originalChar =
        match Map.tryFind (x-1,y) tiles with
        | Some (cv, pv) -> findLeftMostTile (x-1,y) tiles st originalCoord originalChar
        | None -> isExtendedWord (x+1,y) tiles st.dict originalCoord originalChar Right
        
    let rec findTopMostTile (x, y) (tiles: Map<coord, placedTile>) (st: state) originalCoord originalChar =
        match Map.tryFind (x, y-1) tiles with
        | Some (cv, pv) -> findTopMostTile (x, y-1) tiles st originalCoord originalChar
        | None -> isExtendedWord (x, y+1) tiles st.dict originalCoord originalChar Down
     
    let bestWord (word1:(coord * (uint32 * placedTile)) list) (word2:(coord * (uint32 * placedTile)) list) =
        let word1Points = List.fold (fun acc (_,(_,(cv,pv))) -> acc+pv) 0 word1
        let word2Points = List.fold (fun acc (_,(_,(cv,pv))) -> acc+pv) 0 word2
        if word1Points > word2Points then word1 else word2
        
        
    let rec updateAnchorPointsAux (i: int) (x,y) (listOfCoords: coord Set) (st:state) dir =
        match dir with
        | Right ->
            match i with
            | i when i = ((MultiSet.size st.hand) |> int) -> listOfCoords
            | _ -> updateAnchorPointsAux (i+1) (x-1,y) (Set.add (x, y) listOfCoords) st dir
        |Down ->
            match i with
            | i when i = ((MultiSet.size st.hand) |> int) -> listOfCoords
            | _ -> updateAnchorPointsAux (i+1) (x,y-1) (Set.add (x, y) listOfCoords) st dir
    
    let updateAnchorPoints (st: state) dir =
        Map.fold (fun acc key _ -> (updateAnchorPointsAux 0 key acc st dir)) Set.empty st.tilesOnBoard
    
    let isNextTileEmpty (x,y) (st:state) dir =
        match Map.tryFind (nextCoord (x,y) dir) st.tilesOnBoard  with
        |Some (cv,pv) -> false
        |None -> true
       
       
    let isPrevTileEmpty (x,y) (st:state) dir =
        match Map.tryFind (prevCoord (x,y) dir) st.tilesOnBoard  with
        |Some (cv, pv) -> false
        |None -> true
    
    let moveFromCoord c dir (st : state) =
        let outsideBoard coord =
            match (st.board.squares coord) with
            | Success map -> map |> Option.map (fun _ -> false) |> Option.defaultValue true
                       
        let rec aux best acc coord hand dict isIntersecting =
            if outsideBoard coord then best else    
                //Tryfind on the coord with tilesonboard
                match Map.tryFind coord st.tilesOnBoard with
                //A tile is there
                | Some (cv, pv) ->
                    match Map.tryFind (prevCoord coord dir) st.tilesOnBoard with
                    |Some (cv,pv) -> best
                    |None -> 
                        //See if we can step further in our dict with that tile
                        match step cv dict with
                        //Cant, return best (?)
                            | None -> best
                        //Can, check if it's a word. If it is, check if better than current best word.
                            | Some (b, child) ->
                                aux best acc (nextCoord coord dir) hand child true
                //No tile present, fold over hand
                | None ->
                    MultiSet.fold (fun best' id _ ->
                        // Todo maybe: fold henover hvert character istedet som et tile kan repræsenterer
                        let (cv, pv) = Map.find id st.tiles |> Set.minElement
                        let isAdjecent = checkAdjecent st.tilesOnBoard dir coord
                        if isAdjecent then best' else
                        // Todo: se om vi kan ligge den brik på det omvendte led
                        //Check if we can step with each of our tiles in hand
                            match MultiSet.containsValue id hand with
                                |true -> 
                                    match step cv dict with
                                    //Cant step, use best'
                                    | None -> best'
                                    //can step
                                    | Some (b, child) ->
                                        //Remove said tile from hand for next recursive call
                                        //forcePrint (sprintf "\n #### HAND BEFORE : %A \n" ( hand))
                                        let newHand = MultiSet.removeSingle id hand
                                        //forcePrint (sprintf "\n #### HAND  after : %A \n" (newHand))
                                        //Add tile to acc
                                        let newAcc = List.append acc [(coord, (id, (cv, pv)))]
                                        //Check if better than current best
                                        let nextTileEmpty = isNextTileEmpty coord st dir
                                        let newBest = if (b && isIntersecting && nextTileEmpty)
                                                      then bestWord best' newAcc
                                                      else best'
                                        aux newBest newAcc (nextCoord coord dir) newHand child isIntersecting
                                |false -> best'
                        ) best hand
        let isFirstMove = c = st.board.center
        if (isPrevTileEmpty c st dir)
        then
            aux [] [] c st.hand st.dict isFirstMove
        else []
    
    
    let getIDs (word: placedWord) =
        List.fold (fun acc (_, (id, (_, _))) -> id :: acc) [] word     

    let playGame cstream pieces (st : State.state) =
    //Playgame is called at start. St is the players state, which must be updated based on what
    //happens, what is played, etc
        let rec aux (st : State.state) =
            if st.playerNumber = st.playerTurn then 
                DebugPrint.forcePrint (sprintf "player %A's turn\n" st.playerNumber) 
                //Aux is called again and again
                //Print.printHand pieces (State.hand st)
                
                if Map.isEmpty st.tilesOnBoard then
                    let moveRight = moveFromCoord (st.board.center) Right st
                    let moveDown = moveFromCoord (st.board.center) Down st
                    let move = bestWord moveRight moveDown
                    
                    match move with
                        | [] ->
                            if st.isServerOutOfTiles then
                                send cstream (SMPass)
                            else
                                send cstream (SMChange [List.head (MultiSet.toList st.hand)])
                        | _ -> send cstream (SMPlay move)
                    
                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)

                else
                    let anchorPointsRight = updateAnchorPoints st Right
                    let moveRight = Set.fold (fun best coord  ->
                        bestWord (moveFromCoord coord Right st) (best) ) [] anchorPointsRight
                    
                    let anchorPointsDown = updateAnchorPoints st Down
                    let moveDown = Set.fold (fun best coord ->
                        bestWord (moveFromCoord coord Down st) (best) ) [] anchorPointsDown
                    
                    let move = bestWord moveRight moveDown
                    debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

                    
                    (*match move with
                        | [] ->
                            if st.isServerOutOfTiles then
                                debugPrint (sprintf "Player: %A : server out of tiles, passing" st.playerNumber)
                                send cstream (SMPass)
                            else
                                debugPrint (sprintf "Player: %A : Nothing to play, swapping tiles" st.playerNumber)
                                send cstream (SMChange [List.head (MultiSet.toList st.hand)])
                        | _ -> send cstream (SMPlay move)*)
                    
                    match move with
                        | [] ->
                            debugPrint (sprintf "Player: %A : server out of tiles, passing" st.playerNumber)
                            send cstream (SMPass)
                        | _ -> send cstream (SMPlay move)
                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)

                 // keep the debug lines. They are useful.
            let msg = recv cstream
            
            
            match msg with
            //Msg is the answer from the server after a given play. For each possible answer, some
            //the player state must be updated in order to make sure that the state we have
            //on our pc reflects the state which is on the server.
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *) 
                let newHand = removePieces (getIDs ms) st.hand |> addPieces newPieces
                let newBoard = updateTilesOnBoard st.tilesOnBoard ms
                
                let newPoints = st.myPoints + uint32 points 
                let st' = {st with hand = newHand; tilesOnBoard = newBoard; myPoints = newPoints; playerTurn = updatePlayerTurn st.playerTurn st.nrOfPlayers }
                aux st'
            | RCM (CMPassed (pid)) ->
                //if (pid = st.playerNumber)
                let st' = {st with playerTurn = updatePlayerTurn st.playerTurn st.nrOfPlayers}
                aux st'
            | RCM (CMChange (_,_)) ->
                //debugPrint (sprintf)
                debugPrint "CMCHange recieved \n"
                let st' = {st with playerTurn = updatePlayerTurn st.playerTurn st.nrOfPlayers}
                aux st'
            | RCM (CMChangeSuccess (newPieces)) ->
                debugPrint "CMChangeSucess recieved \n"
                let newHand = MultiSet.empty |> addPieces newPieces
                let st' = {st with hand = newHand; playerTurn = updatePlayerTurn st.playerTurn st.nrOfPlayers}
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let newBoard = updateTilesOnBoard st.tilesOnBoard ms
                let st' = {st with tilesOnBoard = newBoard; playerTurn = updatePlayerTurn st.playerTurn st.nrOfPlayers}
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let newBoard = updateTilesOnBoard st.tilesOnBoard ms
                let st' = {st with tilesOnBoard = newBoard; playerTurn = updatePlayerTurn st.playerTurn st.nrOfPlayers}
                aux st'
            
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                let error = sprintf "Gameplay Error:\n%A" err
                if error.Contains "GPENotEnoughPieces" then
                    let st' = {st with isServerOutOfTiles = true; playerTurn = updatePlayerTurn st.playerTurn st.playerNumber}
                    aux st'
                else
                    let st' = {st with playerTurn = updatePlayerTurn st.playerTurn st.playerNumber }
                    aux st'


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32)
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            //Hand er muligvis tal men faktisk bogstaver. As in, 0 er f.eks. A, 1 er B osv.
            //Ikke sikkert men maybe.
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n
                      board = %A\n\n" numPlayers playerNumber playerTurn hand timeout boardP)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet tiles playerTurn numPlayers)
        