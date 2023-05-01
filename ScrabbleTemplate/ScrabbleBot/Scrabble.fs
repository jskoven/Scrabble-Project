namespace JohnBuks

open ScrabbleBot
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
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
        hand          : MultiSet.MultiSet<uint32>
        // Use the types.fs types in the map, such that the key is a coord type, and the
        // value is option<chartype*square> where chartype is a type we need to define ourself.
        // We need to define it such that char contains both the current char but also said char's
        // point value.
        tilesOnBoard  : Map<coord, placedTile>
        //Tilføj new pieces, tilføj tilesonboard
        tiles         : Map<uint32, tile>
    }

    let mkState b d pn h t = {board = b; dict = d; playerNumber = pn; hand = h; tilesOnBoard = Map.empty; tiles = t }


    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    


module Scrabble =
    open System.Threading
    open State
    open Dictionary
    //open MultiSet
    
    
    type direction =
        | Right
        | Down
    
    let nextCoord (x,y) direction =
        match direction with
        |Right -> (x+1,y)
        |Down -> (x, y-1)
        
    let checkLeftAndRight (x,y) (tiles: Map<coord, placedTile>) =
        match Map.tryFind (x-1,y) tiles with
        | Some (cv, pv) -> true
        | None ->
            match Map.tryFind (x+1,y) tiles with
            | Some (cv, pv) -> true
            | None -> false

    let rec findLeftMostTile (x,y) (tiles: Map<coord, placedTile>) =
        match Map.tryFind (x-1,y) tiles with
        | Some (cv, pv) -> findLeftMostTile (x-1,y) tiles
        | None -> (x,y)
        
    let rec isHorizontalLineWord (x,y) (tiles: Map<coord, placedTile>) (dict: Dict)=
        //Need to add original tile that we are "checking from" in here as well. 
        match Map.tryFind (x,y) tiles with
        | Some (cv, pv) ->
            match step cv dict with
            | None -> false
            | Some (b, child) ->
                isHorizontalLineWord (x+1,y) tiles child
        | None -> true

    let rec placedWordToWord (placeWord: (coord * (uint32 * (char * int))) list) =
        List.fold (fun acc (_,(_,(c,_))) -> acc + (string c)) "" placeWord

    
    let bestWord word1 word2 =
        if List.length word1 > List.length word2 then word1 else word2
    let moveFromCoord c dir (st : state) =
        let rec aux best acc coord hand dict =
            //Tryfind on the coord with tilesonboard
            match Map.tryFind coord st.tilesOnBoard with
            //A tile is there
            | Some (cv, pv) ->
                forcePrint (sprintf "Found on board value: %A \n" cv)
                //See if we can step further in our dict with that tile
                match step cv dict with
                //Cant, return best (?)
                    | None -> best
                //Can, check if it's a word. If it is, check if better than current best word.
                    | Some (b, child) ->
                        let newBest = if b then bestWord best acc else best
                        aux newBest acc (nextCoord coord dir) hand child
            //No tile present, fold over hand
            | None ->
                MultiSet.fold (fun best' id _ ->
                    // Todo maybe: fold henover hvert character istedet som et tile kan repræsenterer
                    let (cv, pv) = Map.find id st.tiles |> Set.minElement
              
                    // Todo: se om vi kan ligge den brik på det omvendte led !!!
                   
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
                                let newBest = if b then bestWord best' newAcc else best'
                                aux newBest newAcc (nextCoord coord dir) newHand child
                        |false -> best'
                    ) best hand
        aux [] [] c st.hand st.dict
    
    
    let getIDs (word: placedWord) =
        List.fold (fun acc (_, (id, (_, _))) -> id :: acc) [] word     

    let playGame cstream pieces (st : State.state) =
    //Playgame is called at start. St is the players state, which must be updated based on what
    //happens, what is played, etc
    
        let rec aux (st : State.state) =      
            //Aux is called again and again
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            
            //let input =  System.Console.ReadLine()
            //let move = RegEx.parseMove input
            let move =
                match Map.fold (fun _ coord _ -> Some(coord)) None st.tilesOnBoard with
                |Some(coord) -> moveFromCoord coord Right st
                |None -> moveFromCoord (0,0) Right st
                
            let move = match lookup (placedWordToWord move) st.dict with
                        |true -> move
                        |false ->
                            match Map.fold (fun _ coord _ -> Some(coord)) None st.tilesOnBoard with
                                |Some(coord) -> moveFromCoord coord Down st
                                |None -> moveFromCoord (0,0) Down st
            
            let move = match lookup (placedWordToWord move) st.dict with
                        |true -> move
                        |false -> []
            
            
            
            //let move = moveFromCoord (0,0) Right st
            
            
            //let move = moveFromCoord (0,0) Right st
            //forcePrint (sprintf "\n ####  In hands :   %A ####\n" st.hand)
            //forcePrint (sprintf "\n ####  Attempted move :  %A ####\n" move)
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            //Msg is the answer from the server after a given play. For each possible answer, some
            //the player state must be updated in order to make sure that the state we have
            //on our pc reflects the state which is on the server.
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *) 
                let newHand = removePieces (getIDs ms) st.hand |> addPieces newPieces
                let newBoard = updateTilesOnBoard st.tilesOnBoard ms
                let st' = {st with hand = newHand; tilesOnBoard = newBoard}
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let newBoard = updateTilesOnBoard st.tilesOnBoard ms
                let st' = {st with tilesOnBoard = newBoard}
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let newBoard = updateTilesOnBoard st.tilesOnBoard ms
                let st' = {st with tilesOnBoard = newBoard}
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error: %A\n" err; aux st


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
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet tiles)
        