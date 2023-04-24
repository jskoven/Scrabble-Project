﻿namespace YourClientName

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
    open Dict
    open MultiSet
    
    
        
        
        
    let getIDs (word: placedWord) =
        List.fold (fun acc (_, (id, (_, _))) -> id :: acc) [] word     

    let playGame cstream pieces (st : State.state) =
    //Playgame is called at start. St is the players state, which must be updated based on what
    //happens, what is played, etc
        let rec aux (st : State.state) =
            
            let calculateMove (currentState: state) (lastMove) (dict: Dict)=
                let playerHand = currentState.hand
                let tilesOnBoard = currentState.tilesOnBoard
                
                // Husk også at få fat i koordinater somehow.
                // Hvis man starter med at spille, starter man på 0,0
                // Hvis man ikke starter, så skal funktionen have den koordinat på den tile
                // man starter med at bygge, med i funktionen. Man skal også huske at
                // så plusse / minusse x,y koordinaterne efter hvilken retning man går
                // (TA'en nævnte at de havde en direction variabel eller sådan noget)
                
                let rec checkmove hand =
                    MultiSet.fold
                        (fun acc key amount->
                            (let actualTile = Map.find key pieces
                             let isWord = step actualTile dict
                             let tempCharacterList = MultiSet.removeSingle key hand
                             match isWord with
                             |Some (true, d) -> acc
                             |Some (false, d) -> acc @ (checkmove tempCharacterList)
                             |_ -> checkmove tempCharacterList
                             )
                        )
                        List.empty hand
                
                checkmove playerHand
            
                    
                
                
                
            //Aux is called again and again
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

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
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


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
        