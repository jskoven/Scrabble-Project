// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad
    open System

    let add a b = a >>= fun x -> b >>= fun y -> ret (x+y) 
    let div a b = a >>= fun x -> b >>= fun y -> if y <> 0 then ret (x/y) else fail DivisionByZero      

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let binop f a b = a >>= fun x -> b >>= fun y -> ret (f x y)
    
    let modu a b = a >>= fun x -> b >>= fun y -> if y <> 0 then ret (x%y) else fail DivisionByZero
    
    let rec arithEval a : SM<int> =
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV pv -> arithEval pv >>= fun result -> pointValue result
        | Add (a1, a2) -> binop ( + ) (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> binop ( - ) (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> binop ( * ) (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (a1, a2) -> modu (arithEval a1) (arithEval a2)
        | CharToInt c -> charEval c >>= fun r -> r |> int |> ret                                              
    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV a -> arithEval a >>= fun r -> r |> characterValue
        | ToUpper c -> charEval c >>= fun r -> r |> Char.ToUpper |> ret
        | ToLower c -> charEval c >>= fun r -> r |> Char.ToLower |> ret
        | IntToChar a -> arithEval a >>= fun r -> r |> char |> ret

    let rec boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a1, a2) -> binop ( = ) (arithEval a1) (arithEval a2)
        | ALt (a1, a2) -> binop ( < ) (arithEval a1) (arithEval a2)
        | Not b -> boolEval b >>= fun r -> r |> not |> ret
        | Conj (b1, b2) -> binop ( && ) (boolEval b1) (boolEval b2)
        | IsVowel c -> charEval c >>= fun r -> r |> "aeiouAEIOUæøåÆØÅ".Contains |> ret
        | IsDigit c -> charEval c >>= fun r -> r |> Char.IsDigit |> ret
        | IsLetter c -> charEval c >>= fun r -> r |> Char.IsLetter |> ret


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare s -> declare s
        | Ass (x, a) -> arithEval a >>= (fun r -> update x r) //spent a long time figuring out I needed that update function... and no, update does not need to declare a new variable if the variable cannot be found
        | Skip -> ret ()
        | Seq (s1, s2) -> stmntEval s1 >>>= stmntEval s2 >>= (fun r -> ret r) //the >>>= is more or less akin to function composition the way I understand it
        | ITE (b, s1 ,s2) -> push >>>= boolEval b >>= (fun boolResult -> if boolResult then stmntEval s1 else stmntEval s2) >>>= pop
        | While (b, s) -> push >>>= boolEval b >>= (fun boolResult -> if boolResult then stmntEval s >>>= stmntEval (While (b, s)) else ret ()) >>>= pop //We need to evaluate the while statement - don't know if the original s should be there though

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let binop2 f a b =
        prog {
            let! x = a
            let! y = b
            return f x y
        }
    
    let toUpper c = c >>= fun x -> ret (Char.ToUpper x)
    let toLower c = c >>= fun x -> ret (Char.ToLower x)
    let arithEval2 a =
        match a with
        | N n -> prog { return n }
        | V v -> prog { return! lookup v } //we use return! instead of return because lookup actually returns an SM already
        | WL -> prog { return! wordLength }
        | PV pv -> prog {
            let! a = arithEval pv
            return! pointValue a
            }
        | Add (a1, a2) -> binop2 ( + ) (arithEval a1) (arithEval a2) //the binop function uses prog
        | Sub (a1, a2) -> binop2 ( - ) (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> binop2 ( * ) (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> prog {
            let! x = arithEval a1
            let! y = arithEval a2
            if y <> 0 then return (x/y) else return! fail DivisionByZero
            } //I tried to use the div function, but this will do
        | Mod (a1, a2) -> prog {
            let! x = arithEval a1
            let! y = arithEval a2
            if y <> 0 then return (x/y) else return! fail DivisionByZero
            }
        | CharToInt c -> prog {
            let! r = charEval c
            return r |> int
            }
    let charEval2 c =
        match c with
        | C c  -> prog { return c }
        | CV c -> prog {
            let! a = (arithEval c)
            return! characterValue a
            }
        | ToUpper c -> prog {            
            return! toUpper (charEval c)
            }
        | ToLower c -> prog {
            return! toLower (charEval c)
            }
        | IntToChar a  -> prog {
            let! r = arithEval a
            return r |> char
            }
    let rec boolEval2 b =
        match b with
        | TT -> prog { return true }
        | FF -> prog { return false }
        | AEq (a1, a2) -> binop2 ( = ) (arithEval a1) (arithEval a2)
        | ALt (a1, a2) -> binop2 ( < ) (arithEval a1) (arithEval a2)
        | Not b -> prog {
            let! a = boolEval b
            return (a |> not)
            }
        | Conj (b1, b2) -> binop2 ( && ) (boolEval b1) (boolEval b2)
        | IsVowel c -> prog {
            let! a = charEval c
            return (a |> "aeiouAEIOUæøåÆØÅ".Contains)
            }
        | IsDigit c -> prog {
            let! a = charEval c
            return (a |> Char.IsDigit)
            }
        | IsLetter c -> prog {
            let! a = charEval c
            return (a |> Char.IsLetter)
            }
            
            
    let stmntEval2 stm =
        match stm with
        | Declare s -> prog { return! declare s }
        | Ass (x, a) -> prog {
            let! r = arithEval a
            return! update x r
            }
        | Skip -> prog { return () }
        | Seq (s1, s2) -> prog {
            do! stmntEval s1
            let! r = stmntEval s2
            return r
            }
        | ITE (b, s1, s2) -> prog {
            do! push
            let! r = boolEval b
            if r then do! stmntEval s1 else do! stmntEval s2
            }
        |While (b, s) -> prog {
            do! push
            let! r = boolEval b
            if r then
                do! stmntEval s
                do! stmntEval (While (b, s))
            else return ()
            do! pop
            }

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun (stm: stm) : squareFun =
        fun w pos acc ->
            evalSM (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"]) (stmntEval2 stm >>>= lookup "_result_")
    type square = Map<int, squareFun>

    type coord = int * int

    type boardFun = coord -> Result<square option, Error>
    
    type squareStmnt = Map<int, stm>
    
    let stmntsToSquare stms = Map.map (fun key value -> stmntToSquareFun value) stms

    let stmntToBoardFun stm (m: Map<int, 'a>) = fun (x,y) ->
        stmntEval2 stm >>>= lookup "_result_" |> evalSM (mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"]) |>
            function
                | Success res -> if m.ContainsKey res then Success (Some m.[res]) else Success None
                | Failure error -> Failure error

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt (ids: Map<int, squareStmnt>) : board =
        {
            center = c;
            defaultSquare = Map.find defaultSq ids |> stmntsToSquare;
            squares = stmntToBoardFun boardStmnt (Map.map (fun key elem -> stmntsToSquare elem) ids)
        }