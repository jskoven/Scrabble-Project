﻿// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy (System.Char.IsWhiteSpace) <?> "whitespace"
    let pletter        = asciiLetter
    let palphanumeric  = satisfy (System.Char.IsLetterOrDigit) <?> "alphanumeric"

    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' <?> "parenthesise"
    let braces p = pchar '{' >*>. p .>*> pchar '}'
    
    let pidaux (c: char, lst: char list) = List.fold (fun s ch -> s + (ch |> string)) (c |> string) lst 

    let pid = (pchar '_' <|> pletter) .>>. many (palphanumeric <|> pchar '_') |>> pidaux
    
    let unop op a = op >*>. a
    
    let binop op p1 p2 = p1 .>*> op .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CTermParse, cref = createParserForwardedToRef<cExp>()
    let BconjParse, bcref = createParserForwardedToRef<bExp>()
    let BequalParse, beref = createParserForwardedToRef<bExp>()
    let BatomParse, baref = createParserForwardedToRef<bExp>()
    let StatementParse, sref = createParserForwardedToRef<stm>()
    let SeqParse, seqref = createParserForwardedToRef<stm>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [DivParse; MulParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let VParse = pid |>> V <?> "V"
    
    //PVParse - we use unop, as we only have a single parser here. Same for NegParse. Our a value is AtomParse,
    // because we need to parse further
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> (N -1, x)) |>> Mul <?> "Neg"
    let CharToIntParse = pCharToInt >*>. parenthesise CTermParse |>> CharToInt <?> "CharToInt"
    let ParParse = parenthesise TermParse <?> "parenthesise"
    
    do aref := choice [NegParse; NParse; ParParse; PVParse; CharToIntParse; VParse]

    let AexpParse = TermParse 

    
    
    
    //We first need to read the ' chars using pchar and discard them, then we simply read anyChar 
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C"
    
    //We use AtomParse because the result should be a aExp?
    let CVParse = unop pCharValue AexpParse |>> CV <?> "CV"
    let CToUpperParse = pToUpper >*>. parenthesise CTermParse |>> ToUpper <?> "toUpper"
    let CToLowerParse = pToLower >*>. parenthesise CTermParse |>> ToLower <?> "toLower"
    
    let CIntToCharParse = pIntToChar >*>. parenthesise AexpParse |>> IntToChar <?> "IntToChar"
    
    do cref.Value <- choice [CParse; CToUpperParse; CToLowerParse; CVParse; CIntToCharParse]
    
    let CexpParse = CTermParse
    
    
    
    
    let BConj = binop (pstring "/\\") BequalParse BconjParse |>> Conj
    let BDisj = binop (pstring "\\/") BequalParse BconjParse |>> (fun (x,y) -> x.||.y)
    do bcref.Value <- choice [BConj; BDisj; BequalParse]
    
    let BEquals = binop (pchar '=') AexpParse AexpParse |>> AEq
    let BNotEquals = binop (pstring "<>") AexpParse AexpParse |>> (fun (x,y) -> x.<>.y)
    let BLessThan = binop (pchar '<') AexpParse AexpParse |>> ALt
    let BLessThanOrEqual = binop (pstring "<=") AexpParse AexpParse |>> (fun (x, y) -> x.<=.y)
    let BMoreThan = binop (pchar '>') AexpParse AexpParse |>> (fun (x, y) -> x.>.y)
    let BMoreThanOrEqual = binop (pstring ">=") AexpParse AexpParse |>> (fun (x, y) -> x.>=.y)
    do beref.Value <- choice [BEquals; BNotEquals; BLessThan; BLessThanOrEqual; BMoreThan; BMoreThanOrEqual; BatomParse]
    
    let BTrue = pTrue |>> (fun _ -> TT) <?> "TT" //Don't know why |>> TT wasn't enough I gotta be honest
    let BFalse = pFalse |>> (fun _ -> FF) <?> "FF"
    let BIsVowel = unop pIsVowel CTermParse |>> IsVowel <?> "IsVowel"
    let BIsDigit = unop pIsDigit CTermParse |>> IsDigit <?> "IsDigit"
    let BIsLetter = unop pIsLetter CTermParse |>> IsLetter <?> "IsLetter"
    let BNot = unop (pchar '~') BconjParse |>> Not <?> "Not"
    let BParParse = parenthesise BconjParse
    do baref.Value <- choice [BFalse; BIsVowel; BIsDigit; BIsVowel; BIsLetter; BNot; BParParse; BTrue]
    
    let BexpParse = BconjParse

    let SseqParse = binop (pchar ';') StatementParse SeqParse |>> Seq //...
    do seqref.Value <- choice [SseqParse; StatementParse]
        
    let SBracesParse = braces SeqParse //like parenthesise, but for {}
    let SDeclareParse = pdeclare >>. whitespaceChar >>. pid |>> Declare
    let SAssParse = binop (pstring ":=") pid AexpParse |>> Ass
    let SITEParse = pif >*>. BParParse .>*> pthen .>*>. SBracesParse .>*> pelse .>*>. SBracesParse |>> (fun ((b, s1), s2) -> ITE (b, s1, s2))
    let SITParse = pif >*>. BParParse .>*> pthen .>*>. SBracesParse |>> (fun (b, s) -> ITE (b, s, Skip)) //took a long time before I found the Skip hint...
    let SWhileParse = pwhile >*>. BParParse .>*> pdo .>*>. SBracesParse |>> While
    do sref.Value <- choice [SBracesParse; SDeclareParse; SAssParse; SITEParse; SITParse; SWhileParse]
    
    
    
    let stmntParse = SeqParse

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    let parseSquareProg sqp =
        sqp |> Map.map (fun key value -> run stmntParse value |> getSuccess |> stmntToSquareFun)

    let parseBoardProg s (sqp: Map<int, square>): boardFun =
       stmntToBoardFun (run stmntParse s |> getSuccess) sqp

    let mkBoard (bp : boardProg) : board =
        
        
        {
            center = bp.center;
            defaultSquare = Map.find bp.usedSquare bp.squares |> parseSquareProg
            squares =
                let m' = Map.map (fun _ m -> parseSquareProg m) bp.squares
                parseBoardProg bp.prog m'
        }
    
    
    
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    //let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
