module ScrabbleBot.Dict


type Dict =
    | Leaf of bool
    | Node of bool * Map<char, Dict>
        
let empty () = Leaf false

let rec insert (s:string) (dict:Dict) : Dict =
    match dict with
    | Leaf _ when s.Length = 0 -> Leaf true
    | Node (_, dictMap) when s.Length = 0 -> Node(true, dictMap)
        
        
    | Node (b, dictMap) ->
        match Map.tryFind s.[0] dictMap with
        | Some child ->
            let newModifiedNode = insert s.[1..] child
            let newMap = Map.add s.[0] newModifiedNode dictMap
            Node(b, newMap)
        | None ->
            let newModifiedNode = insert s.[1..] (empty ())
            let newMap = Map.add s.[0] newModifiedNode dictMap
            Node(b, newMap)
        
        
    | Leaf b ->
        let newModifiedNode = insert s.[1..] (empty ())
        let dictMap = Map.empty
        let newMap = Map.add s.[0] newModifiedNode dictMap
        Node(b, newMap)
        
        
//Old lookup that does not utilize step
(*let rec lookup (s:string) (dict:Dict) : bool =
    match dict with
    | Leaf b when s.Length = 0 -> b
    | Node (b, _) when s.Length = 0 -> b
    | Leaf _ -> false 
    | Node (_, dictMap) ->
        match Map.tryFind s.[0] dictMap with
        | None -> false
        | Some child -> lookup s.[1..] child*)
        
        
let step (c:char) (dict:Dict) : (bool * Dict) option =
    match dict with
    | Leaf _ -> None
    | Node (_, dictMap) ->
        match Map.tryFind c dictMap with
        | Some child ->
            match child with
            | Leaf b -> Some (b, child)
            | Node (b, _) -> Some (b, child)
        | None -> None
  
//Non-recursive lookup that utilizes the step function      
let lookup (s:string) (dict:Dict) : bool =
    let a acc elem =
               match acc with
               | Some (_, child) -> step elem child
               | None -> None
    let b = List.fold a (Some (false, dict)) (Seq.toList s)
    match b with
    | Some (b, _) -> b
    | None -> false
