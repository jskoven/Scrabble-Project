// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = M of Map<'a, uint32>
        
        let empty = M Map.empty
        let size (M s) = Map.fold(fun acc _ elem -> acc+ elem) 0u s
        let isEmpty (M s) = size (M s) = 0u
        let contains a (M s) = Map.containsKey a s
        
        let containsValue a (M s) =
            match Map.tryFind a s with
            |Some(0u) -> false
            |_ -> true
        let numItems a (M s) = Map.tryFind a s |> Option.defaultValue 0u
        
        let add a n (M s) = if contains a (M s) then M (Map.add a (n+(Map.find a s)) s) else M (Map.add a n s)
        let addSingle a (M s) = if contains a (M s) then M (Map.add a ((Map.find a s) + 1u) s) else M (Map.add a 1u s)
        let remove a n (M s) =
            match n with
            | _ when contains a (M s) = false -> M s
            | n when (Map.find a s) <= n -> M (Map.add a 0u s)
            | n -> M (Map.add a ((Map.find a s) - n) s)

        let removeSingle a (M s) = if contains a (M s) = false then M s
                                    elif (Map.find a s) > 0u then M (Map.add a (Map.find a s - 1u) s)
                                    else M s
                                    
        let fold f acc (M s) = Map.fold f acc s
        let foldBack f (M s) acc = Map.foldBack f s acc
        
        let ofList (lst:'a list) = List.fold(fun acc elem -> addSingle elem acc) empty lst
        
        //Sequence expression to construct list - from https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/lists
        let toList (M s) = fold (fun acc key elem -> acc @ [for _ in 1..(int elem) -> key]) [] (M s)
        
        let test = (toList (add 7 3u (add 5 6u (add 9 9u (add 27 2u empty)))))
        let map (f: 'a -> 'b) (M s) = fold (fun acc key elem -> add (f key) elem acc) empty (M s)
        
        let union (M s1) (M s2) : MultiSet<'a> = fold (fun acc key elem ->
            if elem > numItems key acc then add key elem acc else acc) (M s1) (M s2)
        
        let sum (M s1) (M s2) : MultiSet<'a> = fold (fun acc key elem -> add key elem acc) (M s1) (M s2)
        
        let subtract (M s1) (M s2) : MultiSet<'a> = fold (fun acc key elem -> remove key elem acc) (M s1) (M s2)
                      
        let intersection (M s1) (M s2) : MultiSet<'a> = fold (fun acc key elem -> if contains key (M s1) && contains key (M s2) then add key elem acc else acc) empty (M s2)
