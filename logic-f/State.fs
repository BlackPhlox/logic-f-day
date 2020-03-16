module State

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string  

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)

    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)    
    
    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let updateStack k v l = 
        let rec aux first (stack:Map<string,int> list) =
            match stack with
                | [] -> []
                | x::xs when x.ContainsKey k && not first -> x.Add (k,v)::aux true xs
                | x::xs -> x::aux first xs
        aux false l

    let update (var : string) (value : int) : SM<unit> = 
        S (fun s -> 
            let li = List.tryFind(fun m -> (Map.containsKey var m)) s.vars
            match li with
            | None -> Failure(VarNotFound var)
            | Some x -> Success((),{s with vars = updateStack var value s.vars})
            )