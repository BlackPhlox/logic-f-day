//#load "C:/Users/thelu/source/repos/Logic-F-day/logic-f-day/PrintUtil.fs";;
//#load "C:/Users/thelu/source/repos/Logic-F-day/logic-f-day/Gate.fs";;

module Program
    //Load in interactive from here V
    open PrintUtil
    open Gate

    type gExpr = 
    | PRIM of gExp
    | IO   of gExp
    | GATE of gExp

    //To allow dynamic evaluation
    type gExpResult =
        | TYPE of gExp
        | EVAL of bool
        override g.ToString() =
            match g with
            | TYPE x -> x.ToString()
            | EVAL x -> x.ToString()
     // | OUTS of Map<string,gExpResult>

    //Infix operators (No precedence implemented currently)
    let (.|.)  a b = OR  (a, b)
    let (.-|.) a b = NOR (a, b)
    let (.*|.) a b = XOR (a, b)

    let (.&.)  a b = AND (a, b)
    let (.-&.) a b = NAND(a, b) 

    //Evaluation

    let getBoolB (g:gExp) : bool =
        match g with
        | B x -> x
    
    let getBool (b:gExpResult) : bool =
        match b with
        | EVAL x -> x

    let getgExp (b:gExpResult) : gExp =
        match b with
        | TYPE x -> x
        | EVAL x -> B x

    let gateCommutativeMap f gate x y st ee et :gExpResult =
        let a = (f x st)
        let b = (f y st)
        match a,b with
        | EVAL a, EVAL b -> ee a b
        | EVAL a, TYPE b -> et a b
        | TYPE a, EVAL b -> et b a
        | TYPE a, TYPE b -> TYPE (gate(a,b))

    let rec gateEval a st = 
        match a with
        | B x -> EVAL x
        | T x -> TYPE x
        | IN s ->  
            let r = Map.tryFind s st
            if r.IsSome then gateEval r.Value st else 
                printfn "The variable %s is not found, defaulting to type" s
                TYPE a
        | OUT (x,y) -> TYPE y
            //let r = Map.add x y st
            //OUTS (Map.add x (gateEval y r) Map.empty)
        | NOT x -> 
            let t = (gateEval x st)
            match t with
            | EVAL t -> EVAL(not t)
            | TYPE _ -> TYPE a
        | AND (x,y) -> 
            gateCommutativeMap gateEval AND x y st 
                (fun a b -> if a && b then EVAL true else EVAL false)
                (fun a b -> if a then TYPE b else EVAL false)
        | OR (x,y)  -> 
            gateCommutativeMap gateEval OR x y st 
                (fun a b -> if a || b then EVAL true else EVAL false)
                (fun a b -> if a then EVAL true else TYPE b)
        | XOR (x,y) -> 
            gateCommutativeMap gateEval XOR x y st 
                (fun a b -> if a <> b then EVAL true else EVAL false)
                (fun a b -> if a then TYPE(NOT b) else TYPE b)
        | NAND (x,y) -> gateEval (NOT (AND(x,y))) st
        | NOR  (x,y) -> gateEval (NOT (OR (x,y))) st
        | XNOR (x,y) -> gateEval (NOT (XOR(x,y))) st
    //Simplify / Reduction

    //Know which gates are not allowed to be used, using map of gate types?

    let sFind g st f fn =
        let r = Map.tryFind g st
        if r.IsSome then f else fn
    
    //How to do commutative?
    //Currently not correct
    
    let gateSimplify g = 
        let mutable st = Map.empty
        let rec aux g st =
            match g with
            | B x -> B x
            (*
            | IN x -> 
                let r = Map.tryFind x st
                if r.IsSome then aux r.Value st else aux (IN x) (Map.add x (B true) st)
            *)
            | NOT a ->
                let x = aux a st
                match x with 
                | a when a = NOT a -> a
                | B a when a = false -> B true
                | B a when a = true  -> B false
                | a -> NOT a
            | AND(a,b) ->
                let x = aux a st
                let y = aux b st
                match (x,y) with
                | x,y when x = y -> x
                | x,y when NOT(x) = y || NOT(y) = x -> B false
                | x1,OR(x2,y2) when x1 = x2 || x1 = y2 -> x1
                | OR(x1,y1),OR(x2,(NOT y2)) when x1=x2 && y1 = y2 -> x1
                | x1,OR (NOT x2, y1) when x1 = x2 -> AND(x1,y1)
                | x,y -> //Identity Law
                    let p = (gateEval x st)
                    let q = (gateEval y st)
                    match (p,q) with
                    | EVAL p, EVAL q -> B (p && q)
                    | TYPE _, EVAL q when q = false -> B false
                    | TYPE p, EVAL q when q = true  -> p 
                    | EVAL p, TYPE _ when p = false -> B false
                    | EVAL p, TYPE q when p = true  -> q
                    | p , q -> AND (getgExp p, getgExp q)
            | OR(a,b) ->
                let x = aux a st
                let y = aux b st
                match (x,y) with
                | x,y when x = y -> x
                | x,y when NOT(x) = y || NOT(y) = x -> B true
                | x,y when B true = x || B true = y -> B true
                | x1,AND(x2,y2) when x1 = x2 || x1 = y2 -> x1
                | AND(x1,y1),AND(x2,(NOT y2)) when x1=x2 && y1 = y2 -> x1
                | x1,AND (NOT x2, y1) when x1 = x2 -> OR(x1,y1)
                | x,y -> //Identity Law
                    let q = (gateEval x st)
                    let p = (gateEval y st)
                    match q,p with
                    | EVAL q, EVAL p -> B (q || p)
                    | TYPE q, EVAL p when p = false -> q 
                    | TYPE _, EVAL p when p = true  -> B true 
                    | EVAL q, TYPE p when q = false -> p
                    | EVAL q, TYPE _ when q = true  -> B true 
                    | q, p -> OR (getgExp q, getgExp p)
            | XOR (a,b) -> 
                let x = aux a st
                let y = aux b st
                match (x,y) with
                | x,y when NOT(x) = y || NOT(y) = x -> B true
                | x1,OR(x2,y2) when x1 = x2 -> aux (AND(NOT(x1),y2)) st
                | x,y -> //Identity Law
                    let q = (gateEval x st)
                    let p = (gateEval y st)
                    match q,p with
                    | EVAL q, EVAL p -> B (q <> p)
                    | TYPE q, EVAL p -> if p then (NOT q) else q
                    | EVAL q, TYPE p -> if q then (NOT p) else p
                    | q, p -> OR (getgExp q, getgExp p)
            | NAND(x,y) -> aux (NOT (AND(x,y))) st
            | NOR(x,y)  -> aux (NOT (OR(x,y))) st
            | x -> g        
        aux g st
    
    //Based from: https://en.wikipedia.org/wiki/NAND_logic#Making_other_gates_by_using_NAND_gates
    let nandGateify g =
        let rec aux g =
            match g with
            | B x -> B x
            | IN s -> IN s
            | NOT(AND(x,y)) -> NAND(aux x, aux y)
            | OUT (s,x) -> OUT (s,x) 
            | T x -> T x
            | NAND(x,y) -> NAND((aux x),(aux y))
            | NOT(x)    -> 
                let xR = aux x
                NAND(xR,xR)
            | NOR(x,y)  -> 
                let mid = NAND(aux (NOT x),aux (NOT y))
                aux (NAND(mid,mid))
            | OR(x,y)   -> NAND(aux (NOT x),aux (NOT y))
            | AND(x,y)  -> 
                let mid = NAND(aux x, aux y)
                aux (mid .-&. mid)
            | XOR(x,y)  -> 
                let mid = x .-&. y
                let ma = mid .-&. x
                let ba = mid .-&. y
                aux (ma .-&. ba)
            | XNOR(x,y) -> 
                let mida = x .-&. x
                let midb = y .-&. y
                let midrc = x .-&. y
                let midic = mida .-&. midb
                aux (midrc .-&. midic)
        aux g

    let nandGateSimplify g =
        nandGateify g |> gateSimplify

    let rec restrictedGateSimplify g gates:gExp list =
        List.Empty    

    //Truthtable generation
    let IOList a =
        let rec aux a acc = 
            match a with
            | B x       ->  acc
            | T x       ->  aux x acc
            | IN(x)     ->  x :: aux (B false) acc
            | OUT(x,y)  ->  x :: aux y acc
            | NOT(x)    ->  aux x acc
            | AND(x,y)  ->  aux x (aux y acc)
            | NAND(x,y) ->  aux x (aux y acc)
            | OR(x,y)   ->  aux x (aux y acc)
            | NOR(x,y)  ->  aux x (aux y acc)
            | XOR(x,y)  ->  aux x (aux y acc)
            | XNOR(x,y) ->  aux x (aux y acc)
        aux a List.Empty

    let TruthTable g =
        let io = IOList g
        let bools = GenLists io
        let sts = [for bl in bools -> List.map2(fun x y -> (y, B x)) bl io]

        let results = List.map (fun x -> gateEval g (Map.ofList x)) sts

        let io = io @ ["O"]

        let bb = List.map (fun x -> getBool x) results

        let rows = List.map(fun j -> List.map (fun (x,y) -> if getBoolB y then "T" else "F" ) j ) sts 

        let newt = List.map2 (fun x b -> x@[(if b then "T" else "F")]) rows bb

        let com = io::newt
        printTable com