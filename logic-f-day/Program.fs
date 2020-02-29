//#load "C:/Users/thelu/source/repos/Logic-F-day/logic-f-day/PrintUtil.fs";;

module Program =
    //Load in interactive from here V
    open PrintUtil

    type gExp =
        //Primitive
        | B of bool
        
        //Type def.
        | T    of gExp

        //IO
        | IN  of string
        | OUT of string * gExp

        //Gates
        | NOT  of gExp
        | NAND of (gExp * gExp)
        | AND  of (gExp * gExp)
        | OR   of (gExp * gExp)
        | NOR  of (gExp * gExp)
        | XOR  of (gExp * gExp)
        | XNOR of (gExp * gExp)

        member g.represent =
            match g with 
            | NOT  _ -> "~"
            | AND  _ -> "*"  
            | NAND _ -> "~*" 
            | OR   _ -> "+"  
            | NOR  _ -> "~+" 
            | XOR  _ -> "x+" 
            | XNOR _ -> "x~+"
            | _ -> "No representation"

        override g.ToString() =
            let rec aux g =
                match g with 
                //Prim
                | B x        -> x.ToString()
                | T x        -> x.ToString()
                //IO
                | IN   (x)   -> x
                | OUT  (x,y) -> x  + "=" + aux (y)
                //Gates
                | NOT  (x)   -> g.represent + aux(x)
                | AND  (x,y) -> "("+ aux(x) + g.represent + aux(y)+")"
                | NAND (x,y) -> "("+ aux(x) + g.represent + aux(y)+")"
                | OR   (x,y) -> "("+ aux(x) + g.represent + aux(y)+")"
                | NOR  (x,y) -> "("+ aux(x) + g.represent + aux(y)+")"
                | XOR  (x,y) -> "("+ aux(x) + g.represent + aux(y)+")"
                | XNOR (x,y) -> "("+ aux(x) + g.represent + aux(y)+")"
            aux g                             

        member g.PrintTree() = 
            let rec aux g =
                match g with
                //Prim
                | B   (x)    -> Node (Nil   , (if x then "T" else "F") , Nil  )
                //IO
                | IN  (x)    -> Node (Nil   , x           , Nil  )
                | OUT (x,y)  -> Node (Nil   , x           , aux y)
                //Gates
                | NOT (a)    -> Node (Nil   , g.represent , aux a)
                | AND (a,b)  -> Node (aux a , g.represent , aux b)
                | NAND(a,b)  -> Node (aux a , g.represent , aux b)
                | OR  (a,b)  -> Node (aux a , g.represent , aux b)
                | NOR (a,b)  -> Node (aux a , g.represent , aux b)
                | XOR (a,b)  -> Node (aux a , g.represent , aux b)
                | XNOR(a,b)  -> Node (aux a , g.represent , aux b)
            PrintTree (aux g) 

        //member g.TruthTable() = 

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

    //Tree Tests

    let type01 = OUT("O1", OUT("O2",((IN "A") .&. B true)) .&. (IN "B"))
    printfn "%A" (type01.ToString())
    type01.PrintTree()

    let type02 = (B true) .&. (B false)
    printfn "%A" (type02.ToString())
    type02.PrintTree()

    let type03 = ((IN "A") .*|. (IN "A")) .&. (IN "B")
    printfn "%A" (type03.ToString())
    type03.PrintTree()


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

    let gateMap f gate x y st ee et te :gExpResult =
        let a = (f x st)
        let b = (f y st)
        match a,b with
        | EVAL a, EVAL b -> ee a b
        | EVAL a, TYPE b -> et a b
        | TYPE a, EVAL b -> te a b
        | TYPE a, TYPE b -> TYPE (gate(a,b))

    let rec gateEval a st = 
        match a with
        | B x -> EVAL x
        | T x -> TYPE x
        | IN s ->  
            let r = Map.tryFind s st
            if r.IsSome then gateEval r.Value st else TYPE a
        | OUT (x,y) -> TYPE y
            //let r = Map.add x y st
            //OUTS (Map.add x (gateEval y r) Map.empty)
        | NOT x -> 
            let t = (gateEval x st)
            match t with
            | EVAL t -> EVAL(not t)
            | TYPE _ -> TYPE a
        | AND (x,y) -> 
            gateMap gateEval OR x y st 
                (fun a b -> if a && b then EVAL true else EVAL false)
                (fun a b -> if a then TYPE b else EVAL false)
                (fun a b -> if b then TYPE a else EVAL false)
        | OR (x,y)  -> 
            gateMap gateEval OR x y st 
                (fun a b -> if a || b then EVAL true else EVAL false)
                (fun a b -> if a then EVAL true else TYPE b)
                (fun a b -> if b then EVAL true else TYPE a)
        | XOR (x,y) -> 
            gateMap gateEval XOR x y st 
                (fun a b -> if a <> b then EVAL true else EVAL false)
                (fun a b -> if a then TYPE(NOT b) else TYPE b)
                (fun a b -> if b then TYPE(NOT a) else TYPE a)
        | NAND (x,y) -> gateEval (NOT (AND(x,y))) st
        | NOR  (x,y) -> gateEval (NOT (OR (x,y))) st
        | XNOR (x,y) -> gateEval (NOT (XOR(x,y))) st

    //Simplify / Reduction

    //Know which gates are not allowed to be used, using map of gate types?

    //How to do commutative?
    let rec gateSimplify g =
        match g with
        //Redundancy Laws

        //Absorption
        | AND(x1,OR (x2,_)) when x1 = x2 -> gateSimplify x1
        | OR (x1,AND(x2,_)) when x1 = x2 -> gateSimplify x1

        | AND(OR (x1,y1),OR (x2,NOT y2))  when x1 = x2 && y1 = y2 -> gateSimplify x1
        | OR (AND(x1,y1),AND(x2,NOT y2)) when x1 = x2 && y1 = y2 -> gateSimplify x1

        | AND(x1,OR (NOT x2, y1)) when x1 = x2 -> AND(gateSimplify x1, gateSimplify y1)
        | OR (x1,AND(NOT x2, y1)) when x1 = x2 -> OR(gateSimplify x1, gateSimplify y1)

        //Idempotent Law
        | AND(x,y) when x = y -> x
        | OR (x,y) when x = y -> x

        | AND(NOT(x1),x2) when x1 = x2 -> B false
        | OR (NOT(x1),x2) when x1 = x2 -> B true

        //Identity Law
        | AND(x,y) -> 
            let a = (gateEval x Map.empty)
            let b = (gateEval y Map.empty)
            match a,b with
            | EVAL _, EVAL _ -> B (getBool(gateEval g Map.empty))
            | TYPE _, EVAL b when b = false -> B false
            | TYPE a, EVAL b when b = true -> a 
            | EVAL a, TYPE _ when a = false -> B false
            | EVAL a, TYPE b when a = true -> b 
            | a , b -> AND (getgExp a, getgExp b)

        | OR(x,y) -> 
            let a = (gateEval x Map.empty)
            let b = (gateEval y Map.empty)
            match a,b with
            | EVAL _, EVAL _ -> B (getBool(gateEval g Map.empty))
            | TYPE a, EVAL b when b = false -> a 
            | TYPE _, EVAL b when b = true  -> B true 
            | EVAL a, TYPE b when a = false -> b
            | EVAL a, TYPE _ when a = true  -> B true 
            | a, b -> OR (getgExp a, getgExp b)
        
        //Complement Law
        | AND(x, NOT y) when x = y -> B false
        | OR (x, NOT y) when x = y -> B false

        | NOT(B x) when x = false -> B true
        | NOT(B x) when x = true -> B false
        
        | NOT(NOT x)     -> gateSimplify x                 //Involution Law
        | NOT(AND(x,y))  -> gateSimplify (NOT x .|. NOT y) //DeMorgan’s First Law 
        | NOT(OR (x,y))  -> gateSimplify (NOT x .&. NOT y) //DeMorgan’s Second Law
        | _ -> g

    let rec nandGateSimplify g =
        match g with 
        | NAND(x,y) -> nandGateSimplify x .-&. nandGateSimplify y
        | NOT(x)    -> nandGateSimplify (x .-&. x)
        | OR(x,y)   -> nandGateSimplify (NOT x .-&. NOT y)
        | NOR(x,y)  -> 
            let mid = NOT x .-&. NOT y
            nandGateSimplify (gateSimplify mid .-&. mid)
        | AND(x,y)  -> 
            let mid = x .&. y
            nandGateSimplify (gateSimplify mid .-&. mid)
        | XOR(x,y)  -> 
            let mid = x .-&. y
            let ma = mid .-&. x
            let ba = mid .-&. y
            nandGateSimplify (ma .-&. ba)
        | g         -> gateSimplify g

    let rec restrictedGateSimplify g gates:gExp list =
        List.Empty    

    //Truthtable generation
    let IOList a =
        let rec aux a acc = 
            match a with
            | B x -> acc
            | IN(x) -> x :: aux (B false) acc
            | NOT(x)  ->  aux x acc
            | AND(x,y)  ->  aux x (aux y acc)
            | NAND(x,y) ->  aux x (aux y acc)
            | OR(x,y)   ->  aux x (aux y acc)
            | NOR(x,y)  ->  aux x (aux y acc)
            | XOR(x,y)  ->  aux x (aux y acc)
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


    //Tests

    let st = Map.ofList [("A", B true)]

    let v1 = gateEval (NOT (B true)) st
    let v2 = gateEval (NOT (IN("B"))) st
    let v3 = gateEval (NOT (AND(IN("A"),IN("B")))) st
    let v4 = gateEval (OUT("O",NOT (B true)).|.(AND(IN("A"),IN("B")))) st

    let gs01 = gateSimplify (NOT (NOT(IN("B"))))
    let gs02 = gateSimplify (NOT (AND(IN("A"),IN("B"))))

    let gs03 = nandGateSimplify (gateSimplify (NOT (AND(IN("A"),IN("B")))))
    
    let gs04 = gateSimplify(OR(IN "A",IN "A"))

    let gs05 = gateSimplify(AND(IN "x",OR(IN "x",IN "B")))

    //Test Simplify rules

    //Identity
    let sr_i00 = "Identity"
    let sr_i01 = string (gateSimplify(IN "A" .&. IN "A"))//A
    let sr_i02 = string (gateSimplify(IN "A" .|. IN "A"))//A
    let sr_i03 = string (gateSimplify((IN "A" .&. IN "B") .|. (IN "A" .&. (NOT (IN "B"))))) //A //Precedence mistake(No pre.)
    let sr_i04 = string (gateSimplify((IN "A" .|. IN "B") .&. (IN "A" .|. (NOT (IN "B"))))) //A

    //Redundancy
    let sr_r00 = "Redundancy"
    let sr_r01 = string (gateSimplify((IN "A" .&. (IN "A" .|. IN "B"))))    //A
    let sr_r02 = string (gateSimplify((IN "A" .|. (IN "A" .&. IN "B"))))    //A
    let sr_r03 = string (gateSimplify( B false .&. IN "A"))                 //False
    let sr_r04 = string (gateSimplify( B false .|. IN "A"))                 //A
    let sr_r05 = string (gateSimplify( B true  .&. IN "A"))                 //A
    let sr_r06 = string (gateSimplify( B true  .|. IN "A"))                 //True
    let sr_r07 = string (gateSimplify( NOT(IN "A") .&. IN "A"))             //False
    let sr_r071= string (gateSimplify( IN "A" .&. NOT(IN "A")))             //False
    let sr_r08 = string (gateSimplify( NOT(IN "A") .|. IN "A"))             //True
    let sr_r081= string (gateSimplify( IN "A" .|. NOT (IN "A")))            //True
    let sr_r09 = string (gateSimplify( IN "A" .&. NOT(IN "A")))             //A AND B
    let sr_r10 = string (gateSimplify( IN "A" .|. (NOT(IN "A") .&. IN "B")))//A OR  B

    let gs1 = nandGateSimplify (gateSimplify ((NOT (IN "A").-&. IN "B") .*|. NOT (IN "A" .|. NOT(IN "A"))))

    let p1 = string gs1

    let e1 = gateEval gs1 st

    printfn "%A" (gs1)
    gs1.PrintTree()

    let ft = NOT (IN "B") .-|. IN "A"
    ft.PrintTree()
    let ou2 = gateSimplify ft
    let out = gateEval ft st

    gs1.PrintTree();


    let t01 = (NOT (B false) .&. (B true) .-&. (IN "A" .-|. IN "B"))
    t01.ToString()
    let t01simp = gateSimplify t01
    t01.PrintTree()
    TruthTable t01
    t01simp.PrintTree()
        