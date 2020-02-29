//#load "C:/Users/thelu/source/repos/Logic-F-day/logic-f-day/PrintUtil.fs";;

module Program =
    open System
    open PrintUtil

    type gExp =
        //Primitive
        | B of bool
        
        //Type def.
        | T    of gExp

        //IO
        | IN of string
        | OUT of string * gExp

        //Gates
        | NOT  of gExp
        | NAND of (gExp * gExp)
        | AND  of (gExp * gExp)
        | OR   of (gExp * gExp)
        | NOR  of (gExp * gExp)
        | XOR  of (gExp * gExp)
    
    //For dynamic evaluation
    type gExpResult =
        | TYPE of gExp
        | EVAL of bool
     // | OUTS of Map<string,gExpResult>

    //Infix operators (No precedence implemented currently)
    let (.|.)  a b = OR  (a, b)
    let (.-|.) a b = NOR (a, b)
    let (.*|.) a b = XOR (a, b)

    let (.&.)  a b = AND (a, b)
    let (.-&.) a b = NAND(a, b)

    let gateString g =
        let rec aux g =
            match g with 
            //Prim
            | B x        -> x.ToString()
            //IO
            | IN   (x)   -> x
            | OUT  (x,y) -> x  + "=" + aux (y)
            //Gates
            | NOT  (x)   -> "~"+ aux(x)
            | AND  (x,y) -> "("+ aux(x) + " * "  + aux(y)+")"
            | NAND (x,y) -> "("+ aux(x) + " ~* " + aux(y)+")"
            | OR   (x,y) -> "("+ aux(x) + " + "  + aux(y)+")"
            | NOR  (x,y) -> "("+ aux(x) + " ~+ " + aux(y)+")"
            | XOR  (x,y) -> "("+ aux(x) + " x+ " + aux(y)+")"
        aux g

    let gateTree g =
        let rec aux g =
            match g with
            //Prim
            | B   (x)    -> Node (Nil   , (if x then "T" else "F") , Nil  )
            //IO
            | IN  (x)    -> Node (Nil   , x            , Nil  )
            | OUT (x,y)  -> Node (Nil   , x            , aux y)
            //Gates
            | NOT (a)    -> Node (Nil   , "~"          , aux a)
            | AND (a,b)  -> Node (aux a , "*"          , aux b)
            | NAND(a,b)  -> Node (aux a , "~*"         , aux b)
            | OR  (a,b)  -> Node (aux a , "+"          , aux b)
            | NOR (a,b)  -> Node (aux a , "~+"         , aux b)
            | XOR (a,b)  -> Node (aux a , "x+"         , aux b)
        aux g

    let PrintGateTree g = PrintTree (gateTree(g))  

    //Tree Tests

    let type01 = OUT("O1", OUT("O2",((IN "A") .&. B true)) .&. (IN "B"))
    printfn "%A" (gateString type01)
    PrintTree (gateTree(type01))

    let type02 = (B true) .&. (B false)
    printfn "%A" (gateString type02)
    PrintTree (gateTree(type02))

    let type03 = ((IN "A") .*|. (IN "A")) .&. (IN "B")
    printfn "%A" (gateString type03)
    PrintTree (gateTree(type03))


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

    (*
    let getgExp (b:gExpResult) : gExp =
        match b with
        | OUTS x ->
    *)

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
            let a = (gateEval x st)
            let b = (gateEval y st)
            match a,b with
            | EVAL a, EVAL b -> if a && b then EVAL true else EVAL false
            | TYPE a, TYPE b -> TYPE (AND(a,b))
            | EVAL a, TYPE b -> TYPE (AND(B a,b))
            | TYPE a, EVAL b -> TYPE (AND(a,B b))
        | OR (x,y)  -> 
            let a = (gateEval x st)
            let b = (gateEval y st)
            match a,b with
            | EVAL a, EVAL b -> if a || b then EVAL true else EVAL false
            | TYPE a, TYPE b -> TYPE (AND(a,b))
            | EVAL a, TYPE b -> TYPE (AND(B a,b))
            | TYPE a, EVAL b -> TYPE (AND(a,B b))
        | NAND (x,y)-> gateEval (NOT (AND(x,y))) st
        | NOR (x,y) -> gateEval (NOT (OR(x,y))) st
        | x -> TYPE x


    let st = Map.ofList [("A", B true)]

    let v1 = gateEval (NOT (B true)) st
    let v2 = gateEval (NOT (IN("B"))) st
    let v3 = gateEval (NOT (AND(IN("A"),IN("B")))) st
    let v4 = gateEval (OUT("O",NOT (B true)).|.(AND(IN("A"),IN("B")))) st

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

    let rec print g =
        match g with
            | B x -> x.ToString()
            | IN (x) -> x
            | NOT (x) -> "~" + print(x)
            | AND (x,y) ->  "("+print(x) + " * "  + print(y)+")"
            | NAND (x,y) -> "("+print(x) + " ~* " + print(y)+")"
            | OR (x,y) ->   "("+print(x) + " + "   + print(y)+")"
            | NOR (x,y) ->  "("+print(x) + " ~+ "   + print(y)+")"
            | t -> t.ToString() 
    

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

    let gs01 = gateSimplify (NOT (NOT(IN("B"))))
    let gs02 = gateSimplify (NOT (AND(IN("A"),IN("B"))))

    let gs03 = nandGateSimplify (gateSimplify (NOT (AND(IN("A"),IN("B")))))
    
    let gs04 = gateSimplify(OR(IN "A",IN "A"))

    let gs05 = gateSimplify(AND(IN "x",OR(IN "x",IN "B")))

    //Test Simplify rules

    //Identity
    let sr_i00 = "Identity"
    let sr_i01 = print (gateSimplify(IN "A" .&. IN "A"))//A
    let sr_i02 = print (gateSimplify(IN "A" .|. IN "A"))//A
    let sr_i03 = print (gateSimplify((IN "A" .&. IN "B") .|. (IN "A" .&. (NOT (IN "B"))))) //A //Precedence mistake(No pre.)
    let sr_i04 = print (gateSimplify((IN "A" .|. IN "B") .&. (IN "A" .|. (NOT (IN "B"))))) //A

    //Redundancy
    let sr_r00 = "Redundancy"
    let sr_r01 = print (gateSimplify((IN "A" .&. (IN "A" .|. IN "B"))))    //A
    let sr_r02 = print (gateSimplify((IN "A" .|. (IN "A" .&. IN "B"))))    //A
    let sr_r03 = print (gateSimplify( B false .&. IN "A"))                 //False
    let sr_r04 = print (gateSimplify( B false .|. IN "A"))                 //A
    let sr_r05 = print (gateSimplify( B true  .&. IN "A"))                 //A
    let sr_r06 = print (gateSimplify( B true  .|. IN "A"))                 //True
    let sr_r07 = print (gateSimplify( NOT(IN "A") .&. IN "A"))             //False
    let sr_r071= print (gateSimplify( IN "A" .&. NOT(IN "A")))             //False
    let sr_r08 = print (gateSimplify( NOT(IN "A") .|. IN "A"))             //True
    let sr_r081= print (gateSimplify( IN "A" .|. NOT (IN "A")))            //True
    let sr_r09 = print (gateSimplify( IN "A" .&. NOT(IN "A")))             //A AND B
    let sr_r10 = print (gateSimplify( IN "A" .|. (NOT(IN "A") .&. IN "B")))//A OR  B

    let gs1 = nandGateSimplify (gateSimplify ((NOT (IN "A").-&. IN "B") .*|. NOT (IN "A" .|. NOT(IN "A"))))

    let p1 = print gs1

    let e1 = gateEval gs1 st

    printfn "%A" (gateString gs1)
    PrintTree (gateTree(gs1))

    let ft = NOT (IN "B") .-|. IN "A"
    PrintTree (gateTree(ft))
    let ou2 = gateSimplify ft
    let out = gateEval ft st

    PrintGateTree (gs1)


    let t01 = (NOT (B false) .&. (B true) .-&. (IN "A" .-|. IN "B"))
    gateString t01
    let t01simp = gateSimplify t01
    PrintGateTree t01
    TruthTable t01
    PrintGateTree t01simp
        