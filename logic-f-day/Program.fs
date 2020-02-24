    open System


    type ins = (string * bool) list

    type gExp =
        | B of bool
        | T of gExp
        | IN of string
        | OUT
        | NOT of gExp
        | NAND of (gExp * gExp)
        | AND of (gExp * gExp)
        | OR of (gExp * gExp)
        | NOR of (gExp * gExp)
        | XOR of (gExp * gExp)
    
    type gExpResult =
        | TYPE of gExp
        | EVAL of bool

    let (.|.) a b = OR (a, b)
    let (.&.) a b = AND (a, b)
    let (.-&.) a b = NAND (a, b)

    let rec gateEval a st = 
        match a with
        | B x -> EVAL x
        | T x -> TYPE x
        | IN s ->  
            let r = Map.tryFind s st
            if r.IsSome then gateEval r.Value st else TYPE a
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

    let getBool (b:gExpResult) : bool =
        match b with
        | EVAL x -> x
    
    let getgExp (b:gExpResult) : gExp =
        match b with
        | TYPE x -> x

    let rec gateSimplify g =
        match g with
        //Redundancy Laws

        //Absorption
        | OR(x1,AND(x2,_)) when x1 = x2 -> gateSimplify x1
        | AND(x1,OR(x2,_)) when x1 = x2 -> gateSimplify x1

        | OR(AND(x1,y1),AND(x2,NOT y2)) when x1 = x2 && y1 = y2 -> gateSimplify x1
        | AND(OR(x1,y1),OR(x2,NOT y2))  when x1 = x2 && y1 = y2 -> gateSimplify x1

        | OR(x1, AND(NOT x2,y1)) when x1 = x2 -> OR(gateSimplify x1, gateSimplify y1)
        | AND(x1,OR(NOT x2, y1)) when x1 = x2 -> AND(gateSimplify x1, gateSimplify y1)

        //Idempotent Law
        | AND(x,y) when x = y -> x
        | OR(x,y)  when x = y -> x

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
            | TYPE _, EVAL b when b = true -> B true 
            | EVAL a, TYPE b when a = false -> b
            | EVAL a, TYPE _ when a = true -> B true 
            | a , b -> OR (getgExp a, getgExp b)
        
        //Complement Law
        | AND(x, NOT y) when x = y -> B false
        | OR (x, NOT y) when x = y -> B false

        | NOT(B x) when x = false -> B true
        | NOT(B x) when x = true -> B false
        
        | NOT(NOT x)     -> gateSimplify x                 //Involution Law
        | NOT (AND(x,y)) -> gateSimplify (NOT x .|. NOT y) //DeMorgan’s First Law 
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

    let st = Map.ofList [("A", B true)]

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

    //let rec tt a = 
        

    //Tests
    let v1 = gateEval (NOT (B true)) st
    let v2 = gateEval (NOT (IN("B"))) st

    let gs01 = gateSimplify (NOT (NOT(IN("B"))))
    let gs02 = gateSimplify (NOT (AND(IN("A"),IN("B"))))

    let gs03 = nandGateSimplify (gateSimplify (NOT (AND(IN("A"),IN("B")))))
    
    let gs04 = gateSimplify(OR(IN "A",IN "A"))

    let gs05 = gateSimplify(AND(IN "x",OR(IN "x",IN "B")))


    //Test Simplify rules

    //Identity
    let sr_i01 = print (gateSimplify(IN "A" .&. IN "A"))//A
    let sr_i02 = print (gateSimplify(IN "A" .|. IN "A"))//A
    let sr_i03 = print (gateSimplify((IN "A" .&. IN "B") .|. (IN "A" .&. (NOT (IN "B"))))) //A //Precedence mistake(No pre.)
    let sr_i04 = print (gateSimplify((IN "A" .|. IN "B") .&. (IN "A" .|. (NOT (IN "B"))))) //A

    //Redundancy
    let sr_r01 = print (gateSimplify(AND(IN "A",OR(IN "A",IN "B"))))      //A
    let sr_r02 = print (gateSimplify(OR(IN "A",AND(IN "A",IN "B"))))      //A
    let sr_r03 = print (gateSimplify(AND(B false,IN "A")))                //False
    let sr_r04 = print (gateSimplify(OR (B false,IN "A")))                //A
    let sr_r05 = print (gateSimplify(AND (B true,IN "A")))                //A
    let sr_r06 = print (gateSimplify(OR  (B true,IN "A")))                //True
    let sr_r07 = print (gateSimplify(AND (NOT(IN "A"),IN "A")))           //False
    let sr_r08 = print (gateSimplify(OR  (NOT(IN "A"),IN "A")))           //True
    let sr_r09 = print (gateSimplify(AND(IN "A",  NOT(IN "A"))))          //A AND B
    let sr_r10 = print (gateSimplify(OR(IN "A", AND(NOT(IN "A"),IN "B"))))//A OR  B

    let gs1 = nandGateSimplify (gateSimplify (XOR(NAND(NOT (IN "A"), IN "B"),NOT (OR (IN "A",NOT(IN "A"))))))

    let p1 = print gs1

    let e1 = gateEval gs1 st