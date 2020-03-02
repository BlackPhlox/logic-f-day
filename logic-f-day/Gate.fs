
module Gate
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

        static member ( !! )  (a)  = NOT(a)
        static member ( + )  (a,b) = OR(a,b)
        static member ( * )  (a,b) = AND(a,b)
        static member ( !* ) (a,b) = NAND(a,b)
        static member ( !+ ) (a,b) = NOR(a,b)
        static member ( *+ ) (a,b) = XOR(a,b)
        static member ( !*+ )(a,b) = XNOR(a,b)

        member g.represent =
            match g with 
            | NOT  _ -> "~"
            | OR   _ -> "+"  
            | AND  _ -> "*"  
            | NAND _ -> "~*" 
            | NOR  _ -> "~+" 
            | XOR  _ -> "*+" 
            | XNOR _ -> "~*+"
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

    let meh = !!(IN "A")
