(*
Gate.fsi
module GateT
    [<Sealed>]
    type gExp = 
        static member ( !! ) : gExp        -> gExp
        static member ( + )  : gExp * gExp -> gExp
        static member ( * )  : gExp * gExp -> gExp
        static member ( !* ) : gExp * gExp -> gExp
        static member ( !+ ) : gExp * gExp -> gExp
        static member ( *+ ) : gExp * gExp -> gExp
        static member ( !*+ ): gExp * gExp -> gExp

        member PrintTree : unit -> unit
*)
module Gate
    open PrintUtil

    type gPrim =
        | B of bool
        | IO of gIO

    and gIO = 
        | IN  of string
        | OUT of string * gExp
        
    and gExp =
        | P of gPrim
        | G of gate

        member g.represent =
            match g with
            | P x -> 
                match x with 
                | B a -> if a then "T" else "F"
                | _ -> ""
            | G x -> 
                match x with
                | NOT  _ -> "~"
                | OR   _ -> "+"  
                | AND  _ -> "*"  
                | NAND _ -> "~*" 
                | NOR  _ -> "~+" 
                | XOR  _ -> "*+" 
                | XNOR _ -> "~*+"
        
        static member ( !! )  (a)  = G (NOT(a))
        static member ( + )  (a,b) = G (OR(a,b))
        static member ( * )  (a,b) = G (AND(a,b))
        static member ( !* ) (a,b) = G (NAND(a,b))
        static member ( !+ ) (a,b) = G (NOR(a,b))
        static member ( *+ ) (a,b) = G (XOR(a,b))
        static member ( !*+ )(a,b) = G (XNOR(a,b)) 

        override g.ToString() =
            let rec aux g =
                match g with 
                | P x -> match x with
                         | IO y -> match y with
                                   | IN   (x)   -> x
                                   | OUT  (x,y) -> x  + "=" + aux (y)
                         | _ -> g.represent
                | G a -> 
                    match a with
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
                | P x -> 
                    match x with
                    | B x  -> Node (Nil, (if x then "T" else "F") , Nil)
                    | IO y -> match y with
                            | IN x       -> Node (Nil, x, Nil)
                            | OUT (x,y)  -> Node (Nil, x, aux y)
                | G x -> 
                    match x with
                    | NOT (a)    -> Node (Nil   , g.represent , aux a)
                    | AND (a,b)  -> Node (aux a , g.represent , aux b)
                    | NAND(a,b)  -> Node (aux a , g.represent , aux b)
                    | OR  (a,b)  -> Node (aux a , g.represent , aux b)
                    | NOR (a,b)  -> Node (aux a , g.represent , aux b)
                    | XOR (a,b)  -> Node (aux a , g.represent , aux b)
                    | XNOR(a,b)  -> Node (aux a , g.represent , aux b)
            PrintTree (aux g) 

      //member g.TruthTable() =

    and gate =
        | NOT  of gExp
        | NAND of (gExp * gExp)
        | AND  of (gExp * gExp)
        | OR   of (gExp * gExp)
        | NOR  of (gExp * gExp)
        | XOR  of (gExp * gExp)
        | XNOR of (gExp * gExp)