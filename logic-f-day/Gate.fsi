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