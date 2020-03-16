module Eval

    open State
    open System

    (* Code for testing *)

    let hello = [('H',4);('E',1);('L',1);('L',1);('O',2)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []

    let binop f a b = a >>= fun x -> b >>= fun y -> ret(f x y) 
    
    let add a b = binop (+) a b   
    
    let divisor f a b =
        a >>= fun x ->
        b >>= fun y ->
        if y <> 0 then ret (f x y) else fail DivisionByZero

    let div a b = divisor (/) a b

    let modulo a b = divisor (%) a b

    let switch f x = ret (f x)

    let unop f a = a >>= switch f

    type aExp =
        | N of int
        | V of string
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let inline charToInt c = ret (int c)

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V x -> lookup x
        | Add (a1, a2) -> binop ( + ) (arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> binop ( - ) (arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> binop ( * ) (arithEval a1) (arithEval a2)
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (a1, a2) -> modulo (arithEval a1) (arithEval a2)
        | CharToInt c -> charEval c >>= fun a -> charToInt a

    and charEval c : SM<char> = 
        match c with
        | C x -> ret x 
        | ToLower x -> (charEval x >>= fun a -> ret (Char.ToLower(a)))
        | ToUpper x -> (charEval x >>= fun a -> ret (Char.ToUpper(a)))
        | IntToChar x -> (arithEval x >>= fun a -> ret (char a))

    let isVowel l = 
        match System.Char.ToLower(l) with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> true
        | _ -> false

    let isConsonant l = 
        if(System.Char.IsLetter(l) && not (isVowel l)) then true else false 

    let boolEval b : SM<bool> = 
        let rec aux = function
        | TT -> ret true
        | FF -> ret false
        | AEq (x,y) -> (binop (=) (arithEval x) (arithEval y))
        | ALt (x,y) -> (binop (<) (arithEval x) (arithEval y))
        | Not bx -> (unop (not) (aux bx))
        | Conj (bx,by) -> (binop (&&) (aux bx) (aux by))
        | IsVowel cx -> unop (isVowel) (charEval cx)
        | IsConsonant cx -> unop (isConsonant) (charEval cx)
        aux b
