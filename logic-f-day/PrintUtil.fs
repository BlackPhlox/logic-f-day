module PrintUtil
    open System
    type 'a tree =    
    | Node of 'a tree * 'a * 'a tree
    | Nil

    let rec PrettyAndWidthInfo t =
        match t with
        | Nil -> 
            [], 0, 0, 0
        | Node(Nil,d,Nil) -> 
            let s = d.ToString()
            [s], s.Length, 0, s.Length-1
        | Node(l,d,r) ->
            // compute info for string of this node's data
            let s = d.ToString()
            let sw = s.Length
            let swl = sw/2
            let swr = (sw-1)/2
            assert(swl+1+swr = sw)  
            // recurse
            let lp,lw,_,lc = PrettyAndWidthInfo l
            let rp,rw,rc,_ = PrettyAndWidthInfo r
            // account for absent subtrees
            let lw,lb = if lw=0 then 1," " else lw,"/"
            let rw,rb = if rw=0 then 1," " else rw,"\\"
            // compute full width of this tree
            let totalLeftWidth = (max (max lw swl) 1)
            let totalRightWidth = (max (max rw swr) 1)
            let w = totalLeftWidth + 1 + totalRightWidth

            let rc2 = totalLeftWidth + 1 + rc
            // make left and right tree same height        
            let lp = if lp.Length < rp.Length then lp @ List.init (rp.Length-lp.Length) (fun _ -> "") else lp
            let rp = if rp.Length < lp.Length then rp @ List.init (lp.Length-rp.Length) (fun _ -> "") else rp
            // widen left and right trees if necessary (in case parent node is wider, and also to fix the 'added height')
            let lp = lp |> List.map (fun s -> if s.Length < totalLeftWidth then (nSpaces (totalLeftWidth - s.Length)) + s else s)
            let rp = rp |> List.map (fun s -> if s.Length < totalRightWidth then s + (nSpaces (totalRightWidth - s.Length)) else s)
            // first part of line1
            let line1 =
                if swl < lw - lc - 1 then
                    (nSpaces (lc + 1)) + (nBars (lw - lc - swl)) + s
                else
                    (nSpaces (totalLeftWidth - swl)) + s
            // line1 right bars
            let line1 =
                if rc2 > line1.Length then
                    line1 + (nBars (rc2 - line1.Length))
                else
                    line1
            // line1 right padding
            let line1 = line1 + (nSpaces (w - line1.Length))
            // first part of line2
            let line2 = (nSpaces (totalLeftWidth - lw + lc)) + lb 
            // pad rest of left half
            let line2 = line2 + (nSpaces (totalLeftWidth - line2.Length))
            // add right content
            let line2 = line2 + " " + (nSpaces rc) + rb
            // add right padding
            let line2 = line2 + (nSpaces (w - line2.Length))
            let resultLines = line1 :: line2 :: ((lp,rp) ||> List.map2 (fun l r -> l + " " + r))
            for x in resultLines do
                assert(x.Length = w)
            resultLines, w, lw-swl, totalLeftWidth+1+swr
    and nSpaces n = 
        if n >= 0 then String.replicate n " " else String.replicate 1 " "
    and nBars n = 
        String.replicate n "_"

    let PrintTree t =
        let sl,_,_,_ = PrettyAndWidthInfo t
        for s in sl do
            printfn "%s" s

    let printCol (s:(string*int) list) = 
        let rec aux (sln:(string*int) list) acc = 
            match sln with
            | [] -> acc
            | x::xs -> "| " + (String.replicate (snd x) " ") + fst x + " " + aux xs acc
            
        let s = (aux s "")
        printfn "%s" (s + "|")

    let findMaxStrLength (l:string list) = 
      let rec helper((l:string list),m) =
        match l with
        | [] -> m
        | (x::xs) -> helper(xs, if (x.Length > m) then x.Length else m)
      helper (l,0)

    let printTable (il:string list list) =
        let m = il.[0]
        let maxLength = findMaxStrLength m
        let mm = (List.map(fun (x:string) -> (x,maxLength-x.Length)) il.[0])
        printCol mm
        for j = 1 to il.Length-1 do
            printCol (List.map(fun x -> (x,maxLength-x.Length)) il.[j])

    let GenTruthTable variables =
        let vlength = Convert.ToInt32(2.0 ** float variables)
        [for i in 0..vlength-1 -> [for y in 0..variables-1 -> (i >>> y) % 2 = 0]]

    let GenLists (list:'a list) =
        GenTruthTable list.Length