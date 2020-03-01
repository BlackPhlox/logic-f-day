# Logic-F-day
Implementing the same functionality of Logic Friday to F#

Print to string and simplify/minimize gate composition
```c#
let simp = (AND (OR (IN "A", IN "B"), OR (IN "A", NOT (IN "B"))));;

simp.ToString();;
val it : string = "((A+B)*(A+~B))"

(Still work-in-progress)
gateSimplify simp;;
val it : gExp = IN "A"

```

Evaluate gates using variables
```c#
let st = Map.ofList [("A", B true)];;
let eval = ((IN "A") .&. (B true));;
gateEval eval st;;
val it : gExpResult = EVAL true
```

Dynamically evaluate expression if variables does not exist
```c#
let st = Map.ofList [("A", B true)];;
let eval = ((IN "A") .&. (IN "B"));;
gateEval eval st;;
val it : gExpResult = TYPE (IN "B")
```

Convert to only use NAND-gates and print to tree:
```c#
let nandonly = nandGateify ((NOT (IN "A").-&. IN "B") .*|. NOT (IN "A" .|. NOT(IN "A")));;
nandonly.PrintTree();;
```

```
                               ____~*___________________________                         
                              /                                 \                        
       ______________________~*_           ______________________~*_________             
      /                         \         /                                 \            
    ~*__________                ~*      ~*__________                   ______~*_         
    /           \               / \     /           \                 /         \        
  ~*       ______~*_          ~*  B   ~*       ______~*_            ~*__        ~*__     
  / \     /         \         / \     / \     /         \           /   \       /   \    
~*  B   ~*__        ~*__      A A   ~*  B   ~*__        ~*__      ~*    ~*    ~*    ~*   
/ \     /   \       /   \           / \     /   \       /   \     / \   / \   / \   / \  
A A   ~*    ~*    ~*    ~*          A A   ~*    ~*    ~*    ~*    A A ~*  ~*  A A ~*  ~* 
      / \   / \   / \   / \               / \   / \   / \   / \       / \ / \     / \ / \
      A A ~*  ~*  A A ~*  ~*              A A ~*  ~*  A A ~*  ~*      A A A A     A A A A
          / \ / \     / \ / \                 / \ / \     / \ / \                        
          A A A A     A A A A                 A A A A     A A A A                        
```

Print truthtable of Gates:
```c#
TruthTable (XNOR(IN "A",IN "B"));;
| A | B | O |
| T | T | T |
| F | T | F |
| T | F | F |
| F | F | T |
```
