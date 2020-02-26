# Logic-F-day
Implementing the same functionality of Logic Friday to F#

```
let nandonly = nandGateSimplify ((NOT (IN "A").-&. IN "B") .*|. NOT (IN "A" .|. NOT(IN "A")))
PrintGateTree (nandonly)
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
