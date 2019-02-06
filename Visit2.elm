module Visit2 exposing(..)

type Expr = Plus Float Float
            | Moins Float Float
            | Fois Float Float 



eval : Expr ->  Float
eval  o =
    case o of 
        Plus  x y ->  x + y
        Moins x y ->  x - y
        Fois x y  -> x * y




y : Float
y = eval (Moins 8 (eval ( Plus 1 2)))

-- y = eval (Moins (Plus 8 3) 5) Incorrect car (Plus 8 3) est de Type Expr,
                                -- or Moins attend en arguments
                                -- deux elements de type Float
