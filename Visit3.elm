module Visit3 exposing(..)


{-
Reellement l evaluation d une expression arithmetique est recursive,
donc Plus Moins et Fois operent non pas sur des Float mais sur des Expressions
-}

type Expr = Plus     Expr Expr
            | Moins  Expr Expr
            | Fois   Expr Expr 
            | Const  Float


{-
Pareil pour la fonction eval,
c est ce qui permettra de l appeler une seule fois
et non pas a chaque fois qu il y a un operateur
-}

eval : Expr ->  Float
eval  o =
    case o of 
        Plus  x y ->  (eval x ) + (eval  y)
        Moins x y ->  (eval x ) - (eval  y)
        Fois x y  -> (eval x ) * (eval  y)
        Const x -> x




z : Float
z = eval (Moins (Plus (Const 8) (Const 3)) (Const 5))

{-
Pour que ca soit plus pratique on peut redefinir
les operateurs classiques +  - *
-}


(<+>) = Plus  
(<->) = Moins 
(<*>) = Fois

z2 : Float
z2 = eval ( ( (Const 8) <+> (Const 3)) <->  (Const 5))
