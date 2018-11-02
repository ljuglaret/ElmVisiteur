module Visiteur19 exposing(..)


type Expr = Plus       Expr Expr
            | Moins    Expr Expr
            | Fois     Expr Expr
            | Div      Expr Expr 
            | Const    Float
            | Puiss    Expr Expr
            | Inconnue Var
            | Cos      Expr 
            | Sin      Expr
            | Tan      Expr


type Var = X  
            | Y  
            | Z


-- foldl :  (a          ->   b  -> b     )    -> b     -> List a           -> b
-- foldl : ((Var,Float) -> Expr -> Expr  )    -> Expr  -> List (Var, Float) -> Expr

evalFold  : List (Var, Float)  -> Expr ->   Expr
evalFold liste expr0 = 
        let
                eval : (Var,Float) -> Expr -> Expr 
                eval (var,val) expr= 
                        case expr of 
                                Plus  a b  -> Plus (eval (var,val) a )  (eval   (var,val) b)
                                Moins a b ->  Moins (eval  (var,val) a )  (eval   (var,val) b)
                                Fois a b  ->  Fois (eval  (var,val) a )  (eval   (var,val) b)
                                Puiss a b -> Puiss (eval  (var,val) a )  (eval  (var,val) b)
                                Div a b  ->  Div (eval  (var,val) a )  (eval   (var,val) b)
                                Const a -> Const a
                                Inconnue xy-> if (xy == var) then Const val
                                        else Inconnue xy
                                Cos a ->  Cos (eval (var, val) a)
                                Sin a ->Sin (eval (var, val) a)
                                Tan a -> Tan (eval (var, val) a)
        in
                List.foldl  eval expr0 liste


estDivPar : Float -> Float -> Result String Float 
estDivPar x y   = 
        if (y /= 0) then Ok (x/y) 
        else Err "pas div"

calcul :  Expr ->    Float
calcul   expr =
        case expr of 
                Plus  a b   ->   ((calcul a ) + (calcul b))
                Moins a b   ->   (  (calcul a) - (calcul b))
                Fois a b    ->   (   (calcul a) * (calcul b))
                Puiss a b   ->   ( (calcul a)  ^ (calcul b))
                Const a     ->   a 
                Inconnue xy ->   0
                Div a b     ->  (calcul a ) / (calcul b)
                Cos a       ->   cos (calcul a)
                Sin a       ->   sin (calcul a)
                Tan a       ->   tan (calcul a)

resultat : List(Var,Float)-> Expr -> Float
resultat  l expr = calcul (evalFold l expr)

-- (Cos X)^2 + (Sin Y)^2

expr2 : Expr
expr2  = Plus (Puiss (Cos ((Inconnue X )))(Const 2)) (Puiss(Sin ((Inconnue Y)))(Const 2))
r2 : Float
r2 = resultat [(X,2), (Y,2)] expr2