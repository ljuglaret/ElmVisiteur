module VisitFin exposing(..)


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


eval : (Var, Float)  -> Expr ->   Expr
eval   (var,val)        expr =
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

    
(<+>) = Plus  
(<->) = Moins 
(<*>) = Fois
(<^>) = Puiss

z4 : Expr
z4 = Inconnue Z
z4eval = regroupe [(X,1),(Y,5),(Z,2)] z4


z3 : Expr
-- X^2 + Y^2
z3 = ((Inconnue X ) <^> (Const 2)) <+> ((Inconnue Y ) <^> (Const 2))

regroupe :  List(Var , Float ) -> Expr -> Expr 
regroupe l expr = 
     case l of 
        (v,f)::l          -> regroupe l (eval (v,f) expr)
        []                -> expr

z3eval = regroupe [(X,1),(Y,5)] z3


estDivPar : Float -> Float -> Result String Float 
estDivPar x y   = 
        if (y /= 0) then Ok (x/y) 
        else Err "pas div"

calcul :  Expr ->    Float
calcul   expr =
        case expr of 
                Plus  a b   ->   ((calcul a ) + (calcul b))
                Moins a b   ->   ( (calcul a) - (calcul b))
                Fois a b    ->   ( (calcul a) * (calcul b))
                Puiss a b   ->   ((calcul a)  ^ (calcul b))
                Const a     ->   a 
                Inconnue xy ->   0
                Div a b     ->  (calcul a ) / (calcul b)
                Cos a       ->   cos (calcul a)
                Sin a       ->   sin (calcul a)
                Tan a       ->   tan (calcul a)

resultat  l expr = calcul (regroupe l expr)

--(3 + x ) * (x - y)
r = resultat [(X,2),(Y, -3)]   (((Const 3) <+> (Inconnue X)) <*> (((Inconnue X) <-> ( Inconnue Y))))

r2 = resultat [(X,-2)] (Cos ((Const 2 ) <+> (Inconnue X)))