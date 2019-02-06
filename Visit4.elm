module Visit4 exposing(..)


type Expr = Plus     Expr Expr
            | Moins  Expr Expr
            | Fois   Expr Expr 
            | Const  Float
            | Puiss Expr Expr
            | Inconnue Var

type Var = X 
              |Y  



eval : (Var, Float)  -> Expr ->   Expr
eval   var expr =
    case var of 
        (X, val) -> 
            case expr of 
                Plus  a b  -> Plus (eval (X,val) a )  (eval   (X,val) b)
                Moins a b ->  Moins (eval  (X,val) a )  (eval   (X,val) b)
                Fois a b  ->  Fois (eval  (X,val) a )  (eval   (X,val) b)
                Puiss a b -> Puiss (eval  (X,val) a )  (eval  (X,val) b)
                Const a -> Const a
                Inconnue xy-> if (xy == X) then Const val
                              else Inconnue xy
        (Y,val) ->  case expr of 
                Plus  a b  -> Plus (eval  (Y,val) a )  (eval  (Y,val) b)
                Moins a b ->  Moins (eval (Y,val) a )  (eval  (Y,val) b)
                Fois a b  ->  Fois (eval (Y,val) a )  (eval  (Y,val) b)
                Puiss a b -> Puiss (eval (Y,val) a )  (eval  (Y,val) b)
                Const a -> Const a
                Inconnue xy->if (xy == Y) then Const val
                              else Inconnue xy        
      


(<+>) = Plus  
(<->) = Moins 
(<*>) = Fois
(<^>) = Puiss

z3 : Expr
-- X^2 + Y^2
z3 = ((Inconnue X ) <^> (Const 2)) <+> ((Inconnue Y ) <^> (Const 2))

regroupe :  List(Var , Float ) -> Expr -> Expr 
regroupe l expr = 
     case l of 
        (v,f)::(v2,f2)::l -> eval (v2,f2) (eval (v,f) expr) 
        (v,f)::l          -> eval (v,f) expr
        []                -> expr

z3eval = regroupe [(X,1),(Y,5)] z3


calcul :  Expr ->   Float
calcul   expr =

    case expr of 
        Plus  a b  ->  (calcul a ) + (calcul b)
        Moins a b ->   (calcul a) -  (calcul b)
        Fois a b  ->   (calcul a) * (calcul b)
        Puiss a b ->  (calcul a)^ (calcul b)
        Const a -> a 
        Inconnue xy -> 0

resultat  l expr = calcul (regroupe l expr)

--(3 + x ) * (x - y)
r = resultat [(X,2),(Y, -3)]   (((Const 3) <+> (Inconnue X)) <*> (((Inconnue X) <-> ( Inconnue Y))))