module VisitFin2 exposing(..)

type Bin = Plus 
             | Moins 
             | Fois 
             | Div      
             | Puiss    
 

type Trigo =  Cos       
            | Sin      
            | Tan      



type Expr a = OpeBin   { bin  : Bin , exprg : Expr a, exprd : Expr a}
            | Const     Float
            | Inconnue  a
            | OpeTrigo  { trigo : Trigo, expr : Expr a} 

type Var = X  
            | Y  
            | Z

evalFold  : List (a, Float)  -> Expr a ->   Expr a
evalFold liste expr = 
        let
                eval : (a,Float) -> Expr a -> Expr a
                eval (var,val) expr= 
                        case expr of 
                                Const a -> Const a 
                                Inconnue xy-> if (xy == var) then Const val
                                        else Inconnue xy
                                OpeBin { bin,exprg,exprd}-> OpeBin{bin = bin, exprg = eval (var , val) exprg ,exprd = eval (var, val) exprd }
                                OpeTrigo  { trigo , expr } ->OpeTrigo {trigo = trigo, expr = eval (var,val) expr}

                                
        in
                List.foldl  eval expr liste

calcul :  Expr a ->    Float
calcul   expr =
        case expr of 
                Const a -> a
                Inconnue xy-> 0
                OpeBin { bin,exprg,exprd}  -> 
                        case bin of 
                                Plus -> ( calcul exprg ) + ( calcul exprd)
                                Moins -> ( calcul exprg ) - ( calcul exprd)
                                Fois -> ( calcul exprg ) * ( calcul exprd)
                                Div -> ( calcul exprg ) / ( calcul exprd)
                                Puiss -> ( calcul exprg ) ^ ( calcul exprd)
                OpeTrigo  { trigo , expr } ->
                        case trigo of
                                Cos -> cos (calcul expr )
                                Sin -> sin (calcul expr)
                                Tan -> tan (calcul expr)


(<+>):  Expr a ->  Expr a -> Expr a
(<+>) g d = OpeBin   { bin  = Plus , exprg = g  , exprd = d }
 
(<->):  Expr a ->  Expr a -> Expr a
(<->) g d = OpeBin   { bin  = Moins , exprg = g  , exprd = d }
 

(<*>):  Expr a ->  Expr a -> Expr a
(<*>) g d = OpeBin   { bin  = Fois , exprg = g  , exprd = d }
 
(</>):  Expr a ->  Expr a -> Expr a
(</>) g d = OpeBin   { bin  = Div , exprg = g  , exprd = d }


(<^>):  Expr a ->  Expr a -> Expr a
(<^>) g d = OpeBin   { bin  = Puiss , exprg = g  , exprd = d }
  
(cosinus):   Expr a -> Expr a
(cosinus) g = OpeTrigo   { trigo  = Cos , expr = g  }

(sinus):   Expr a -> Expr a
(sinus) g = OpeTrigo   { trigo  = Sin , expr = g  }


(tangente):   Expr a -> Expr a
(tangente) g = OpeTrigo   { trigo  = Tan , expr = g  }


--(3 + x ) * (x - y)
expr : Expr Var 
expr =  ((Const 3) <+> (Inconnue X)) <*> ((Inconnue X) <-> ( Inconnue Y))

r :Float
r = calcul (evalFold [(X,2),(Y, -3)] expr)

-- (Cos X)^2 + (Sin Y)^2
expr2 : Expr Var
expr2  = ((cosinus (Inconnue X ))<^>(Const 2)) <+> ((sinus (Inconnue Y))<^>(Const 2))

r2 : Float
r2 = calcul (evalFold [(X,2), (Y,2)] expr2)

{-
m a      -> ( a -> b) -> m b 

andThenExpr ma fab =
        case ma of =
                OpeBin { bin, exprg , exprd } -> OpeBin {bin = bin, exprg = eval (var , val) exprg ,exprd = eval (var, val) exprd }
                Const a -> Const a 
                Inconnue xy-> if (xy == var) then Const val
                        else Inconnue xy
                OpeTrigo  { trigo , expr } ->OpeTrigo {trigo = trigo, expr = eval (var,val) expr}

OpeBin { bin, exprg , exprd } -> andThenExpr OpeBin { bin, exprg , exprd } ()


-}
