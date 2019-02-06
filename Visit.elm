module Visit exposing(..)

type Visit = Up Elt
            | Down Elt 
            | Midle Elt 


type Elt = Foo   
        |Bar    
        |Baz
        |Buzz


doUp :Elt ->  String 
doUp elt =
    case elt of 
        Foo ->  "do Up on foo "
        Bar ->  "do Up on bar "
        Buzz ->  "do Up on buzz"
        Baz ->  "do Up on baz"

doDown : Elt  -> String 
doDown elt  = "do Down on "++ (toString elt )

doMidle : Elt  -> String 
doMidle elt  = "do Midlde on "++ (toString elt )

x  : List Elt 
x = [Foo, Bar, Baz, Buzz]

appliquerDoDown l   =  List.map doDown  l 
appliquerDoUp l     =  List.map  doUp   l 
appliquerDoMidle l     =  List.map  doMidle  l 
