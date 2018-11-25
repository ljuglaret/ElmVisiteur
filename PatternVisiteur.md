
# De java a elm

---

## Java c est (trop de ) classes

```javascript
 class Monkey { shout() { console.log('Ooh oo aa aa!') } accept(operation) { operation.visitMonkey(this) } }



//Et ainsi de suite pour chaque animal

//Maintenant le visiteur

const speak = { visitMonkey(monkey){ monkey.shout() }
}

//Et on visite ainsi de suite tous les animaux


//Et enfin son utilisation

const monkey = new Monkey()  monkey.accept(speak) 
```

---
 Que peut on remarquer?
C est très (trop) répétitif, et pas du tout pratique.

 Par exemple : que se passe t il pour ajouter un animal avec sa propre méthode "cri" ?
Et bien il faudra ajouter dans la variable speak 

```javascript

visitMonkey(monkey){ monkey.shout() }

```


 Et si l on veut ajouter un nouveau visiteur par exemple espèce ?
 Ça sera encore plus long puisque dans chaque classe correspondant un animal il faudra indiquer
.accept(espèce)

---

## case of, un visiteur comme un autre

---

Comment va t on faire en elm?
- creer un type Animaux sous la forme d un record avec comme champs le nom le cri et l espèce.
- chaque champs est un d un type particulier qu il faut aussi définir

```elm
type Animaux s  = Animal { nom : Nom, cri : Cri s, espece  : Espece}
``` 

---

Avantage

Pour ajouter une opération ou une variable c est très rapide.

Par exemple, pour ajouter un animal il faudra autoriser une nouvelle valeur à Nom.


```elm
type Nom = Chat 
           |Chien
           |Cheval -- Un nouvel animal a ete ajoute
```
Pour ajouter un nouveau visiteur comme l espèrence de vie il faudra créer un nouveau type et l ajouter au record "Animaux" , fonctionne sur ce principe  : 
```elm
type Espece = Canide
            | Felin 
```

---