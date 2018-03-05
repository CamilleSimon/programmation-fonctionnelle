# programmation-fonctionnelle

# Introduction

Une queue est une liste spécifique permettant un accès FIFO (first-in, first-out) c’est-à-dire une liste dans laquelle on peut retirer le dernier élément en temps raisonnable. Cette implémentation permet en outre de conserver l’accès au premier élément en temps constant.

Cette iplémentation repose sur l'utilisation en interne de deux listes nommées « in » et « out » (entrée, et sortie). La liste d’entrée reçoit les éléments que l’on ajoute au fil du temps. La seconde reste vide tant que l’on n’a pas besoin de retirer d’élément à la fin. Dès que cette occasion se présente, on inverse la liste d’entrée et on l’assigne à la sortie, la liste d’entrée devient alors vide.

L’opération d’inversion ne se produit en retirant les éléments à la fin que si la liste de sortie est vide. La structure fonctionne donc en temps constant « amorti ».

# Les méthodes

- La méthode `enqueue(x:T)` ajoute un élément à la queue.
- La méthode `dequeue()` retourne un tuple composé du premier élément ajouté à la queue, et de la queue privé de cet élément.
- La méthode `head()` retourne le premier élément ajouté à la queue.
- La méthode `isEmpty` retourne `true` si la queue est vide, `false` sinon.
- La méthode `length` retourne le nombre d'éléments présent dans la queue.
- La méthode `rear()` retourne le dernier élément ajouté à la queue sans modifier cette dernière.

# Exemple d'utilisation

    val A = Queue[Int](Nil, Nil)    // in = Nil, out = Nil
    val B = A.enqueue(1)            // = Queue(1->Nil, Nil)
    val C = B.enqueue(2)            // = Queue(2->1->Nil, Nil)
    val D = C.enqueue(3)            // = Queue(3->2->1->Nil, Nil)
    val E = D.dequeue()._2          // = (1, Queue(Nil, 2->3->Nil) ici 'in' est inversée et placée en 'out'
    val F = E.dequeue()._2          // = (2, Queue(Nil, 3->Nil)
    val G = F.dequeue()._2          // = (3, Queue(Nil, Nil))
    G.isEmpty                       // = True     
