/** Une classe liste FIFO. */

case class Queue[T](in:List[T] = Nil, out:List[T] = Nil){
    
    /** Ajoute un élément `x` en tête. */
    def enqueue(x:T):Queue[T] = {
        Queue(x :: in,out)
    }
    
    /** Retire le premier élément. (C'est à dire l'élément le plus ancien, celui ajouté en premier à la liste)*/
    def dequeue():(T,Queue[T]) = {
        in match {
            //Les deux listes sont vide
            case Nil if(out.isEmpty) => throw new Exception("Vous ne pouvez pas retirer d'element a une liste vide.")
            //in est vide et out est non vide
            case Nil => (out.head, Queue(Nil, out.tail))
            //in est non vide et out est vide
            case _ if(out.isEmpty) => (in.reverse.head, Queue(Nil,in.reverse.tail))
            //Les deux listes sont non vide
            case _ => (out.head, Queue(Nil, out.tail ::: in.reverse))
        }
    }
    
    /** Accès au premier élément. (L'élément le plus ancien)*/
    def head():T = {
        out match {
            //Les deux listes sont vide
            case Nil if(in.isEmpty) => throw new Exception("Vous ne pouvez pas afficher les elements d'une liste vide.")
            //in est non vide et out est vide
            case Nil => in.reverse.head
            //out est non vide (qu'importe l'état de in)
            case _ => out.head
        }
    }
    
    /** Accès au dernière élément. (L'élément le plus récent)*/
    def rear():T = {
        in match {
            //Les deux listes sont vide
            case Nil if(out.isEmpty) => throw new Exception("Vous ne pouvez pas afficher les elements d'une liste vide.")
            //in est vide et out est non vide
            case Nil => out.reverse.head
            //in est non vide (qu'importe l'état de out)
            case _ => in.head
        }
    }
    
    /** Vrai si la liste est vide. */
    def isEmpty:Boolean = in.isEmpty && out.isEmpty
	
	/** Taille de la file. */
    def length:Int = in.length + out.length
    
}

