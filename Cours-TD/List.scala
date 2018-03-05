package pf.datastructures
/*
Type générique de A, covariant avec la liste
Si Dog héride d'Animal
List[Dog] hérite de List[Animal]
(grâce à "+A")
sealed restreint l'heritage à se fichier (vérouillage)
*/
sealed trait List[+A]
/*
Définit une classe qui ne permet de créer q'une seul instance de la classe
singleton
Nothing => terminateur en scala
*/
case object Nil extends List[Nothing]

case class Cons[+A](head:A, tail:List[A]) extends List[A]

/*
A* = {0,1,....n} \in N
*/
object List{
	def apply[A](as:A*):List[A] =
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail:_*))

	def sum(ints:List[Int]):Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
	}

	def product(ds:List[Double]):Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}

	def tail[A](l:List[A]):List[A] = l match {
		case Nil => Nil
		case Cons(_,t) => t
	}

	def drop[A](l:List[A], n:Int):List[A] = l match{
		case l if (n <= 0) => l
		case Nil => Nil
		case _ => drop(tail(l), n-1)
	}

	def dropWhile[A](l:List[A])(f:A => Boolean):List[A] = l match {
		/*case Cons(h,t) if f(h) => dropWhile(t, f) 
		case Nil => Nil 
		case _ => tail(l)
		*/
		case Cons(h,t) if f(h) => dropWhile(t)(f)
		case _ => l
	}

	def setHead[A](l:List[A], a:A):List[A] = l match{
		case Cons(h,t) => Cons(a,t)
		case _ => l
	}

	def append[A](a1:List[A], a2:List[A]):List[A] = a1 match{
		case Nil => a2
		case Cons(h,t) => Cons(h, append(t, a2))
	}

	def init[A](l:List[A]):List[A] = l match{
		case Nil => Nil
		case Cons(_, Nil) => Nil
		case Cons(h,t) => Cons(h, init(t))
	}

	/*def length[A](l:List[A]):Int = {
		case Nil => 0
		case Cons(h,t) => 1 + length(t)
	}*/

	def foldRight[A,B](l:List[A], z:B)(f:(A,B) => B):B = {
		l match {
			case Nil => z
			case Cons(x,xs) => f(x, foldRight(xs,z)(f))
		}
	}

	def foldLeft[A,B](l:List[A], z:B)(f:(B, A) => B):B = {
		l match {
			case Nil => z
			case Cons(h,t) => foldLeft(t,f(z,h))(f)

		}
	}

	def sum2(l:List[Int]) = foldRight(l,0.0)(_+_)
	def minus(l:List[Int]) = foldRight(l,0.0)(_-_)

	def product2(l:List[Double]) = foldRight(l,1.0)(_*_)

	def length2[A](l:List[A]):Int = foldRight(l,0)((a:A, x:Int) => {x+1})

	def main(args: Array[String]): Unit = {
		val example = Cons(1, Cons(2, Cons(3, Nil)))
		val example2 = List(1, 2, 3)
		val total = sum(example)
	}
}
/*
List(1,2,3) match {case => 42}
res: Int = 42
List(1,2,3) match {case Cons(h,t) => h}
res: Int = 1
List(1,2,3) match {case Cons(h,t) => t} ~ List(1,2,3) match {case Cons(_,t) => t}
res: List[Int] = Cons(2,Cons(3,Nil))
List(1,2,3) match {case Nil => 42} // ERREUR

val x = List(1,2,3,4,5) match {
	case Cons(x, Cons(2, Cons (4, _))) //FAUX
	case Nil => 42 //FAUX
	case Cons(x, Cons(y, Cons(3, Cons(4, _)))) //VRAI
	case Cons(h, t) => h + sum(t) //VRAI
	case _ => 101 //VRAI
}
*/