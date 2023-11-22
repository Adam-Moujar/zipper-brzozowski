// huge thanks to : https://exercism.org/tracks/scala/exercises/zipper/solutions/mendellev
//                  and http://blog.ezyang.com/2010/04/you-could-have-invented-zippers/

// Rexp is our Binary tree

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Option[Rexp], r2: Option[Rexp]) extends Rexp 
case class SEQ(r1: Option[Rexp], r2: Option[Rexp]) extends Rexp 
case class STAR(r: Option[Rexp]) extends Rexp 

// Rexp convenience 
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(Some(CHAR(c)), Some(charlist2rexp(s)))
}

implicit def string2rexp(s : String) : Rexp = {
    charlist2rexp(s.toList)
}

extension (r: Rexp) {
  def ~ (s: Rexp) = SEQ(Some(r), Some(s))
  def % = STAR(Some(r))
  def | (s: Rexp) = ALT(Some(r), Some(s))
}


extension (s: String) {
  def | (r: Rexp) = ALT(Some(s), Some(r))
  def | (r: String) = ALT(Some(s), Some(r))
  def % = STAR(Some(s))
  def ~ (r: Rexp) = SEQ(Some(s), Some(r))
  def ~ (r: String) = SEQ(Some(s), Some(r))
  //def $ (r: Rexp) = RECD(Some(s), Some(r))
}

// derivative and nullable

def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(Some(r1), Some(r2)) => nullable(r1) || nullable(r2)
  case SEQ(Some(r1), Some(r2)) => nullable(r1) && nullable(r2)
  case STAR(Some(r)) => true
}

def znullable(zipper : Zipper) : Boolean = zipper.r match {
    case ZERO    => false
    case ONE     => true
    case CHAR(c) => false
    case ALT(Some(r1), Some(r2)) => znullable(left(zipper).get) || znullable(right(zipper).get)
    case SEQ(Some(r1), Some(r2)) => znullable(left(zipper).get) && znullable(right(zipper).get)
    case STAR(Some(r)) => true
}
// THIS LOOKS HIDEOUS BUT IT WORKS I GUESS?
def zder(c: Char, zipper: Zipper) : Zipper = zipper.r match {
    case ZERO    => zipper
    case ONE     => Zipper(ZERO, zipper.context)
    case CHAR(d) => if(c == d) Zipper(ONE, zipper.context) else Zipper(ZERO, zipper.context)
    case ALT(Some(r1), Some(r2)) => Zipper(ALT(Some(zder(c, left(zipper).get).r), Some(zder(c, right(zipper).get).r)), zipper.context)
    case SEQ(Some(r1), Some(r2)) => 
        if(nullable(r1)) Zipper(ALT(Some(SEQ(Some(zder(c, left(zipper).get).r), Some(r2))), Some(zder(c, right(zipper).get).r)), zipper.context)
        else Zipper(SEQ(Some(zder(c, left(zipper).get).r), Some(r2)), zipper.context)
    case STAR(Some(r)) => Zipper(SEQ(Some(zder(c, left(zipper).get).r), Some(zipper.r)), zipper.context)
}
// Context for zipper

abstract class Context
case class TopContext() extends Context
case class LeftContext(r: Rexp,context: Context) extends Context
case class RightContext(r: Rexp,context: Context) extends Context

// zipper 
case class Zipper(r: Rexp, context: Context = TopContext())

def newZipper(r: Rexp) : Zipper = Zipper(r)

def returnRexp(zipper : Zipper) : Rexp = (zipper.r, zipper.context) match{
    case (r, TopContext()) => r
    case (_ , _ ) => returnRexp(up(zipper).get)  
}

def value(zipper : Zipper) : Rexp = {
    zipper.r
}

def left(zipper: Zipper) : Option[Zipper] = {
    if(!hasLeft(zipper.r))
    {
        None
    }
    else
    {
        val leftRexp = returnLeft(zipper.r)
        val currentRexp = zipper.r
        val newRexp = currentRexp match {
            case ALT(Some(r1), Some(r2)) => ALT(None, Some(r2))
            case SEQ(Some(r1), Some(r2)) => SEQ(None, Some(r2))
            case STAR(_) => STAR(None)
        }
        Some(Zipper(leftRexp, RightContext(newRexp, zipper.context)))
    }
}


def right(zipper: Zipper) : Option[Zipper] = {
    if(!hasRight(zipper.r))
    {
        None
    }
    else
    {
        val rightRexp = returnRight(zipper.r)
        val currentRexp = zipper.r
        val newRexp = currentRexp match {
            case ALT(Some(r1), Some(r2)) => ALT(Some(r1), None)
            case SEQ(Some(r1), Some(r2)) => SEQ(Some(r1), None)
        }
        Some(Zipper(rightRexp, LeftContext(newRexp, zipper.context)))
    }
}

def up(zipper : Zipper) : Option[Zipper] = (zipper.r, zipper.context) match {
    //case TopContext() => None
    //case LeftContext(binTree, context)  =>  Some[Zipper[A]](Zipper[A](BinTree[A](binTree.value,left  =binTree.left, right  = Some(zipper.binTree)), context))
    //case RightContext(binTree, context) =>  Some(Zipper[A](BinTree[A](binTree.value,right =binTree.right, left   = Some(zipper.binTree)), context))
    case (_, TopContext()) => None
    case (r1, RightContext(ALT(None, r2), c)) => Some(Zipper(ALT(Some(r1),r2), c))
    case (r1, RightContext(SEQ(None, r2), c)) => Some(Zipper(SEQ(Some(r1),r2), c))
    case (r, RightContext(STAR(None), c))     => Some(Zipper(STAR(Some(r)), c))
    case (r2, LeftContext(ALT(r1, None), c))  => Some(Zipper(ALT(r1,Some(r2)), c))
    case (r2, LeftContext(SEQ(r1, None), c))  => Some(Zipper(SEQ(r1,Some(r2)), c))

}

// helper functions for zipper
def hasLeft(r: Rexp) : Boolean = r match{
    case ZERO                    => false
    case ONE                     => false
    case CHAR(_)                 => false
    case ALT(Some(r1), Some(r2)) => true
    case SEQ(Some(r1), Some(r2)) => true
    case STAR(Some(r))           => true
}

def hasRight(r: Rexp) : Boolean = r match{
    case ZERO                    => false
    case ONE                     => false
    case CHAR(_)                 => false
    case ALT(Some(r1), Some(r2)) => true
    case SEQ(Some(r1), Some(r2)) => true
    case STAR(Some(r))           => false
}

def returnLeft(r: Rexp) : Rexp = r match{
    case ALT(Some(r1), Some(r2)) => r1
    case SEQ(Some(r1), Some(r2)) => r1
    case STAR(Some(r))           => r 
}

def returnRight(r: Rexp) : Rexp = r match{
    case ALT(Some(r1), Some(r2)) => r2
    case SEQ(Some(r1), Some(r2)) => r2
}

// tests
@main 
def test_hasLeft() = {
    val r1 = "a" | "b"
    val r2 = "a" ~ "b"
    val r3 = "a".%
    val r4 = "a"
    val r5 = ZERO
    val r6 = ONE

    println(s"hasLeft of rexp: ${r1} is ${hasLeft(r1)}")
    println(s"hasLeft of rexp: ${r2} is ${hasLeft(r2)}")
    println(s"hasLeft of rexp: ${r3} is ${hasLeft(r3)}")
    println(s"hasLeft of rexp: ${r4} is ${hasLeft(r4)}")
    println(s"hasLeft of rexp: ${r5} is ${hasLeft(r5)}")
    println(s"hasLeft of rexp: ${r6} is ${hasLeft(r6)}")
}

@main 
def test_hasRight() = {
    val r1 = "a" | "b"
    val r2 = "a" ~ "b"
    val r3 = "a".%
    val r4 = "a"
    val r5 = ZERO
    val r6 = ONE

    println(s"hasLeft of rexp: ${r1} is ${hasRight(r1)}")
    println(s"hasLeft of rexp: ${r2} is ${hasRight(r2)}")
    println(s"hasLeft of rexp: ${r3} is ${hasRight(r3)}")
    println(s"hasLeft of rexp: ${r4} is ${hasRight(r4)}")
    println(s"hasLeft of rexp: ${r5} is ${hasRight(r5)}")
    println(s"hasLeft of rexp: ${r6} is ${hasRight(r6)}")
}

@main 
def test_ALT() = {
    val r = "a" | "b"
    println(r)
}

@main 
def test_LEFT() = {
    val r = "a" | "b"
    println(r)
    val zip = newZipper(r)
    println(zip)
    val lzip = left(zip).get
    println(lzip)
    println(lzip.r)
}

@main 
def test_TWO_LEFT() = {
    val r = ("a" | "b") ~ ("c" | "d")
    val zip = newZipper(r)
    val lzip = left(zip).get
    println(lzip)
    val llzip = left(lzip).get
    println(llzip)
}

@main
def test_RIGHT() = {
    val r = "a" | "b"
    val zip = newZipper(r)
    val rzip = right(zip).get
    println(rzip)
}

@main 
def test_UP() = 
{
    val r = "a" | "b"
    val zip = newZipper(r)
    println(zip)
    val rzip = right(zip).get
    println(rzip)
    val bzip = up(rzip).get
    println(bzip)
    println(up(bzip))
}

@main 
def test_return_rexp() = 
{
    val r = "a" | "b"
    val zip = newZipper(r)
    val rzip = right(zip).get
    println(rzip)
    println(returnRexp(rzip))
}

@main 
def test_znullable() = {
    val r1 = "a" | "b"
    val zip1 = newZipper(r1)
    val r2 = "a" | ""
    val zip2 = newZipper(r2)
    println(znullable(zip1))
    println(znullable(zip2))
}

@main 
def test_zder() = {
    val r1 = "a".%
    val zip1 = newZipper(r1)
    val lzip1 = left(zip1).get
    val derlzip1 = zder('a', lzip1)
    val derzip1 = up(derlzip1).get
    println(lzip1)
    println(derlzip1)
    println(derzip1)
}