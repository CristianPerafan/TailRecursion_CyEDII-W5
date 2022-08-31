import scala.annotation.tailrec

@main
def main(): Unit = {

  println(findElement(List("123","456","789"),"456"))
  println(replacementFunction(List("Cat","Dog","Dog","Cat","Dog","Cat","Dog"),"Cat","Dog"))
  println(findEvenNumbers(List(1,2,4,5,6,7,8,9,10,11,12)))
  println(additionOfMultiplesOfSix(List(6,12,18)))


}
def isSame(a:String,b:String):Boolean = {
  if(a == b) true else false
}
@tailrec
def findElement(l:List[String],elementSearched:String):Boolean = {
  l match{
    case Nil => false
    case h::Nil => isSame(h,elementSearched)
    case h::t => {
      if(isSame(h,elementSearched)) true else findElement(t,elementSearched)
    }
  }
}

def replacementFunction(l:List[String],s1:String,s2:String):List[String] = {
  auxReplacementFunction(l,List(),s1, s2)
}

@tailrec
def auxReplacementFunction(l:List[String],out:List[String],s1:String,s2:String):List[String] = {
  l match{
    case Nil => out
    case h::Nil => if(isSame(h,s1)) out:+s2 else out
    case h::t => if(isSame(h,s1)) auxReplacementFunction(t,out:+s2,s1,s2) else auxReplacementFunction(t,out:+h,s1,s2)
  }
}

def findEvenNumbers(l:List[Int]):  List[Int] = {
  auxFindEvenNumber(l,List())
}

def isAEvenNumber(a: Int): Boolean = {
  if(a%2 == 0) true else false
}

@tailrec
def auxFindEvenNumber(l:List[Int],out:List[Int]): List[Int] = {
  l match{
    case Nil => out
    case h::Nil => if(isAEvenNumber(h)) out:+h else out
    case h::t => if(isAEvenNumber(h)) auxFindEvenNumber(t,out:+h) else auxFindEvenNumber(t,out)
  }
}


def additionOfMultiplesOfSix(l:List[Int]):Int = {
  auxAdditionOfMultiplesOfSix(l,0)
}


def isMultipleOfSix(a:Int):Boolean = {
  if(a%6 == 0) true else false
}

def auxAdditionOfMultiplesOfSix(l:List[Int],accum: Int): Int = {
  l match
    case Nil => accum
    case h::Nil => if(isMultipleOfSix(h)) accum+h else accum
    case h::t => if(isMultipleOfSix(h)) auxAdditionOfMultiplesOfSix(t,accum+h) else auxAdditionOfMultiplesOfSix(t,accum)
}


