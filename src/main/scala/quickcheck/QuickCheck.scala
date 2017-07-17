package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    atIdx <- arbitrary[Int]
    heapRest <- oneOf(const(empty),genHeap)
  } yield insert(atIdx, heapRest)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //when one inserts two elements the findMin should return the smallest
  property("smallest from two") = forAll{ (a: Int,b: Int) =>
    val heap = insert(a, insert(b, empty))
    val min: Int = if (a>b) b else a
    findMin(heap) == min
  }

  property("delete one from two") = forAll{ (a: Int,b: Int) =>
    val heap = insert(a, insert(b, empty))
    val max: Int = if (a<b) b else a
    deleteMin(heap) == max
  }

  property("delete one leave empty") = forAll{ a: Int =>
    val heap = insert(a, empty)
    deleteMin(heap) == empty
  }

  property("meld two, findmin smallest of both") = forAll{ (heapOne: H, heapTwo: H) =>
    val melded = meld(heapOne, heapTwo)
    val minOne = findMin(heapOne)
    val minTwo = findMin(heapTwo)
    val minBoth = if (minOne < minTwo) minOne else minTwo
    findMin(melded) == minBoth
  }
}