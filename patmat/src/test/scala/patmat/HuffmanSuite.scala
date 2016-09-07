package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("create code tree") {
    val charlist = List('a', 'a', 'a', 'b', 'b', 'c')
    assert(createCodeTree(charlist) === Fork(Leaf('a', 3), Fork(Leaf('c', 1), Leaf('b', 2), List('c', 'b'), 3), List('a', 'c', 'b'), 6))
  }

  test("singletone") {
    assert(singleton(List()) == false)
    assert(singleton(List(Leaf('a', 1))) == true)
    assert(singleton(List(Leaf('a', 1), Leaf('b', 2))) == false)
  }

  test("decode only") {
    val tree = createCodeTree(List('a', 'a', 'a', 'b', 'b', 'c'))
    assert(decode(tree, List(0)) === List('a'))
    assert(decode(tree, List(1, 0)) === List('c'))
    assert(decode(tree, List(1, 1)) === List('b'))
    assert(decode(tree, List(0, 1, 0)) === List('a', 'c'))
    assert(decode(tree, List(0, 1, 0, 0, 0)) === List('a', 'c', 'a', 'a'))
    assert(decode(tree, List(1, 1, 0)) === List('b', 'a'))    
  }

  test("encode only") {
    val tree = createCodeTree(List('a', 'a', 'a', 'b', 'b', 'c'))
    assert(encode(tree)(List('a')) === List(0))
    assert(encode(tree)(List('b')) === List(1, 1))
    assert(encode(tree)(List('c')) === List(1, 0))
    assert(encode(tree)(List('a', 'b')) === List(0, 1, 1))
  }

  test("quick encode") {
    val tree = createCodeTree(List('a', 'a', 'a', 'b', 'b', 'c'))
    assert(quickEncode(tree)(List('a')) === List(0))
    assert(quickEncode(tree)(List('b')) === List(1, 1))
    assert(quickEncode(tree)(List('c')) === List(1, 0))
    assert(quickEncode(tree)(List('a', 'b')) === List(0, 1, 1))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
