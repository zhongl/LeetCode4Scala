/**
 * {{{
 * v [1,2,3]
 * v [1] [2,3]
 * v [1] [2] [3] []
 * ---------------------
 * ^ [1] [2] [[3], []]
 * ^ [1] [[2,3], [3], [2], []]
 * ^ [[1,2,3], [2,3], [1,3], [3], [1,2], [2], [1], []]
 * }}}
 * @see https://oj.leetcode.com/problems/subsets/
 */
def subsets(in: List[Int]): List[List[Int]] = {
  def combine(h: Int, t: List[List[Int]]): List[List[Int]] = {
    if (t.isEmpty) (h :: Nil) :: Nil :: Nil 
    else (t map {i => h :: i}) ::: t
  }
  @scala.annotation.tailrec
  def loop(i: List[Int], f: List[List[Int]] => List[List[Int]]): List[List[Int]] =
    i match {
      case Nil    => f(Nil)
      case h :: t => loop(t, l => f(combine(h, l)))
    }
  loop(in sortWith (_ < _), l => l)
}

