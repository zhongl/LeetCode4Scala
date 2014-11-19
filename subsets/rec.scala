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
  def prepend(h: Int, t: List[List[Int]]): List[List[Int]] = {
    (t map {i => h :: i}) ::: t
  }
  in sortWith (_ < _) match {
    case Nil    => in :: Nil
    case h :: t => prepend(h, subsets(t))
  }
}

