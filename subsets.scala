/* @see https://oj.leetcode.com/problems/subsets/ */
def subsets(in: List[Int]): List[List[Int]] = {
  val l = in sortWith (_ > _)
  def bit(n: Int): List[Int] = {
    l.zipWithIndex.foldLeft(List.empty[Int]) {
      case (r, (x, i)) => if ((n >> i & 1) > 0) x :: r else r
    }
  }
  @scala.annotation.tailrec
  def loop(i: Int, r: List[List[Int]]): List[List[Int]] = {
    if (i == 0) List.empty[Int] :: r else loop(i - 1 ,bit(i) :: r)
  }
  loop((1 << in.size) - 1, Nil)
}

