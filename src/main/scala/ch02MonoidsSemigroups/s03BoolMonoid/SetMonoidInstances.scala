package ch02MonoidsSemigroups.s03BoolMonoid

object SetMonoidInstances {
  implicit def unionSetMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  implicit def intersectionSetSemigroup[A]: Semigroup[Set[A]] = _ intersect _

  implicit def symmetricDiffSetMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]
    override def combine(x: Set[A], y: Set[A]): Set[A] = (x diff y) union (y diff x)
  }
}
