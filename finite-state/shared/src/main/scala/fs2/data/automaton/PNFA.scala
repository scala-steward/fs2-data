package fs2
package data
package automaton

import Pred.syntax._

import cats.Show
import cats.syntax.all._

import scala.annotation.tailrec

import scala.collection.compat._

class PNFA[P, T](val init: Int, val finals: Set[Int], val transitions: Map[Int, List[(Option[P], Int)]])(implicit
    P: Pred[P, T]) {

  def epsilonClosure(qs: List[Int]): Set[Int] = {
    @tailrec
    def loop(qs: List[Int], visited: Set[Int]): Set[Int] =
      qs match {
        case q :: qs =>
          if (visited.contains(q))
            loop(qs, visited)
          else
            loop(transitions.getOrElse(q, Nil).collect { case (None, q1) => q1 } reverse_::: qs, visited + q)
        case Nil => visited
      }
    loop(qs, Set.empty)
  }

  /** Generates the stream of all combinations of predicates splitting in two sets:
    *  - predicates to match
    *  - predicates to negate
    * and then combining them with conjunction.
    * Returns only predicate that are not obviously non satisfiable.
    */
  def combineAll(predicates: List[(P, Set[Int])]): Stream[Pure, (P, Set[Int])] =
    Stream
      .range(1, predicates.size + 1)
      .flatMap(n =>
        Stream.unfold(predicates.combinations(n))(combinations =>
          if (combinations.hasNext) Some((combinations.next(), combinations)) else None))
      .map { pos =>
        val neg = predicates.diff(pos)
        (neg.foldLeft(pos.foldLeft(P.always)((p1, p2) => p1 && p2._1))((p1, p2) => p1 && !p2._1),
         pos.foldLeft(Set.empty[Int])(_ union _._2))
      }
      .filter(_._1.isSatisfiable)

  def determinize: PDFA[P, T] = {

    @tailrec
    def loop(toProcess: List[(Set[Int])],
             newStates: Map[Set[Int], Int],
             newFinals: Set[Set[Int]],
             newTransitions: Map[Int, List[(P, Set[Int])]]): PDFA[P, T] =
      toProcess match {
        case Nil =>
          new PDFA[P, T](0,
                         newFinals.map(newStates(_)),
                         newTransitions.view.mapValues(_.map { case (p, q) => (p, newStates(q)) }).toMap)
        case q :: qs =>
          if (newStates.contains(q)) {
            loop(qs, newStates, newFinals, newTransitions)
          } else {
            val newQ = newStates.size
            val newStates1 = newStates.updated(q, newQ)
            val ts = q.toList
              .flatMap(transitions.get(_))
              .flatMap(_.collect { case (Some(p), q) =>
                (p, q)
              })
              .groupMap(_._1)(_._2)
              .view
              .mapValues(epsilonClosure(_))
              .toList
            val ts1 =
              combineAll(ts).compile.toList
            val newTransistions1 = if (ts1.nonEmpty) newTransitions.updated(newQ, ts1) else newTransitions
            val newFinals1 = if (finals.exists(q.contains(_))) newFinals + q else newFinals
            loop(qs ++ ts1.map(_._2), newStates1, newFinals1, newTransistions1)
          }
      }

    loop(List(epsilonClosure(List(init))), Map.empty, Set.empty, Map.empty)
  }

}

object PNFA {

  implicit def show[P: Show, T]: Show[PNFA[P, T]] = new Show[PNFA[P, T]] {

    implicit val transShow: Show[(Option[P], Int)] = Show.show { case (p, q) =>
      val pred = p.map(_.show).getOrElse("ε")
      show"q$q: $pred"
    }

    def showTransitions(t: PNFA[P, T], q: Int): String =
      t.transitions
        .get(q)
        .map { ts =>
          show"q$q --> ${ts.mkString_(show"\n  q$q --> ")}"
        }
        .getOrElse("")

    override def show(t: PNFA[P, T]): String =
      show"""stateDiagram-v2
            |  direction LR
            |  [*] --> q${t.init}
            |  ${t.finals.toList.map(q => show"q$q --> [*]").mkString_("\n  ")}
            |  ${t.transitions.keySet.toList.map(showTransitions(t, _)).mkString_("\n  ")}""".stripMargin

  }

}
