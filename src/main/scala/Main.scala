import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object Opses {
  implicit class FutureOptionOps[A](value: Future[Option[A]])(implicit ec: ExecutionContext) {
    def innerMap[B](that: A => B): Future[Option[B]] = value.map(_.map(that))
    def innerFlatMap[B](that: A => Option[B]): Future[Option[B]] = value.map(_.flatMap(that))
    def flatLiftMap[B](that: A => Future[B]): Future[Option[B]] = value.flatMap {
      case Some(a) => that(a).map(Some.apply)
      case None => Future.successful(None)
    }
    def nestedFlatMap[B](that: A => Future[Option[B]]): Future[Option[B]] = value.flatMap {
      case Some(a) => that(a)
      case None => Future.successful(None)
    }
  }
}

object Main extends App {
  import scala.concurrent.ExecutionContext.Implicits.global

  def intMapper(a: Int): Int = a
  def intOptionMapper(a: Int): Option[Int] = Some(a)
  def intFutureMapper(a: Int): Future[Int] = Future(a)
  def intFutureOptionMapper(a: Int): Future[Option[Int]] = Future(Some(a))
  def optionalIntFutureOptionMapper(a: Option[Int]): Future[Option[Int]] =
    a.fold(Future(Option.empty[Int]))(a => Future(Option(a)))


  def one: Future[Option[Int]] =
    Future.successful(Option(1))
      .flatMap(optionalIntFutureOptionMapper)
      .map(optionalInt => optionalInt.map(intMapper))
      .map(optionalInt => optionalInt.flatMap(intOptionMapper))
      .flatMap {
        case Some(int) => intFutureMapper(int).map(Some.apply)
        case None => Future.successful(None)
      }
      .flatMap {
        case Some(int) => intFutureOptionMapper(int)
        case None => Future.successful(None)
      }

  def two: Future[Option[Int]] = {
    import Opses.FutureOptionOps

    Future.successful(Option(2))
      .flatMap(optionalIntFutureOptionMapper)
      .innerMap(intMapper)
      .innerFlatMap(intOptionMapper)
      .flatLiftMap(intFutureMapper)
      .nestedFlatMap(intFutureOptionMapper)
  }

  println(Await.result(one, 1.seconds))
  println(Await.result(two, 1.seconds))
}


