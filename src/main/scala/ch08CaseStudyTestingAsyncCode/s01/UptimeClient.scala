package ch08CaseStudyTestingAsyncCode.s01

import cats.Id

import scala.concurrent.Future

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int] = Future.successful(hostname.length)
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  override def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
}

