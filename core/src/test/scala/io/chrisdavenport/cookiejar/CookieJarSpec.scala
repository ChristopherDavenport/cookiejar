package io.chrisdavenport.cookiejar

import org.specs2.mutable.Specification

import cats.effect._
// import cats.effect.laws.util.TestContext
import org.http4s._
import CookieJar._
import org.http4s.ResponseCookie

class CookieJarSpec extends Specification{

  val epoch: HttpDate = HttpDate.unsafeFromEpochSecond(0L)

  "CookieJar" should {
    "do something" in {
      // val ctx = TestContext()
      // implicit val CS = ctx.contextShift[IO]
      // implicit val T : Timer[IO] = ctx.timer[IO]

      // val now = CookieJar.currentHttpDate[IO]
      //   .unsafeRunSync()

      ok

    }
  }

  "cookieAppliesToRequest" should {
    "apply if the given domain matches" in {
      val req = Request[IO](Method.GET, uri = Uri.uri("http://google.com"))
      println(req.uri.authority.map(_.renderString))
      val cookie = ResponseCookie(
        "foo",
        "bar",
        domain = Some("google.com")
      )
      cookieAppliesToRequest(req,cookie) must_=== true
    }

    "not apply if not given a domain" in {
      val req = Request[IO](Method.GET, uri = Uri.uri("http://google.com"))
      val cookie = ResponseCookie(
        "foo",
        "bar",
        domain = None
      )
      cookieAppliesToRequest(req,cookie) must_=== false
    }

    "apply if a subdomain" in {
      val req = Request[IO](Method.GET, uri = Uri.uri("http://api.google.com"))
      val cookie = ResponseCookie(
        "foo",
        "bar",
        domain = Some("google.com") 
      )
      cookieAppliesToRequest(req,cookie) must_=== true
    }

    "not apply if the wrong subdomain" in {
      val req = Request[IO](Method.GET, uri = Uri.uri("http://api.google.com"))
      val cookie = ResponseCookie(
        "foo",
        "bar",
        domain = Some("bad.google.com") 
      )
      cookieAppliesToRequest(req,cookie) must_=== false
    }


    "not apply if the superdomain" in {
      val req = Request[IO](Method.GET, uri = Uri.uri("http://google.com"))
      val cookie = ResponseCookie(
        "foo",
        "bar",
        domain = Some("bad.google.com") 
      )
      cookieAppliesToRequest(req,cookie) must_=== false
    }

  }

}