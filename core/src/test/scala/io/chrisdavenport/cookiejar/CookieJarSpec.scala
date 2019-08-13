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

  // "isExpiredByExpiration" should {
  //   "show an expiration in the past as expired" in {
  //     val now = HttpDate.unsafeFromEpochSecond(5000)
  //     val expired = HttpDate.unsafeFromEpochSecond(0)
  //     val key = CookieKey("foo", None, None)
  //     val value = CookieValue(expired, ResponseCookie(
  //       "foo",
  //       "bar",
  //       expires = Some(expired)
  //     ))
  //     CookieJar.isExpiredByExpiration(now)((key, value)) must_=== true
  //   }

  //   "show a cookie that is still valid" in {
  //     val now = HttpDate.unsafeFromEpochSecond(0) 
  //     val expired = HttpDate.unsafeFromEpochSecond(5000)
  //     val key = CookieKey("foo", None, None)
  //     val value = CookieValue(expired, ResponseCookie(
  //       "foo",
  //       "bar",
  //       expires = Some(expired)
  //     ))
  //     CookieJar.isExpiredByExpiration(now)((key, value)) must_=== false
  //   }
  //   "is not expired if not set" in {
  //     val now = HttpDate.unsafeFromEpochSecond(0) 
  //     val key = CookieKey("foo", None, None)
  //     val value = CookieValue(now, ResponseCookie(
  //       "foo",
  //       "bar",
  //       expires = None
  //     ))
  //     CookieJar.isExpiredByExpiration(now)((key, value)) must_=== false
  //   }
  // }

  // "isExpiredByMaxAge" should {
  //   "not be expired if the time is great enough" in {
  //     val now = HttpDate.unsafeFromEpochSecond(5000) 
  //     val key = CookieKey("foo", None, None)
  //     val value = CookieValue(epoch, ResponseCookie(
  //       "foo",
  //       "bar",
  //       maxAge = Some(5001L)
  //     ))
  //     CookieJar.isExpiredByMaxAge(now)((key, value)) must_=== false
  //   }

  //   "be expired if the time is small enough" in {
  //     val now = HttpDate.unsafeFromEpochSecond(5000) 
  //     val key = CookieKey("foo", None, None)
  //     val value = CookieValue(epoch, ResponseCookie(
  //       "foo",
  //       "bar",
  //       maxAge = Some(4999L)
  //     ))
  //     CookieJar.isExpiredByMaxAge(now)((key, value)) must_=== true
  //   }

  //   "be expired if it matches exactly" in {
  //     val now = HttpDate.unsafeFromEpochSecond(5000) 
  //     val key = CookieKey("foo", None, None)
  //     val value = CookieValue(epoch, ResponseCookie(
  //       "foo",
  //       "bar",
  //       maxAge = Some(500L)
  //     ))
  //     CookieJar.isExpiredByMaxAge(now)((key, value)) must_=== true
  //   }

  //   "not be expired if not set" in {
  //     val now = HttpDate.unsafeFromEpochSecond(5000) 
  //     val key = CookieKey("foo", None, None)
  //     val value = CookieValue(epoch, ResponseCookie(
  //       "foo",
  //       "bar"
  //     ))
  //     CookieJar.isExpiredByMaxAge(now)((key, value)) must_=== false
  //   }
  // }

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