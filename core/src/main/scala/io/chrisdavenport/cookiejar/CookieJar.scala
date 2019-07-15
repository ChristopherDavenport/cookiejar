package io.chrisdavenport.cookiejar

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.concurrent._
import org.http4s._
import org.http4s.client.Client
import scala.concurrent.duration._

object CookieJar {

  def apply[F[_]: Sync](
    alg: CookieJarAlg[F]
  )(
    client: Client[F]
  ): Client[F] = 
    Client{req => 
      for{
        _ <- Resource.liftF(alg.evictExpired)
        modRequest <- Resource.liftF(alg.enrichRequest(req))
        out <- client.run(modRequest)
        _ <- Resource.liftF(
          out.cookies
            .map(r => r.domain.fold(r.copy(domain = req.uri.host.map(_.value)))(_ => r))
            .traverse_(alg.addCookie)
        )
      } yield out
    }

  def impl[F[_]: Sync: Timer](c: Client[F]): F[Client[F]] = 
    CookieJarAlg.impl[F].map(apply(_)(c))

  /**
   * Algebra for Interfacing with the Cookie Jar
   **/
  trait CookieJarAlg[F[_]]{
    def evictExpired: F[Unit]

    def evictAll: F[Unit]

    def addCookie(c: ResponseCookie): F[Unit]

    def enrichRequest[G[_]](r: Request[G]): F[Request[G]]
  }

  object CookieJarAlg {
    def impl[F[_]: Sync: Clock]: F[CookieJarAlg[F]] = 
      in[F, F]

    def in[F[_]: Sync: Clock, G[_]: Sync]: G[CookieJarAlg[F]] = 
      Ref.in[G, F, Map[CookieKey, CookieValue]](Map.empty).map{ ref => 
        new CookieJarAlg[F]{
          override def evictExpired: F[Unit] = for {
            now <- currentHttpDate
            out <- ref.update(
              _.filterNot{t => 
                val byExp = isExpiredByExpiration(now)(t)
                val byMaxAge = isExpiredByMaxAge(now)(t)
                val isExpired =  byExp || byMaxAge
                isExpired
              }
            )
          } yield out

          override def evictAll: F[Unit] = ref.set(Map.empty)

          override def addCookie(c: ResponseCookie): F[Unit] = for {
            now <- currentHttpDate
            out <- ref.update(extractFromResponseCookie(_)(c, now))
          } yield out

          override def enrichRequest[N[_]](r: Request[N]): F[Request[N]] = 
            for {
              cookies <- ref.get.map(_.map(_._2.cookie).toList)
              applicable  = cookiesForRequest(r, cookies)
              out = applicable.foldLeft(r){ case (req, cookie) => req.addCookie(cookie)}
            } yield out
        }
      }
  }

  private final case class CookieKey(
    name: String,
    domain: Option[String],
    path: Option[String]
  )

  private final case class CookieValue(
    setAt: HttpDate,
    cookie: ResponseCookie
  )

  private def currentHttpDate[F[_]: Clock: MonadError[?[_], Throwable]] = 
    Clock[F].monotonic(SECONDS)
      .flatMap(s => HttpDate.fromEpochSecond(s).liftTo[F])

  private def keyFromRespCookie(c: ResponseCookie): CookieKey =
    CookieKey(c.name, c.domain, c.path)

  private def extractFromResponseCookie[F[_]](
    m: Map[CookieKey,CookieValue]
  )(c: ResponseCookie, httpDate: HttpDate): Map[CookieKey, CookieValue] = 
    m + (keyFromRespCookie(c) -> CookieValue(httpDate, c))

  private def isExpiredByExpiration(
    now: HttpDate
  )(m: (CookieKey, CookieValue)): Boolean = 
    m._2.cookie.expires.exists(expiresAt => now <= expiresAt)

  private def isExpiredByMaxAge(
    now: HttpDate
  )(m: (CookieKey, CookieValue)): Boolean = 
    m._2.cookie.maxAge.exists{plusSeconds => 
      val epochSecondExpiredAt = m._2.setAt.epochSecond + plusSeconds
      now <= HttpDate.unsafeFromEpochSecond(epochSecondExpiredAt)
    }

  private def responseCookieToRequestCookie(r: ResponseCookie): RequestCookie =
    RequestCookie(r.name, r.content)

  private def cookieAppliesToRequest[N[_]](r: Request[N], c: ResponseCookie): Boolean = {
    val domainApplies = c.domain.forall(s => r.uri.host.forall(host => host.value.contains(s)))
    val pathApplies = c.path.forall(s => r.uri.path.contains(s))
    domainApplies && pathApplies
  }

  private def cookiesForRequest[N[_]](
    r: Request[N],
    l: List[ResponseCookie]
  ): List[RequestCookie] = l.foldLeft(List.empty[RequestCookie]){case (list, cookie) => 
    if (cookieAppliesToRequest(r, cookie)) responseCookieToRequestCookie(cookie) :: list
    else list
  }

}