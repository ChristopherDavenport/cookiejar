package io.chrisdavenport.cookiejar

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.concurrent._
import org.http4s._
import org.http4s.client.Client
import scala.concurrent.duration._

/**
 * A CookieJar Middleware which enriches requests 
 * and extracts `Set-Cookies` from the
 * responses to be available for the next client calls.
 * 
 * Implements what is outlined in 
 * https://tools.ietf.org/html/rfc6265
 * 
 */
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
    in[F, F](c)

  def in[F[_]: Sync: Timer, G[_]: Sync](c: Client[F]): G[Client[F]] = 
    CookieJarAlg.in[F, G].map(apply(_)(c))

  /**
   * Algebra for Interfacing with the Cookie Jar
   **/
  trait CookieJarAlg[F[_]]{
    /**
     * Default Expiration Approach, Removes Expired Cookies
     */
    def evictExpired: F[Unit]

    /**
     * Available for Use To Relieve Memory Pressure
     */
    def evictAll: F[Unit]

    /**
     * Add Cookie to the cookie jar
     */
    def addCookie(c: ResponseCookie): F[Unit]

    /**
     * Enrich a Request with the cookies available
     */
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

  private[cookiejar] final case class CookieKey(
    name: String,
    domain: Option[String],
    path: Option[String]
  )

  private[cookiejar] final case class CookieValue(
    setAt: HttpDate,
    cookie: ResponseCookie
  )

  private[cookiejar] def currentHttpDate[F[_]: Clock: MonadError[?[_], Throwable]] = 
    Clock[F].monotonic(SECONDS)
      .flatMap(s => HttpDate.fromEpochSecond(s).liftTo[F])

  private[cookiejar] def keyFromRespCookie(c: ResponseCookie): CookieKey =
    CookieKey(c.name, c.domain, c.path)

  private[cookiejar] def extractFromResponseCookie[F[_]](
    m: Map[CookieKey,CookieValue]
  )(c: ResponseCookie, httpDate: HttpDate): Map[CookieKey, CookieValue] = 
    m + (keyFromRespCookie(c) -> CookieValue(httpDate, c))

  private[cookiejar] def isExpiredByExpiration(
    now: HttpDate
  )(m: (CookieKey, CookieValue)): Boolean = 
    m._2.cookie.expires.exists(expiresAt => expiresAt <= now)

  private[cookiejar] def isExpiredByMaxAge(
    now: HttpDate
  )(m: (CookieKey, CookieValue)): Boolean = 
    m._2.cookie.maxAge.exists{plusSeconds => 
      val epochSecondExpiredAt = m._2.setAt.epochSecond + plusSeconds
      HttpDate.unsafeFromEpochSecond(epochSecondExpiredAt) <= now
    }

  private[cookiejar] def responseCookieToRequestCookie(r: ResponseCookie): RequestCookie =
    RequestCookie(r.name, r.content)

  private[cookiejar] def cookieAppliesToRequest[N[_]](r: Request[N], c: ResponseCookie): Boolean = {
    val domainApplies = c.domain.exists(s => r.uri.host.forall{authority => 
      authority.renderString.contains(s)
    })
    val pathApplies = c.path.forall(s => r.uri.path.contains(s))
    domainApplies && pathApplies
  }

  private[cookiejar] def cookiesForRequest[N[_]](
    r: Request[N],
    l: List[ResponseCookie]
  ): List[RequestCookie] = l.foldLeft(List.empty[RequestCookie]){case (list, cookie) => 
    if (cookieAppliesToRequest(r, cookie)) responseCookieToRequestCookie(cookie) :: list
    else list
  }

}