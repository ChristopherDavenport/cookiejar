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
object CookieJarMiddleware {

  def apply[F[_]: Sync](
    alg: CookieJar[F]
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
            .traverse_(alg.addCookie(_, req.uri))
        )
      } yield out
    }

  def impl[F[_]: Sync: Timer](c: Client[F]): F[Client[F]] = 
    in[F, F](c)

  def in[F[_]: Sync: Timer, G[_]: Sync](c: Client[F]): G[Client[F]] = 
    CookieJar.in[F, G].map(apply(_)(c))

}

  /**
   * Algebra for Interfacing with the Cookie Jar
   **/
  trait CookieJar[F[_]]{
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
    def addCookie(c: ResponseCookie, uri: Uri): F[Unit]

    /**
     * Enrich a Request with the cookies available
     */
    def enrichRequest[G[_]](r: Request[G]): F[Request[G]]

  }

  object CookieJar {
    def impl[F[_]: Sync: Clock]: F[CookieJar[F]] = 
      in[F, F]

    def in[F[_]: Sync: Clock, G[_]: Sync]: G[CookieJar[F]] = 
      Ref.in[G, F, Map[CookieKey, CookieValue]](Map.empty).map{ ref => 
        new CookieJar[F]{
          override def evictExpired: F[Unit] = for {
            now <- currentHttpDate
            out <- ref.update(
              _.filterNot{t => 
                now <= t._2.expiresAt
              }
            )
          } yield out

          override def evictAll: F[Unit] = ref.set(Map.empty)

          override def addCookie(c: ResponseCookie, uri: Uri): F[Unit] = for {
            now <- currentHttpDate
            out <- ref.update(extractFromResponseCookie(_)(c, now, uri))
          } yield out



          override def enrichRequest[N[_]](r: Request[N]): F[Request[N]] = 
            for {
              cookies <- ref.get.map(_.map(_._2.cookie).toList)
            } yield cookiesForRequest(r, cookies)
              .foldLeft(r){ case (req, cookie) => req.addCookie(cookie)}
        }
      }

      private[cookiejar] final case class CookieKey(
        name: String,
        domain: String,
        path: Option[String]
      )
    
      final class CookieValue(
        val setAt: HttpDate,
        val expiresAt: HttpDate,
        val cookie: ResponseCookie
      ){
        override def equals(obj: Any): Boolean = obj match {
          case c: CookieValue => 
            setAt == c.setAt &&
              expiresAt == c.expiresAt &&
              cookie == c.cookie
          case _ => false
        }
      }

      object CookieValue  {
        def apply(
          setAt: HttpDate,
          expiresAt: HttpDate,
          cookie: ResponseCookie
        ): CookieValue = new CookieValue(setAt, expiresAt, cookie)
      }
    
      private[cookiejar] def currentHttpDate[F[_]: Clock: MonadError[?[_], Throwable]] = 
        Clock[F].monotonic(SECONDS)
          .flatMap(s => HttpDate.fromEpochSecond(s).liftTo[F])

      private[cookiejar] def expiresAt(
        now: HttpDate,
        c: ResponseCookie,
        default: HttpDate
      ): HttpDate = c.expires.orElse(
        c.maxAge.flatMap(seconds => 
          HttpDate.fromEpochSecond(now.epochSecond + seconds).toOption
        )
      ).getOrElse(default)

      private[cookiejar] def extractFromResponseCookie[F[_]](
        m: Map[CookieKey,CookieValue]
      )(c: ResponseCookie, httpDate: HttpDate, uri: Uri): Map[CookieKey, CookieValue] = {
        c.domain.orElse(uri.host.map(_.value))  match {
          case Some(domainS) => 
            val key = CookieKey(c.name, domainS, c.path)
            val newCookie = c.copy(domain = domainS.some)
            val expires: HttpDate = expiresAt(httpDate, c, HttpDate.MaxValue)
            val value = CookieValue(httpDate, expires, newCookie)
            m + (key -> value)
          case None => // Ignore Cookies We Can't get a domain for
            m
        }
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



