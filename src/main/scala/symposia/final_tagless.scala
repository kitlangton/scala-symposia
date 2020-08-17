package symposia

object final_tagless {
  trait Sync[F[_]] {
    def delay[A](a: => A): F[A]
  }

  def later[F[_]](string: String)(implicit sync: Sync[F]): F[String] = sync.delay(string)

  def funny[F[_]: Sync]: F[String] = later("hi")
}
