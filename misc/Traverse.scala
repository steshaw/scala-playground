//
// See response from Tony Morris here:
//     http://stackoverflow.com/questions/1722726/is-the-scala-2-8-collections-library-a-case-of-the-longest-suicide-note-in-histo
//

// Hmmm, just doesn't compile...
def traverse[A, B](f: A => F[B], a: T[A])(implicit t: Traversable[T], ap: Applicative[F]): F[T[B]]
