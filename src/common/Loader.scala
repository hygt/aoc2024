package common

import scala.io.Source
import scala.util.Using

object Loader:

  def load[T: Decoder](resourcePath: String): Seq[T] =
    Using.resource(Source.fromResource(resourcePath)): resource =>
      decode[T](resource.getLines)

  def decode[T: Decoder](sample: String): Seq[T] =
    decode[T](sample.linesIterator)

  private def decode[T: Decoder](lines: Iterator[String]): Seq[T] =
    lines.filterNot(_.isBlank).map(_.trim.nn.decoded).toSeq

  def entire[T: Decoder](resourcePath: String): T =
    Using.resource(Source.fromResource(resourcePath)): resource =>
      resource.mkString.trim.nn.decoded
