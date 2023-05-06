/*
 * Copyright 2023 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fs2
package data
package json
package tagged

import scala.collection.mutable.ListBuffer
import cats.Show
import cats.syntax.all._

private[json] sealed trait TaggedJson
private[json] object TaggedJson {
  case object StartJson extends TaggedJson
  case object EndJson extends TaggedJson
  case class Raw(token: Token) extends TaggedJson
  case class StartArrayElement(idx: Int) extends TaggedJson
  case object EndArrayElement extends TaggedJson
  case class StartObjectValue(name: String) extends TaggedJson
  case object EndObjectValue extends TaggedJson

  implicit object show extends Show[TaggedJson] {

    override def show(t: TaggedJson): String =
      t match {
        case Raw(token)             => token.jsonRepr
        case StartArrayElement(idx) => show".[$idx]"
        case EndArrayElement        => ".[/]"
        case StartObjectValue(name) => show".{$name}"
        case EndObjectValue         => ".{/}"
        case _                      => ""
      }

  }

}

/** Tags json tokens in an xml like fashion, with explicit open and close tags for points
  * of interest. This allows for implementing interesting queries with a simple tree automaton.
  */
private[json] object JsonTagger {
  def pipe[F[_]: RaiseThrowable]: Pipe[F, Token, TaggedJson] = {

    def object_(chunk: Chunk[Token], idx: Int, rest: Stream[F, Token], chunkAcc: ListBuffer[TaggedJson])
        : Pull[F, TaggedJson, (Chunk[Token], Int, Stream[F, Token], ListBuffer[TaggedJson])] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.from(chunkAcc.result())) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.clear()
            object_(hd, 0, tl, chunkAcc)
          case None =>
            Pull.raiseError(JsonException("unexpected end of input"))
        }
      } else {
        chunk(idx) match {
          case Token.Key(name) =>
            value_(chunk, idx + 1, rest, chunkAcc += TaggedJson.StartObjectValue(name)).flatMap {
              case (chunk, idx, rest, chunkAcc) =>
                object_(chunk, idx, rest, chunkAcc += TaggedJson.EndObjectValue)
            }
          case Token.EndObject =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += TaggedJson.Raw(Token.EndObject)))
          case tok =>
            Pull.output(Chunk.from(chunkAcc.result())) >> Pull.raiseError(JsonException(s"unexpected JSON token $tok"))

        }
      }

    def array_(chunk: Chunk[Token], idx: Int, rest: Stream[F, Token], arrayIdx: Int, chunkAcc: ListBuffer[TaggedJson])
        : Pull[F, TaggedJson, (Chunk[Token], Int, Stream[F, Token], ListBuffer[TaggedJson])] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.from(chunkAcc.result())) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.clear()
            array_(hd, 0, tl, arrayIdx, chunkAcc)
          case None =>
            Pull.raiseError(JsonException("unexpected end of input"))
        }
      } else {
        chunk(idx) match {
          case Token.EndArray =>
            Pull.pure((chunk, idx + 1, rest, chunkAcc += TaggedJson.Raw(Token.EndArray)))
          case _ =>
            value_(chunk, idx, rest, chunkAcc += TaggedJson.StartArrayElement(arrayIdx)).flatMap {
              case (chunk, idx, rest, chunkAcc) =>
                array_(chunk, idx, rest, arrayIdx + 1, chunkAcc += TaggedJson.EndArrayElement)
            }
        }
      }

    def value_(chunk: Chunk[Token], idx: Int, rest: Stream[F, Token], chunkAcc: ListBuffer[TaggedJson])
        : Pull[F, TaggedJson, (Chunk[Token], Int, Stream[F, Token], ListBuffer[TaggedJson])] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.from(chunkAcc.result())) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.clear()
            value_(hd, 0, tl, chunkAcc)
          case None =>
            Pull.raiseError(JsonException("unexpected end of input"))
        }
      } else {
        chunk(idx) match {
          case Token.StartObject =>
            object_(chunk, idx + 1, rest, chunkAcc += TaggedJson.Raw(Token.StartObject))
          case Token.StartArray =>
            array_(chunk, idx + 1, rest, 0, chunkAcc += TaggedJson.Raw(Token.StartArray))
          case tok => Pull.pure((chunk, idx + 1, rest, chunkAcc += TaggedJson.Raw(tok)))
        }
      }

    def go_(chunk: Chunk[Token],
            idx: Int,
            rest: Stream[F, Token],
            chunkAcc: ListBuffer[TaggedJson]): Pull[F, TaggedJson, Unit] =
      if (idx >= chunk.size) {
        Pull.output(Chunk.from(chunkAcc.result())) >> rest.pull.uncons.flatMap {
          case Some((hd, tl)) =>
            chunkAcc.clear()
            go_(hd, 0, tl, chunkAcc)
          case None =>
            Pull.done
        }
      } else {
        value_(chunk, idx, rest, chunkAcc += TaggedJson.StartJson).flatMap { case (chunk, idx, rest, chunkAcc) =>
          go_(chunk, idx, rest, chunkAcc += TaggedJson.EndJson)
        }
      }

    go_(Chunk.empty, 0, _, new ListBuffer).stream
  }
}
