/*
 * Copyright 2019 Lucas Satabin
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
package fs2.data.csv

import cats._

import scala.annotation.implicitNotFound

/** Describes how a row can be encoded from a value of the given type.
  */
@implicitNotFound(
  "No implicit CsvRowEncoder found for type ${T}.\nYou can define one using CsvRowEncoder.instance, by calling contramap on another CsvRowEncoder or by using generic derivation for product types like case classes.\nFor that, add the fs2-data-csv-generic module to your dependencies and use either full-automatic derivation:\nimport fs2.data.csv.generic.auto._\nor the recommended semi-automatic derivation:\nimport fs2.data.csv.generic.semiauto._\nimplicit val csvRowEncoder: CsvRowEncoder[${T}] = deriveCsvRowEncoder\nMake sure to have instances of CellEncoder for every member type in scope.\n")
trait CsvRowEncoder[T, Header] {
  def apply(elem: T): CsvRow[Header]

  def contramap[B](f: B => T): CsvRowEncoder[B, Header] = elem => apply(f(elem))
}

object CsvRowEncoder extends ExportedCsvRowEncoders {

  implicit def CsvRowEncoderContravariant[Header]: Contravariant[CsvRowEncoder[*, Header]] =
    new Contravariant[CsvRowEncoder[*, Header]] {
      override def contramap[A, B](fa: CsvRowEncoder[A, Header])(f: B => A): CsvRowEncoder[B, Header] = fa.contramap(f)
    }

  @inline
  def apply[T: CsvRowEncoder[*, Header], Header]: CsvRowEncoder[T, Header] = implicitly[CsvRowEncoder[T, Header]]

  @inline
  def instance[T, Header](f: T => CsvRow[Header]): CsvRowEncoder[T, Header] = f(_)
}

trait ExportedCsvRowEncoders {
  implicit def exportedCsvRowEncoders[A](
      implicit exported: Exported[CsvRowEncoder[A, String]]): CsvRowEncoder[A, String] = exported.instance
}
