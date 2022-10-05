/*
 * Copyright 2019-2022 Lucas Satabin
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

/** Contains a DSL to build a json selector. Start a selector with `root` and
  * then chain the rest.
  *
  * {{{
  * val sel = root.index(10).?.fields("a", "b").compile
  * }}}
  */
package object selector {

  /** The root selector builder, selects the root of the stream, i.e. all tokens.
    * This is the entry point for the selector DSL.
    */
  def root: RootBuilder.type = RootBuilder

}
