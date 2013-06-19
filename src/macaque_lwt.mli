(*
Copyright (c) 2013 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

module Lwt_thread : sig
  include module type of Lwt
  include module type of Lwt_chan
end

module Lwt_PGOCaml : PGOCaml_generic.PGOCAML_GENERIC with type 'a monad = 'a Lwt.t
module Lwt_Query : Query.QUERY with type 'a Db.monad = 'a Lwt.t

module type CONFIG = sig
  val connect : unit -> 'a Lwt_PGOCaml.t Lwt.t
  val pool_number : int
end

module Make : functor (Config : CONFIG) -> sig
  class type ['a, 'b] macaque_type = object
    method get : unit
    method nul : 'b
    method t : 'a
  end

  type ('a, 'b) t = ('a, 'b) macaque_type Sql.t

  val view : ?log:out_channel -> ('a, _) Sql.view -> 'a list Lwt.t

  val view_one : ?log:out_channel -> ('a, _) Sql.view -> 'a Lwt.t

  val view_opt : ?log:out_channel -> ('a, _) Sql.view -> 'a option Lwt.t

  val query : ?log:out_channel -> 'a Sql.query -> 'a Lwt.t

  val value :
    ?log:out_channel ->
    < nul : Sql.non_nullable; t : 'a #Sql.type_info; .. > Sql.t ->
    'a Lwt.t

  val value_opt :
    ?log:out_channel ->
    < nul : Sql.nullable; t : 'a #Sql.type_info; .. > Sql.t ->
    'a option Lwt.t

  module Low_level : sig
    val inject :
      ?name:string ->
      ?log:out_channel ->
      string ->
      Lwt_PGOCaml.row list Lwt.t
    val alter : ?name:string -> ?log:out_channel -> string -> unit Lwt.t
  end
end

module Utils : sig
  val in' :
    < nul : 'a; t : 'b; .. > Sql.t ->
    < nul : 'a; t : 'b; .. > Sql.t list ->
    < nul : 'a; t : Sql.bool_t > Sql.t
end
