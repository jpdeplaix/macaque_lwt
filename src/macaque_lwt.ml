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

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

module type CONFIG = sig
  val connect : unit -> 'a Lwt_PGOCaml.t Lwt.t
  val pool_number : int
end

module Make (Config : CONFIG) = struct
  let (>>=) = Lwt.(>>=)

  class type ['a, 'b] macaque_type = object
    method get : unit
    method nul : 'b
    method t : 'a
  end

  type ('a, 'b) t = ('a, 'b) macaque_type Sql.t
  type db_t = Lwt_PGOCaml.pa_pg_data Lwt_PGOCaml.t

  let validate db =
    Lwt.try_bind
      (fun () -> Lwt_PGOCaml.ping db)
      (fun () -> Lwt.return true)
      (fun _ -> Lwt.return false)

  let pool = Lwt_pool.create Config.pool_number ~validate Config.connect

  let exec f ?log x = Lwt_pool.use pool (fun db -> f db ?log x)

  let view ?log x = exec Lwt_Query.view ?log x
  let view_opt ?log x = exec Lwt_Query.view_opt ?log x
  let view_one ?log x = exec Lwt_Query.view_one ?log x
  let query ?log x = exec Lwt_Query.query ?log x
  let value ?log x = exec Lwt_Query.value ?log x
  let value_opt ?log x = exec Lwt_Query.value_opt ?log x

  let transaction_block ?log f =
    let aux db f =
      Lwt_PGOCaml.begin_work db >>= fun _ ->
      Lwt.catch
        (fun () ->
           f () >>= fun r ->
           Lwt_PGOCaml.commit db >>= fun () ->
           Lwt.return r
        )
        (fun e ->
           Lwt_PGOCaml.rollback db >>= fun () ->
           Lwt.fail e
        )
    in
    exec (fun db ?log () -> aux db (fun () -> f ?log db)) ?log ()

  module Low_level = struct
    let inject ?(name="query") =
      let aux db ?log query =
        (match log with
          | None -> ()
          | Some out -> Printf.fprintf out "%s\n%!" query
        );
        Lwt_PGOCaml.prepare db ~query ~name () >>= fun () ->
        Lwt_PGOCaml.execute db ~name ~params:[] () >>= fun ret ->
        Lwt_PGOCaml.close_statement db ~name () >>= fun () ->
        Lwt.return ret
      in
      exec aux

    let alter ?name ?log query =
      inject ?name ?log query >>= fun _ -> Lwt.return ()
  end
end

module Utils = struct
  let rec in' value = function
    | [] -> Sql.Value.bool false
    | x::xs -> Sql.Op.(||) (Sql.Op.(=) x value) (in' value xs)
end
