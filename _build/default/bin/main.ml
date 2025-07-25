(* open! Core
open! Async

let do_a_thing (n : int) : unit Deferred.t =
  let%bind () = Clock_ns.after (Time_ns_unix.Span.of_int_sec n) in
  Core.printf "Finished waiting %i seconds\n%!" n;
  return ()
;;

let main () =
  print_endline "Hello, World!";
  Graphics.open_graph " 10000 x 10000 ";
  let black = Graphics.rgb 000 000 000 in
  Graphics.set_color black;
  Graphics.fill_rect 0 0 10000 10000;
  (* (let%bind () = Clock_ns.after (Time_ns_unix.Span.of_int_sec 10))  in *)
  let%bind () = do_a_thing 10 in
  Graphics.close_graph ();
  return ()
;;

let command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return () in
     fun () -> main ())
;;

let () = Command_unix.run command *)

open! Core
open! Physics_engine_lib

let () =
  Run.run ();
  Core.never_returns (Async.Scheduler.go ())
;;