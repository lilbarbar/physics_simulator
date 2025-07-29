open! Core
open! Physics_engine_lib

let () =
  Run.run ();
  Core.never_returns (Async.Scheduler.go ())
;;