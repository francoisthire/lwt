type promise_uid = int

type promise_type =
  [
    | `Return
    | `Fail
    | `Wait
    | `Task
    | `Sequence
    | `Protected
    | `No_cancel
    | `Bind
    | `Backtrace_bind
    | `Map
    | `Catch
    | `Backtrace_catch
    | `Try_bind
    | `Backtrace_try_bind
    | `Join
    | `Choose
    | `Pick
    | `Nchoose
    | `Npick
    | `Nchoose_split
  ]

val is_already_resolved : promise_type -> bool

type tracer =
  {
    create_promise : promise_uid -> promise_type -> unit;

    create_callback : promise_uid -> Owee_location.t -> unit;

    callback_binded : promise_uid -> Owee_location.t -> unit;

    resolved : promise_uid -> exn option -> unit;

    proxy : promise_uid -> promise_uid -> unit;

    promise_destroy : promise_uid -> unit;

    callback_destroy : Owee_location.t -> unit
  }

val null_tracer : tracer

val tracer : tracer ref
