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

let is_already_resolved = function
  | `Return
  | `Fail -> true
  | _ -> false


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

let null_tracer =
  {
    create_promise = (fun _ _ -> ());

    create_callback = (fun _ _ -> ());

    callback_binded = (fun _ _ -> ());

    resolved = (fun _ _ -> ());

    proxy = (fun _ _ -> ());

    promise_destroy = (fun _ -> ());

    callback_destroy = (fun _ -> ());

  }

let tracer = ref null_tracer
