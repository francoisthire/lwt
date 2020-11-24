type promise_uid = int

type attached_strategy = [`Eager | `Deferred | `Pending]

type state =
  | TFulfilled : ('a -> 'b) -> state
  | TRejected  : ('a -> 'b) -> state
  | TPending   : ('a -> 'b) * ('c -> 'd) -> state

type callback_type =
  | Add_task_r
  | Add_task_l
  | Protected
  | Cancel
  | No_cancel
  | Bind : attached_strategy * ('a -> 'b) -> callback_type
  | Backtrace_bind : attached_strategy * ('a -> 'b) -> callback_type
  | Map : attached_strategy * ('a -> 'b) -> callback_type
  | Catch : attached_strategy * ('a -> 'b) -> callback_type
  | Backtrace_catch : attached_strategy * ('a -> 'b) -> callback_type
  | Try_bind : attached_strategy * state  -> callback_type
  | Backtrace_try_bind : attached_strategy * state -> callback_type
  | On_cancel : attached_strategy * ('a -> 'b) -> callback_type
  | On_success : attached_strategy * ('a -> 'b) -> callback_type
  | On_failure : attached_strategy * ('a -> 'b) -> callback_type
  | On_termination : attached_strategy * ('a -> 'b) -> callback_type
  | On_any : attached_strategy * state  -> callback_type
  | Async
  | Ignore_result
  | Join
  | Choose
  | Pick
  | Nchoose
  | Npick
  | Nchoose_split

type callback_uid = int

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
  | `Pause
  ]

let is_already_resolved = function
  | `Return
  | `Fail -> true
  | _ -> false


type tracer =
  {
    create_promise : promise_uid -> promise_type -> unit;

    attach_callback : promise_uid -> callback_uid -> callback_type -> unit;

    wakeup : promise_uid -> exn option -> unit;

    cancel : promise_uid -> unit;

    resolve : callback_uid -> unit;

    proxy : promise_uid -> promise_uid -> unit;

    detach_callback : promise_uid -> callback_uid -> unit;

    promise_destroy : promise_uid -> unit;

    callback_destroy : callback_uid -> unit
  }

let null_tracer =
  {
    create_promise = (fun _ _ -> ());

    attach_callback = (fun _ _ _ -> ());

    wakeup = (fun _ _ -> ());

    cancel = (fun _ -> ());

    resolve = (fun _ -> ());

    proxy = (fun _ _ -> ());

    detach_callback = (fun _ _ -> ());

    promise_destroy = (fun _ -> ());

    callback_destroy = (fun _ -> ());

  }

let tracer = ref null_tracer
