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
  | On_any : attached_strategy * state -> callback_type
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

val is_already_resolved : promise_type -> bool

type tracer =
  {
    create_promise : promise_uid -> promise_type -> unit;
    (** Called when [Lwt] create a promise. *)

    attach_callback : promise_uid -> callback_uid -> callback_type -> unit;
    (** Called when [Lwt] attached a callback to a promise. *)

    wakeup : promise_uid -> exn option -> unit;
    (** Called when a promise is waked up. *)

    cancel : promise_uid -> unit;
    (** Called when a promise is canceled. *)

    resolve : callback_uid -> unit;

    proxy : promise_uid -> promise_uid -> unit;
    (** Proxy p p' means that p is a proxy for p' *)

    detach_callback : promise_uid -> callback_uid -> unit;

    promise_destroy : promise_uid -> unit;
    (** Called when a promise is destroyed by the GC *)

    callback_destroy : callback_uid -> unit
    (** Called when a callback created by Lwt is destroyed by the GC *)
  }

val null_tracer : tracer

val tracer : tracer ref
