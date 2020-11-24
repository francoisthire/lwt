type promise_uid

(** [fresh ()] generates a fresh id. The [id] cannot be one returned
   by [uid_of_static_promise] except if there is an overflow. *)
val fresh : unit -> promise_uid

type static_promise =
  [
    | `Return_unit
    | `Return_none
    | `Return_nil
    | `Return_true
    | `Return_false
  ]

val uid_of_static_promise : static_promise -> promise_uid

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

type wakeup_type =
  | Wakeup_type : ('a, exn) result -> wakeup_type

val is_already_resolved : promise_type -> bool

type tracer =
  {
    create_promise : promise_uid -> promise_type -> unit;
    (** Called when [Lwt] create a promise. A type is associated
        depending on the internal function of [Lwt] which created the
        promise. For [promises] which are allocated once such as
        [Lwt.return_unit], a the identifier is always the same (see
        [Lwt.ml]). *)

    attach_callback : promise_uid -> callback_uid -> callback_type -> unit;
    (** Called when [Lwt] attached a callback to a promise. This
        callback can either be created by [Lwt] or provided by the
        user. In that case, the underlying callback provided by the
        user is given in the [callback_type]. WARNING: This may create
        memory leaks if the tracer store the callback. This information
        should be used with care, for example to get the position of
        the function using the [Owee] library. *)

    wakeup : promise_uid -> wakeup_type -> unit;
    (** Called when a promise is woken up. WARNING: As for
        [attach_callback] the result value should not be stored
        otherwise it could cause memory leaks. *)

    cancel : promise_uid -> unit;
    (** Called when a promise is canceled. *)

    resolve : callback_uid -> unit;
    (** Called just before calling a callback. *)

    proxy : promise_uid -> promise_uid -> unit;
    (** Proxy p p' means that p is a proxy for p'. This means they
        should behave the same way. *)

    detach_callback : promise_uid -> callback_uid -> unit;
    (** Called when a callback is detached from a promise *)

    promise_destroy : promise_uid -> unit;
    (** Called when a promise is destroyed by the GC. *)

    callback_destroy : callback_uid -> unit
    (** Called when a callback created by Lwt is destroyed by the
        GC. This callback can be a wrapper around a callback given by
        the user via a function such as [Lwt.bind]. *)
  }

val null_tracer : tracer
(** Each callback is a noop *)

val tracer : tracer ref
