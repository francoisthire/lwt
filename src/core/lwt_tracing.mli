(** {1: Lwt interface} *)

(** Unique identifier for promises *)
type promise_uid



(** Enumeration of static promises in [Lwt]. A static promise is a
   promise which is allocated at top level by [Lwt]. *)
type static_promise =
  [
    | `Return_unit  (** uid: 1 *)
    | `Return_none  (** uid: 2 *)
    | `Return_nil   (** uid: 3 *)
    | `Return_true  (** uid: 4 *)
    | `Return_false (** uid: 5 *)
  ]


(** Type of promises *)
type promise_type =
  [
    | static_promise
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

(** [Lwt] creates its own callbacks. These callbacks are not exposed
   to the user. *)

type internal = unit

(** For sequential functions, [Lwt] uses two strategies to attach a
   callback to a resolved promise:

    - [Eager]: The callback is resolved immediatly.

    - [Deferred]: The stack of callbacks is full. The call to the
   callback is consequently deferred.

    Any callback attached with the strategy [`Eager] is not wrapped by
   [Lwt] and therefore the tracer will never called
   [callback_collected] on it. It is the responsability of the user to
   do that. The reason is because the same callback can be given
   several times to bind and such occurence cannot be detected by
   [Lwt] itself. This is to avoid to attach several times callbacks
   function to the same callback via [Gc.finalise_last].

   For callbacks with the [Deferred] strategy or callbacks attached to
   promises in [Pending] state, callbacks given by the user are
   wrapped by [Lwt] except for [On_cancel]. For these callbacks, the
   tracer may record when they are collected by the GC. But id does
   not mean that the underlying callback will be garbage
   collected. The motto is every callback which comes from the
   external world is not recorded to be collected by the tracer. This
   is a choice is debatable.

   Of course, this design also uncovered [lwt] internal design which
   may be useful for debugging. *)

type attached_strategy = [`Eager | `Deferred]

(** [fulfilled_or_pending] represents callbacks that can be attached
   only when the promise is in [Fulfilled] or [Pending]. *)

type fulfilled_or_pending =
  | FPFulfilled : attached_strategy * ('a -> 'b) -> fulfilled_or_pending
  | FPPending : ('a -> 'b) -> fulfilled_or_pending

(** [rejected_or_pending] represents callbacks that can be attached
   only when the promise is in [Rejected] or [Pending]. *)

type rejected_or_pending =
  | RPRejected : attached_strategy * ('a -> 'b) -> rejected_or_pending
  | RPPending : ('a -> 'b) -> rejected_or_pending

(** [fullfilled_or_rejected_or_pending] represents callbacks that can
   be attached when the promise is [Fulfilled], [Rejected] or
   [Pending]. The callback attached to [Fulfilled] or [Rejected] may
   be different. Except for [On_termination] the callbacks of
   [FRPPending] are different a priori. *)

type fulfilled_or_rejected_or_pending =
  | FRPFulfilled : attached_strategy * ('a -> 'b) -> fulfilled_or_rejected_or_pending
  | FRPRejected : attached_strategy * ('c -> 'd) -> fulfilled_or_rejected_or_pending
  | FRPPending : ('a -> 'b) * ('c -> 'd) -> fulfilled_or_rejected_or_pending

(** Type of a callback depending in which [lwt] function it is attached. *)

type 'a callback_type =
  | Add_task_r         : internal callback_type
  | Add_task_l         : internal callback_type
  | Protected          : internal callback_type
  | Cancel             : internal callback_type
  | No_cancel          : internal callback_type
  | Bind               : fulfilled_or_pending callback_type
  | Backtrace_bind     : fulfilled_or_pending callback_type
  | Map                : fulfilled_or_pending callback_type
  | Catch              : rejected_or_pending  callback_type
  | Backtrace_catch    : rejected_or_pending  callback_type
  | Try_bind           : fulfilled_or_rejected_or_pending callback_type
  | Backtrace_try_bind : fulfilled_or_rejected_or_pending callback_type
  | On_cancel          : rejected_or_pending callback_type
  | On_success         : fulfilled_or_pending callback_type
  | On_failure         : rejected_or_pending callback_type
  | On_termination     : fulfilled_or_rejected_or_pending callback_type
  | On_any             : fulfilled_or_rejected_or_pending callback_type
  | Async              : internal callback_type
  | Ignore_result      : internal callback_type
  | Join               : internal callback_type
  | Choose             : internal callback_type
  | Pick               : internal callback_type
  | Nchoose            : internal callback_type
  | Npick              : internal callback_type
  | Nchoose_split      : internal callback_type

(** Contain the value given to a resolve promise *)

type wakeup_type =
  | Wakeup_type : ('a, exn) result -> wakeup_type


(** Unique identifiers for callback.  WARNING: If a function is called
   several times with the same callback it won't have the same uniquer
   identifier. However, using ['a callback_type] it may be possible to
   trace the same callback via their position using the [owee] library
   for example. *)

type callback_uid

(** A wrapper for [tracer.create_promise]. *)
val create_promise : promise_type -> promise_uid

(** A wrapper for [attack_callback]. *)
val attach_callback : promise_uid -> 'a callback_type -> 'a -> callback_uid

(** A wrapper for [attack_callback]. *)
val attach_callback_list : promise_uid list -> 'a callback_type -> 'a -> callback_uid

(** A wrapper for [proxy]. *)
val proxy : proxy:promise_uid -> promise_uid -> unit

(** A wraper for [resolve]. *)
val resolve : callback_uid -> ('a -> 'b) -> 'a -> 'b

(** A wraper for [cancel]. *)
val cancel : promise_uid -> unit

(** A wraper for [wakeup]. *)
val wakeup : promise_uid -> wakeup_type -> unit

(** A wrapper for [detach_callback]. *)
val detach_callback : promise_uid -> callback_uid -> unit

(** A wrapper for [promise_collected]. *)
val promise_collected : promise_uid -> unit

(** A wrapper for [callback_collected]. *)
val callback_collected : callback_uid -> unit


(** {2: User interface} *)

(** A user can implement a tracer. [Lwt] calls callbacks of this
   tracer when the corresponding events occurs. *)
type tracer =
  {
    create_promise : promise_uid -> promise_type -> unit;
    (** Called when [Lwt] create a promise. A type is associated
        depending on the internal function of [Lwt] which created the
        promise. For [promises] which are allocated once such as
        [Lwt.return_unit], a the identifier is always the same (see
        [Lwt.ml]). *)

    attach_callback : 'a. promise_uid -> callback_uid -> 'a callback_type -> 'a -> unit;
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

    resolve : 'a 'b. callback_uid -> ('a -> 'b) -> 'a -> 'b;
    (** Called just before calling a callback. It gives the
       opportunity to wrap the callback for profiling. WARNING: As for
       attach_callback, this may create a memory leak if the callback
       is stored. *)

    proxy : promise_uid -> promise_uid -> unit;
    (** Proxy p p' means that p is a proxy for p'. This means they
        should behave the same way. *)

    detach_callback : promise_uid -> callback_uid -> unit;
    (** Called when a callback is detached from a promise *)

    promise_collected : promise_uid -> unit;
    (** Called when a promise is collected by the GC. *)

    callback_collected : callback_uid -> unit;
    (** Called when a callback created by Lwt is collected by the
       GC. This callback can be a wrapper around a callback given by
       the user via a function such as [Lwt.bind]. See
       [attached_strategy] for more information. *)

  }

(** Each callback is a noop except for hook_promise which is the identity function. *)
val null_tracer : tracer

(** Tracer reference that can be changed by the user *)
val tracer : tracer ref
