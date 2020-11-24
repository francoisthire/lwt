type promise_uid = int

type static_promise =
  [
    | `Return_unit
    | `Return_none
    | `Return_nil
    | `Return_true
    | `Return_false
  ]

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

(* Counters below 50 are reserved for lwt management *)
let promise_fresh_uid =
  let counter = ref 50 in
  fun () ->
    incr counter;
    !counter

type internal = unit

type attached_strategy = [`Eager | `Deferred]

type fulfilled_or_pending =
  | FPFulfilled : attached_strategy * ('a -> 'b) -> fulfilled_or_pending
  | FPPending : ('a -> 'b) -> fulfilled_or_pending

type rejected_or_pending =
  | RPRejected : attached_strategy * ('a -> 'b) -> rejected_or_pending
  | RPPending : ('a -> 'b) -> rejected_or_pending

type fulfilled_or_rejected_or_pending =
  | FRPFulfilled : attached_strategy * ('a -> 'b) -> fulfilled_or_rejected_or_pending
  | FRPRejected : attached_strategy * ('c -> 'd) -> fulfilled_or_rejected_or_pending
  | FRPPending : ('a -> 'b) * ('c -> 'd) -> fulfilled_or_rejected_or_pending

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

type wakeup_type =
  | Wakeup_type : ('a, exn) result -> wakeup_type

type callback_uid = int

let callback_fresh_uid =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

type tracer =
  {
    create_promise : promise_uid -> promise_type -> unit;

    attach_callback : 'a. promise_uid -> callback_uid -> 'a callback_type -> 'a -> unit;

    wakeup : promise_uid -> wakeup_type -> unit;

    cancel : promise_uid -> unit;

    resolve : 'a 'b. callback_uid -> ('a -> 'b) -> 'a -> 'b;

    proxy : promise_uid -> promise_uid -> unit;

    detach_callback : promise_uid -> callback_uid -> unit;

    promise_collected : promise_uid -> unit;

    callback_collected : callback_uid -> unit;
  }


let null_tracer =
  {
    create_promise     = (fun _ _ -> ());

    attach_callback    = (fun _ _ _ _ -> ());

    wakeup             = (fun _ _ -> ());

    cancel             = (fun _ -> ());

    resolve            = (fun _ x -> x);

    proxy              = (fun _ _ -> ());

    detach_callback    = (fun _ _ -> ());

    promise_collected  = (fun _ -> ());

    callback_collected = (fun _ -> ());
  }

let tracer = ref null_tracer


let [@inline] create_promise promise_type =
  let uid =
    match promise_type with
    | `Return_unit  -> 0
    | `Return_none  -> 1
    | `Return_nil   -> 2
    | `Return_true  -> 3
    | `Return_false -> 4
    |  _ -> promise_fresh_uid ()
  in
  !tracer.create_promise uid promise_type;
  uid


let attach_callback promise_uid callback_type payload =
  let uid = callback_fresh_uid () in
  !tracer.attach_callback promise_uid uid callback_type payload;
  uid
[@@ inlined]

let attach_callback_list promise_uid_list callback_type payload =
  let uid = callback_fresh_uid () in
  List.iter (fun promise_uid ->
      !tracer.attach_callback promise_uid uid callback_type payload) promise_uid_list;
  uid
[@@ inlined]

let detach_callback promise_uid callback_uid =
  !tracer.detach_callback promise_uid callback_uid
[@@ inlined]

let proxy ~proxy promise_uid =
  !tracer.proxy proxy promise_uid
[@@ inlined]

let resolve callback_uid f =
  !tracer.resolve callback_uid f
[@@ inlined]

let cancel promise_uid =
  !tracer.cancel promise_uid
[@@ inlined]

let wakeup promise_uid result =
  !tracer.wakeup promise_uid result
[@@ inlined]

let promise_collected promise_uid =
  !tracer.promise_collected promise_uid
[@@ inlined]

let callback_collected callback_uid =
  !tracer.callback_collected callback_uid
[@@ inlined]
