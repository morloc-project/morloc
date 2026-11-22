------------------------------ MODULE WorkerPool ------------------------------
(* The runtime's process-wide pool of helper threads that run the spans of *)
(* parallel list calls (WPOOL, model/runtime/workers.md).                   *)
(*                                                                          *)
(* A caller posts a job of spans under the pool lock and works its own job *)
(* by claiming spans from the job's cursor. The first post starts the       *)
(* pool's timer thread; a later post that finds the timer not armed arms it *)
(* and notifies it. The timer, at any time after it sees a job (T after),   *)
(* wakes the job and one sleeping helper, or starts one; each helper that   *)
(* joins a job while woken work stays open passes the wake on to the next   *)
(* sleeper or starts the next helper. A job its caller finishes first wakes *)
(* none, and no helper takes a job before it is woken. If the timer cannot  *)
(* be started, every job still waiting for it is woken at once, and later  *)
(* posts wake their own. The caller removes the job and, if a *)
(* helper is still on it, sleeps until the last one leaves. A helper picks  *)
(* an eligible job under the lock (round-robin), claims spans, runs the     *)
(* job-end hook and leaves the job under the lock; it then picks again, or  *)
(* sleeps, or retires. A span may post a nested job, fail, or fork. A       *)
(* failure is recorded under the job's own lock, after the span's fork      *)
(* generation check. A fork copies the process with only the forking        *)
(* thread. The pool instance is keyed on the fork generation: a post in a   *)
(* new generation gets a fresh instance (FORK-8), and a span that returns   *)
(* or fails in a process whose generation differs from its job's exits      *)
(* there (FORK-17). A caller may exit the process; exit joins no helper.    *)
(*                                                                          *)
(* Thread-local runtime state a span leaves on its thread (ShmTracker       *)
(* entries, closure traces) is `res`, tagged with the job it belongs to.    *)
(* The hook hands the helper's entries to the job, which the caller adds to *)
(* its own tracker, keeps the trace of the job's recorded failure, and      *)
(* drops the rest.                                                          *)
(*                                                                          *)
(* Condition variables are explicit: a sleeping thread moves only when      *)
(* another thread notifies it (or, for a helper, when its idle timeout      *)
(* fires), so a lost wakeup shows as a deadlock or as NoSleepWithWork.      *)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS Variant, Scenario
\* Variant:
\*   "design"            the protocol above
\*   "no_hook"           helpers skip the job-end hook
\*   "hook_drops_trace"  the hook drops every trace of its thread, including
\*                       the one the caller will report
\*   "hook_after_leave"  the hook runs after the helper leaves the job
\*   "flush"             the hook releases tracker entries instead of handing
\*                       them to the job
\*   "no_gen_check"      no fork-generation check (instance or after a span)
\*   "fail_lock_first"   a failing span takes the job lock before its
\*                       generation check
\*   "caller_idle"       a caller does not work its own job
\*   "fifo"              helpers pick the oldest eligible job
\*   "uncapped"          the pool grows to the requested worker count
\*   "exit_joins"        exit waits for every helper to end
\*   "retire_no_dec"     a retiring helper leaves the helper count as it was
\*   "spawn_fail_keeps"  a failed spawn keeps its reservation
\*   "post_no_wake"      the timer's wake wakes no sleeper
\*   "boundary_wake"     a job is woken only while its caller is between spans
\*   "helpers_pick_unwoken" helpers take jobs the timer has not woken
\*   "no_done_notify"    the last helper to leave a job does not wake callers
\*   "post_never_notifies" a post arms the timer but does not notify it
\*   "armed_read_early"  a post reads whether the timer is armed before it
\*                       takes the lock
\*   "fail_wakes_own"    a failed timer start wakes only its own job
\*   "no_cascade"        a helper joining a job does not pass the wake on

Procs == {"P", "C"}   \* the process and its one forked child

\* ---- Scenarios ------------------------------------------------------------
\* nest:     two callers, two helpers; job 1's first span posts job 3; no
\*           failures (nest1, nestfail and forknest have them).
\* nest1:    one caller, one helper; job 1's first span posts job 2; spawn
\*           failure and idle retirement.
\* nestfail: one caller, two helpers; nesting and failures.
\* fair:     two callers with three-span jobs asking for different worker
\*           counts, two helpers.
\* fork:     two callers, one helper; any span, or a caller before its call,
\*           may fork once.
\* forknest: nest1 with one fork, without retirement.
\* exit:     two callers, one helper; a caller that has finished may exit.
\* pair:     one caller, one helper, one job of two spans, each of which
\*           blocks until the other has been claimed.
\* tfail:    pair with two callers and jobs, where the timer's start may fail.
\* cap:      one caller asking for four workers under a ceiling of two, with
\*           one CPU's worth of helpers kept, spawn failure and retirement.
\* wake3:    two callers, three helpers, jobs asking for different worker
\*           counts: a woken helper may take another job than the one it
\*           was woken for.
Callers == IF Scenario \in {"cap", "nest1", "forknest", "nestfail", "pair"} THEN {"c1"} ELSE {"c1", "c2"}
HSeq    == CASE Scenario \in {"nest", "fair", "nestfail", "tfail"} -> <<"h1", "h2">>
           []   Scenario \in {"cap", "wake3"}             -> <<"h1", "h2", "h3">>
           []   OTHER                                     -> <<"h1">>
Helpers == {HSeq[i] : i \in 1..Len(HSeq)}
Timer   == "tm"
Threads == Callers \cup Helpers \cup {Timer}
\* The ceiling on helpers.
Cap     == CASE Scenario = "wake3" -> 3
           []   Scenario \in {"nest", "fair", "cap", "nestfail"} -> 2
           []   OTHER -> 1
\* Helpers past this many retire as soon as they find no work (the CPUs less
\* the caller); the rest retire after an idle timeout.
Soft    == IF Scenario = "cap" THEN 1 ELSE Cap
\* Workers job j asks for (opts.workers or -t).
WJ(j)   == CASE Scenario = "cap"  -> 4
           []   Scenario = "fair" -> (IF j = 1 THEN 3 ELSE 2)
           []   Scenario = "wake3" -> (IF j = 1 THEN 4 ELSE 2)
           []   OTHER             -> Cap + 1
NJ      == CASE Scenario = "nest" -> 3
           []   Scenario \in {"cap", "pair"} -> 1
           []   OTHER             -> 2
Jobs    == 1..NJ
Spans   == CASE Scenario \in {"fair", "nestfail", "wake3"} -> 3
           []   Scenario = "cap"  -> 4
           []   OTHER             -> 2
TopJob(c) == IF c = "c1" THEN 1 ELSE 2
Nest(j, k) == CASE Scenario = "nest" /\ j = 1 /\ k = 1 -> 3
              []   Scenario \in {"nest1", "forknest", "nestfail"} /\ j = 1 /\ k = 1 -> 2
              []   OTHER -> 0
FailOK      == Scenario \in {"nest1", "forknest", "nestfail"}
EscapeOK    == Scenario = "nest1"
ForkOK      == Scenario \in {"fork", "forknest"}
ExitOK      == Scenario = "exit"
RetireOK    == Scenario \in {"nest1", "cap"}
SpawnFailOK == Scenario \in {"nest1", "cap"}
TimerFailOK == Scenario \in {"tfail", "exit"}
Blocking    == Scenario \in {"pair", "tfail"}
TrackBypass == Scenario = "fair"

Min(a, b) == IF a < b THEN a ELSE b
\* The workers a job runs on: its own, at most one per span.
WS(j)   == Min(WJ(j), Spans)
\* Helpers job j may hold: its workers less its caller, under the ceiling.
MaxH(j) == IF Variant = "uncapped" THEN WS(j) - 1 ELSE Min(Cap, WS(j) - 1)
\* Helpers a post of job j grows the pool to.
Grow(j) == IF Variant = "uncapped" THEN WS(j) - 1 ELSE MaxH(j)
GenCheck  == Variant # "no_gen_check"
HookFirst == Variant \notin {"no_hook", "hook_after_leave"}

VARIABLES s, forked
vars == <<s, forked>>

Frame(k, j, st) == [k |-> k, j |-> j, sp |-> 0, st |-> st, fl |-> FALSE, nf |-> FALSE, sa |-> FALSE]

Fresh(live) ==
  [ live |-> live, alive |-> Callers, gen |-> 0, exiting |-> FALSE,
    tstate |-> "absent", tarmed |-> FALSE, tpc |-> "none",
    inst |-> 0, holder |-> "none", queue |-> <<>>, created |-> {}, rr |-> 1,
    count |-> 0, nres |-> 0, woken |-> [j \in Jobs |-> FALSE],
    jst |-> [j \in Jobs |-> "idle"], next |-> [j \in Jobs |-> 1],
    stop |-> [j \in Jobs |-> FALSE], hon |-> [j \in Jobs |-> {}],
    jm |-> [j \in Jobs |-> "none"],
    jgen |-> [j \in Jobs |-> 0], first |-> [j \in Jobs |-> FALSE],
    ftrace |-> [j \in Jobs |-> "none"], handed |-> [j \in Jobs |-> FALSE],
    esc |-> [j \in Jobs |-> {}],
    ran |-> [j \in Jobs |-> [k \in 1..Spans |-> 0]],
    fin |-> [j \in Jobs |-> 0], bypass |-> [j \in Jobs |-> 0],
    stk |-> [t \in Threads |-> <<>>], cpc |-> [c \in Callers |-> "pre"],
    hpc |-> [h \in Helpers |-> "none"], timed |-> [h \in Helpers |-> FALSE],
    res |-> [t \in Threads |-> {}],
    uaf |-> FALSE, late |-> FALSE, lost |-> FALSE ]

Init == /\ s = [p \in Procs |-> Fresh(p = "P")]
        /\ forked = FALSE

Top(p, t) == s[p].stk[t][Len(s[p].stk[t])]
SetTop(q, f) == [q EXCEPT ![Len(q)] = f]
Pop(q) == SubSeq(q, 1, Len(q) - 1)
Busy(p, t) == s[p].live /\ t \in s[p].alive /\ s[p].stk[t] # <<>>
At(p, t, st) == Busy(p, t) /\ Top(p, t).st = st
InQueueOf(P, j) == \E i \in 1..Len(P.queue) : P.queue[i] = j
Remove(q, j) == SelectSeq(q, LAMBDA x : x # j)
EligOf(P) == {j \in Jobs : /\ InQueueOf(P, j)
                           /\ (P.woken[j] \/ Variant = "helpers_pick_unwoken")
                           /\ P.next[j] <= Spans
                           /\ ~P.stop[j]
                           /\ Cardinality(P.hon[j]) < MaxH(j)}
Elig(p) == EligOf(s[p])
JobsOn(p, t) == {s[p].stk[t][i].j : i \in 1..Len(s[p].stk[t])}
LiveHelpers(P) == Helpers \cap P.alive
Sleepers(P) == {h \in LiveHelpers(P) : P.hpc[h] = "sleep"}
Awake(P) == {h \in LiveHelpers(P) : P.hpc[h] \in {"idle", "acq"}}
\* Jobs posted, not yet woken, with spans left.
Waiting(P) == {j \in Jobs : InQueueOf(P, j) /\ ~P.woken[j] /\ P.next[j] <= Spans /\ ~P.stop[j]}
\* Woken jobs a helper could join.
Open(P) == {j \in EligOf(P) : P.woken[j]}
\* Wake job j and pass the wake to one sleeper; the states this can lead to.
WakeSet(P, j) ==
  LET W == [P EXCEPT !.woken[j] = TRUE] IN
  IF Variant = "post_no_wake" \/ Sleepers(W) = {} THEN {W}
  ELSE {[W EXCEPT !.hpc[w] = "idle"] : w \in Sleepers(W)}
\* With no sleeper to wake, start a helper for an open job.
StartFor(P) == Sleepers(P) = {} /\ \E j \in Open(P) : P.count < Grow(j)

\* A fork copies the process; the child has only the forking thread. The
\* fork leaves the parent's pool state as it was, so the parent's behaviours
\* after it are those of the same state with no fork, which the other
\* branch explores; the parent is therefore not explored past the fork,
\* which keeps the search to the sum of the two processes, not the product.
Fork(p, t) ==
  /\ ForkOK /\ p = "P" /\ ~forked
  /\ forked' = TRUE
  /\ s' = [s EXCEPT !["C"] = [s["P"] EXCEPT !.alive = {t}, !.gen = @ + 1,
                                             !.live = TRUE],
                     !["P"].live = FALSE]

\* ---- Callers ---------------------------------------------------------------
Pre(p, c) ==
  /\ c \in Callers /\ s[p].live /\ c \in s[p].alive /\ s[p].cpc[c] = "pre"
  /\ \/ Fork(p, c)
     \/ /\ s' = [s EXCEPT ![p].cpc[c] = "busy",
                          ![p].stk[c] = <<[Frame("own", TopJob(c), "acq_post") EXCEPT !.sa = s[p].tarmed]>>]
        /\ UNCHANGED forked

\* The instance is looked up at the post; a new generation gets a fresh one.
AcqPost(p, t) ==
  LET P == s[p]
      reset == GenCheck /\ P.inst # P.gen
      holder == IF reset THEN "none" ELSE P.holder
  IN /\ At(p, t, "acq_post") /\ holder = "none"
     /\ s' = [s EXCEPT ![p] = [P EXCEPT
                 !.holder = t,
                 !.inst = IF reset THEN P.gen ELSE @,
                 !.queue = IF reset THEN <<>> ELSE @,
                 !.created = IF reset THEN {} ELSE @,
                 !.tstate = IF reset THEN "absent" ELSE @,
                 !.tarmed = IF reset THEN FALSE ELSE @,
                 !.tpc = IF reset THEN "none" ELSE @,
                 !.count = IF reset THEN 0 ELSE @,
                 !.nres = IF reset THEN 0 ELSE @,
                 !.rr = IF reset THEN 1 ELSE @,
                 !.stk[t] = SetTop(@, [Top(p, t) EXCEPT !.st = "post"])]]
     /\ UNCHANGED forked

AfterSpawns == IF Variant = "caller_idle" THEN "idle_wait" ELSE "claim"

\* Under the lock: push the job. The first post starts the timer; one that
\* finds the timer failed wakes its own job; one that finds it not armed arms
\* and notifies it.
Post(p, t) ==
  LET P == s[p]
      f == Top(p, t)
      j == f.j
      P1 == [P EXCEPT
                 !.holder = "none",
                 !.queue = Append(@, j),
                 !.jst[j] = "posted", !.jgen[j] = P.gen, !.next[j] = 1, !.woken[j] = FALSE,
                 !.stop[j] = FALSE, !.hon[j] = {}, !.jm[j] = "none", !.first[j] = FALSE,
                 !.ftrace[j] = "none", !.handed[j] = FALSE, !.esc[j] = {},
                 !.ran[j] = [x \in 1..Spans |-> 0], !.fin[j] = 0,
                 !.bypass[j] = 0]
      armed == IF Variant = "armed_read_early" THEN f.sa ELSE P.tarmed
      Go(Q, st) == [Q EXCEPT !.stk[t] = SetTop(@, [f EXCEPT !.st = st])]
  IN /\ At(p, t, "post")
     /\ CASE P.tstate = "absent" ->
               s' = [s EXCEPT ![p] = Go([P1 EXCEPT !.tstate = "running"], "tstart")]
          [] P.tstate = "failed" ->
               \E W \in WakeSet(P1, j) :
                 s' = [s EXCEPT ![p] = IF StartFor(W)
                                      THEN Go([W EXCEPT !.count = @ + 1, !.nres = @ + 1], "cspawn")
                                      ELSE Go(W, AfterSpawns)]
          [] armed ->
               s' = [s EXCEPT ![p] = Go(P1, AfterSpawns)]
          [] OTHER ->
               s' = [s EXCEPT ![p] = Go([P1 EXCEPT
                       !.tarmed = TRUE,
                       !.tpc = IF Variant # "post_never_notifies" /\ P.tpc \in {"wait_t", "wait_u"}
                               THEN "scan" ELSE @], AfterSpawns)]
     /\ UNCHANGED forked

\* Outside the lock: the caller of the first post starts the timer.
TimerStart(p, t) ==
  LET f == Top(p, t) IN
  /\ At(p, t, "tstart")
  /\ \/ s' = [s EXCEPT ![p].alive = @ \cup {Timer}, ![p].tpc = "scan",
                       ![p].stk[t] = SetTop(@, [f EXCEPT !.st = AfterSpawns])]
     \/ /\ TimerFailOK
        /\ s' = [s EXCEPT ![p].stk[t] = SetTop(@, [f EXCEPT !.st = "tfail"])]
  /\ UNCHANGED forked

\* Under the lock, after the timer failed to start: every job still waiting
\* for it is woken, one sleeper is woken, or a helper started, and later posts
\* wake their own job.
TimerFailed(p, t) ==
  LET P == s[p]
      f == Top(p, t)
      J == IF Variant = "fail_wakes_own" THEN {f.j} ELSE Waiting(P)
      P1 == [P EXCEPT !.tstate = "failed", !.tarmed = FALSE,
                      !.woken = [x \in Jobs |-> IF x \in J THEN TRUE ELSE P.woken[x]]]
      Go(Q, st) == [Q EXCEPT !.stk[t] = SetTop(@, [f EXCEPT !.st = st])]
  IN /\ At(p, t, "tfail") /\ P.holder = "none"
     /\ \E W \in WakeSet(P1, f.j) :
          s' = [s EXCEPT ![p] = IF StartFor(W)
                               THEN Go([W EXCEPT !.count = @ + 1, !.nres = @ + 1], "cspawn")
                               ELSE Go(W, AfterSpawns)]
     /\ UNCHANGED forked

\* A helper start some thread reserved, outside the lock; a failure gives
\* the reservation back under the lock.
StartHelper(P) == {[P EXCEPT !.alive = @ \cup {h}, !.created = @ \cup {h}, !.nres = @ - 1,
                             !.hpc[h] = "idle", !.timed[h] = FALSE, !.res[h] = {}, !.stk[h] = <<>>]
                   : h \in Helpers \ P.alive}
FailStart(P) == [P EXCEPT !.count = IF Variant = "spawn_fail_keeps" THEN @ ELSE @ - 1, !.nres = @ - 1]

CallerSpawn(p, t) ==
  LET P == s[p]
      f == Top(p, t)
      Go(Q) == [Q EXCEPT !.stk[t] = SetTop(@, [f EXCEPT !.st = AfterSpawns])]
  IN /\ At(p, t, "cspawn")
     /\ \/ \E Q \in StartHelper(P) : s' = [s EXCEPT ![p] = Go(Q)]
        \/ /\ SpawnFailOK /\ P.holder = "none" /\ s' = [s EXCEPT ![p] = Go(FailStart(P))]
     /\ UNCHANGED forked

\* ---- Timer -----------------------------------------------------------------
TimerAcq(p) ==
  /\ s[p].live /\ Timer \in s[p].alive /\ s[p].tpc = "scan" /\ s[p].holder = "none"
  /\ s' = [s EXCEPT ![p].holder = Timer, ![p].tpc = "scan_l"]
  /\ UNCHANGED forked

\* Holding the lock: wake a waiting job that is due (and pass its wake on),
\* or wait armed while jobs wait or, after the last job, while it lingers,
\* or wait unarmed.
TimerWake(p) ==
  LET P == s[p]
      P0 == [P EXCEPT !.holder = "none"]
  IN /\ P.live /\ Timer \in P.alive /\ P.tpc = "scan_l"
     /\ \E j \in Waiting(P) :
          /\ Variant = "boundary_wake" =>
               \E t \in P.alive : Busy(p, t) /\ Top(p, t).j = j /\ Top(p, t).k = "own"
                                 /\ Top(p, t).st = "claim"
          /\ \E W \in WakeSet(P0, j) :
               s' = [s EXCEPT ![p] = IF StartFor(W)
                                    THEN [W EXCEPT !.count = @ + 1, !.nres = @ + 1, !.tpc = "tspawn"]
                                    ELSE [W EXCEPT !.tpc = "scan"]]
  /\ UNCHANGED forked

TimerIdle(p) ==
  LET P == s[p]
      U == Waiting(P)
      P0 == [P EXCEPT !.holder = "none"]
  IN /\ P.live /\ Timer \in P.alive /\ P.tpc = "scan_l"
     /\ \/ /\ U # {} \/ P.queue = <<>>
           /\ s' = [s EXCEPT ![p] = [P0 EXCEPT !.tarmed = TRUE, !.tpc = "wait_t"]]
        \/ /\ U = {}
           /\ s' = [s EXCEPT ![p] = [P0 EXCEPT !.tarmed = FALSE, !.tpc = "wait_u"]]
  /\ UNCHANGED forked

TimerTimeout(p) ==
  /\ s[p].live /\ Timer \in s[p].alive /\ s[p].tpc = "wait_t"
  /\ s' = [s EXCEPT ![p].tpc = "scan"]
  /\ UNCHANGED forked

TimerSpawn(p) ==
  LET P == s[p]
      Go(Q) == [Q EXCEPT !.tpc = "scan"]
  IN /\ P.live /\ Timer \in P.alive /\ P.tpc = "tspawn"
     /\ \/ \E Q \in StartHelper(P) : s' = [s EXCEPT ![p] = Go(Q)]
        \/ /\ SpawnFailOK /\ P.holder = "none" /\ s' = [s EXCEPT ![p] = Go(FailStart(P))]
     /\ UNCHANGED forked

IdleWait(p, t) ==
  LET f == Top(p, t) IN
  /\ At(p, t, "idle_wait")
  /\ s[p].next[f.j] > Spans \/ s[p].stop[f.j]
  /\ s' = [s EXCEPT ![p].stk[t] = SetTop(@, [f EXCEPT !.st = "acq_remove"])]
  /\ UNCHANGED forked

\* Callers and helpers claim spans from the job's cursor, outside the lock.
Claim(p, t) ==
  LET P == s[p]
      f == Top(p, t)
      j == f.j
      none == IF f.k = "own" THEN "acq_remove"
              ELSE IF HookFirst THEN "hook" ELSE "acq_leave"
  IN /\ At(p, t, "claim")
     /\ IF P.next[j] <= Spans /\ ~P.stop[j]
        THEN s' = [s EXCEPT ![p].next[j] = @ + 1,
                            ![p].stk[t] = SetTop(@, [f EXCEPT !.st = "run", !.sp = P.next[j]])]
        ELSE s' = [s EXCEPT ![p].stk[t] = SetTop(@, [f EXCEPT !.st = none])]
     /\ UNCHANGED forked

\* A span returns or fails. A failure goes on to its generation check, then
\* takes the job's lock to record itself.
Complete(p, t, fail) ==
  LET f == Top(p, t)
      j == f.j
      fst == IF Variant = "fail_lock_first" THEN "acq_jm" ELSE "fail_gen"
  IN \E e \in (IF EscapeOK /\ ~fail THEN {FALSE, TRUE} ELSE {FALSE}) :
       /\ s' = [s EXCEPT
                 ![p].ran[j][f.sp] = @ + 1,
                 ![p].res[t] = @ \cup {j},
                 ![p].esc[j] = IF e THEN @ \cup {t} ELSE @,
                 ![p].stk[t] = SetTop(@, [f EXCEPT !.st = IF fail THEN fst ELSE "after",
                                                   !.fl = fail])]
       /\ UNCHANGED forked

\* User code: a span returns, fails, posts a nested job, or forks.
Run(p, t) ==
  LET f == Top(p, t)
      n == Nest(f.j, f.sp)
  IN /\ At(p, t, "run")
     /\ IF n # 0
        THEN /\ s' = [s EXCEPT ![p].stk[t] =
                        Append(SetTop(@, [f EXCEPT !.st = "nest_ret"]),
                               [Frame("own", n, "acq_post") EXCEPT !.sa = s[p].tarmed])]
             /\ UNCHANGED forked
        ELSE /\ Blocking => s[p].next[f.j] > Spans
             /\ \/ Complete(p, t, FALSE)
                \/ FailOK /\ Complete(p, t, TRUE)
                \/ Fork(p, t)

NestRet(p, t) ==
  /\ At(p, t, "nest_ret")
  /\ Complete(p, t, Top(p, t).nf)

\* FORK-17 on the failure path, before the job's lock.
FailGen(p, t) ==
  LET P == s[p]
      f == Top(p, t)
  IN /\ At(p, t, "fail_gen")
     /\ IF GenCheck /\ P.gen # P.jgen[f.j]
        THEN s' = [s EXCEPT ![p].live = FALSE]
        ELSE s' = [s EXCEPT ![p].stk[t] = SetTop(@, [f EXCEPT !.st = "acq_jm"])]
     /\ UNCHANGED forked

AcqJm(p, t) ==
  LET f == Top(p, t) IN
  /\ At(p, t, "acq_jm") /\ s[p].jm[f.j] = "none"
  /\ s' = [s EXCEPT ![p].jm[f.j] = t,
                    ![p].stk[t] = SetTop(@, [f EXCEPT !.st = "rec_fail"])]
  /\ UNCHANGED forked

\* The first failure is recorded with its trace and stops further claims.
RecFail(p, t) ==
  LET P == s[p]
      f == Top(p, t)
      j == f.j
  IN /\ At(p, t, "rec_fail")
     /\ s' = [s EXCEPT ![p].jm[j] = "none", ![p].stop[j] = TRUE, ![p].first[j] = TRUE,
                       ![p].ftrace[j] = IF ~P.first[j] THEN t ELSE @,
                       ![p].stk[t] = SetTop(@, [f EXCEPT !.st = "after"])]
     /\ UNCHANGED forked

\* FORK-17: a span that returns in another generation ends the process. A
\* helper keeps claiming its job without the lock while it is the only job
\* posted.
After(p, t) ==
  LET P == s[p]
      f == Top(p, t)
      nst == IF f.k = "own" THEN (IF f.fl THEN "acq_remove" ELSE "claim")
             ELSE IF ~f.fl /\ Len(P.queue) = 1 THEN "claim"
             ELSE IF HookFirst THEN "hook" ELSE "acq_leave"
  IN /\ At(p, t, "after")
     /\ IF GenCheck /\ P.gen # P.jgen[f.j]
        THEN s' = [s EXCEPT ![p].live = FALSE]
        ELSE s' = [s EXCEPT ![p].stk[t] = SetTop(@, [f EXCEPT !.st = nst])]
     /\ UNCHANGED forked

AcqRemove(p, t) ==
  /\ At(p, t, "acq_remove") /\ s[p].holder = "none"
  /\ s' = [s EXCEPT ![p].holder = t,
                    ![p].stk[t] = SetTop(@, [Top(p, t) EXCEPT !.st = "remove"])]
  /\ UNCHANGED forked

\* The caller, holding the lock with no helper on its job, reports the job's
\* outcome once, keeps what was handed to it, frees the job and unlocks.
FinishEff(p, t) ==
  LET P == s[p]
      f == Top(p, t)
      j == f.j
      q == P.stk[t]
      nested == Len(q) > 1
      below == q[Len(q) - 1]
      mine == j \in P.res[t] \/ P.handed[j]
      nres == (P.res[t] \ {j}) \cup (IF nested /\ mine THEN {below.j} ELSE {})
      nq == IF nested THEN SetTop(Pop(q), [below EXCEPT !.nf = P.first[j]]) ELSE Pop(q)
      top == ~nested /\ t \in Callers
  IN [P EXCEPT
        !.holder = "none",
        !.queue = Remove(@, j),
        !.fin[j] = @ + 1,
        !.jst[j] = IF P.first[j] THEN "failed" ELSE "done",
        !.lost = @ \/ (P.first[j] /\ P.ftrace[j] = "none"),
        !.res[t] = nres,
        !.stk[t] = nq,
        !.cpc = IF top THEN [P.cpc EXCEPT ![t] = "end"] ELSE P.cpc,
        \* A child leaves the user code it was forked in: FORK-17.
        !.live = IF top /\ p = "C" THEN FALSE ELSE @]

\* Remove the job; with no helper on it the caller is done at once, which
\* is the case for every call whose spans the caller ran before any helper
\* picked the job. Otherwise it sleeps on `done`.
RemoveJob(p, t) ==
  LET j == Top(p, t).j IN
  /\ At(p, t, "remove")
  /\ IF s[p].hon[j] = {}
     THEN s' = [s EXCEPT ![p] = FinishEff(p, t)]
     ELSE s' = [s EXCEPT ![p].holder = "none", ![p].queue = Remove(@, j),
                         ![p].jst[j] = "removed",
                         ![p].stk[t] = SetTop(@, [Top(p, t) EXCEPT !.st = "wait"])]
  /\ UNCHANGED forked

\* Woken on `done`: retake the lock and look again.
Recheck(p, t) ==
  LET j == Top(p, t).j IN
  /\ At(p, t, "recheck") /\ s[p].holder = "none"
  /\ IF s[p].hon[j] = {}
     THEN s' = [s EXCEPT ![p] = FinishEff(p, t)]
     ELSE s' = [s EXCEPT ![p].stk[t] = SetTop(@, [Top(p, t) EXCEPT !.st = "wait"])]
  /\ UNCHANGED forked

\* ---- Helpers ---------------------------------------------------------------
Dist(p, x) == (x + NJ - s[p].rr) % NJ
Choose(p, E) ==
  IF Variant = "fifo"
  THEN s[p].queue[CHOOSE i \in 1..Len(s[p].queue) :
                    /\ s[p].queue[i] \in E
                    /\ \A m \in 1..(i - 1) : s[p].queue[m] \notin E]
  ELSE CHOOSE k \in E : \A x \in E : Dist(p, k) <= Dist(p, x)

AcqPick(p, h) ==
  /\ h \in Helpers /\ s[p].live /\ h \in s[p].alive
  /\ s[p].hpc[h] = "idle" /\ s[p].stk[h] = <<>> /\ s[p].holder = "none"
  /\ s' = [s EXCEPT ![p].holder = h, ![p].hpc[h] = "acq"]
  /\ UNCHANGED forked

\* Under the lock: with no eligible job, retire (past the kept count, or
\* after the idle timeout) or sleep on `work`. Otherwise join the job next
\* after the last one picked.
Pick(p, h) ==
  LET P == s[p]
      E == Elig(p)
      k == Choose(p, E)
      retire == RetireOK /\ (P.timed[h] \/ P.count > Soft)
      P2 == [P EXCEPT
               !.holder = "none", !.hpc[h] = "busy", !.timed[h] = FALSE,
               !.hon[k] = @ \cup {h},
               !.bypass = IF ~TrackBypass THEN P.bypass
                          ELSE [x \in Jobs |-> IF x = k THEN 0
                                               ELSE IF x \in E THEN P.bypass[x] + 1
                                               ELSE 0],
               !.rr = (k % NJ) + 1,
               !.stk[h] = <<Frame("help", k, "claim")>>]
      pass == Variant # "no_cascade" /\ Open(P2) # {}
  IN /\ h \in Helpers /\ P.live /\ h \in P.alive /\ P.hpc[h] = "acq"
     /\ IF E = {}
        THEN IF retire
             THEN s' = [s EXCEPT ![p] = [P EXCEPT
                          !.holder = "none", !.hpc[h] = "none", !.timed[h] = FALSE,
                          !.alive = @ \ {h}, !.created = @ \ {h},
                          !.count = IF Variant = "retire_no_dec" THEN @ ELSE @ - 1]]
             ELSE s' = [s EXCEPT ![p].holder = "none", ![p].hpc[h] = "sleep",
                                 ![p].timed[h] = FALSE]
        ELSE IF pass /\ Sleepers(P2) # {}
             THEN \E w \in Sleepers(P2) : s' = [s EXCEPT ![p] = [P2 EXCEPT !.hpc[w] = "idle"]]
             ELSE IF pass /\ StartFor(P2)
             THEN s' = [s EXCEPT ![p] = [P2 EXCEPT !.count = @ + 1, !.nres = @ + 1,
                                                  !.stk[h] = <<Frame("help", k, "hspawn")>>]]
             ELSE s' = [s EXCEPT ![p] = P2]
     /\ UNCHANGED forked

\* Outside the lock, before its span: the helper starts the next helper.
HelperSpawn(p, h) ==
  LET P == s[p]
      f == Top(p, h)
      Go(Q) == [Q EXCEPT !.stk[h] = SetTop(@, [f EXCEPT !.st = "claim"])]
  IN /\ At(p, h, "hspawn")
     /\ \/ \E Q \in StartHelper(P) : s' = [s EXCEPT ![p] = Go(Q)]
        \/ /\ SpawnFailOK /\ P.holder = "none" /\ s' = [s EXCEPT ![p] = Go(FailStart(P))]
     /\ UNCHANGED forked

\* A sleeping helper's idle timeout fires.
Timeout(p, h) ==
  /\ RetireOK /\ h \in Helpers /\ s[p].live /\ h \in s[p].alive
  /\ s[p].hpc[h] = "sleep"
  /\ s' = [s EXCEPT ![p].hpc[h] = "idle", ![p].timed[h] = TRUE]
  /\ UNCHANGED forked

\* The job-end hook: hand the helper's tracker entries to the job, keep the
\* trace of the job's recorded failure for the caller, drop the rest. It
\* takes the job's lock only when the helper left something.
HookEffect(p, h, j) ==
  LET P == s[p] IN
  [P EXCEPT
     !.res[h] = {},
     !.handed[j] = @ \/ (Variant # "flush" /\ j \in P.res[h]),
     !.uaf = @ \/ (Variant = "flush" /\ h \in P.esc[j]),
     !.ftrace = [x \in Jobs |->
                   IF P.ftrace[x] = h
                   THEN (IF x = j /\ Variant # "hook_drops_trace" THEN "kept" ELSE "none")
                   ELSE P.ftrace[x]],
     !.late = @ \/ P.jst[j] \in {"done", "failed"}]

HookLockFree(p, h, j) == j \notin s[p].res[h] \/ s[p].jm[j] = "none"

Hook(p, h) ==
  LET f == Top(p, h) IN
  /\ At(p, h, "hook") /\ f.k = "help" /\ HookLockFree(p, h, f.j)
  /\ s' = [s EXCEPT ![p] = [HookEffect(p, h, f.j) EXCEPT
                              !.stk[h] = SetTop(@, [f EXCEPT !.st = "acq_leave"])]]
  /\ UNCHANGED forked

AcqLeave(p, h) ==
  /\ At(p, h, "acq_leave") /\ s[p].holder = "none"
  /\ s' = [s EXCEPT ![p].holder = h,
                    ![p].stk[h] = SetTop(@, [Top(p, h) EXCEPT !.st = "leave"])]
  /\ UNCHANGED forked

\* Leaving under the lock; the last helper to leave a job wakes the callers
\* sleeping on `done`. The helper is then awake and picks again.
Leave(p, h) ==
  LET P == s[p]
      f == Top(p, h)
      j == f.j
      last == P.hon[j] = {h} /\ Variant # "no_done_notify"
      woken == [c \in Threads |->
                  IF last /\ c \in P.alive /\ P.stk[c] # <<>>
                     /\ P.stk[c][Len(P.stk[c])].st = "wait"
                  THEN SetTop(P.stk[c], [P.stk[c][Len(P.stk[c])] EXCEPT !.st = "recheck"])
                  ELSE P.stk[c]]
  IN /\ At(p, h, "leave")
     /\ IF Variant = "hook_after_leave"
        THEN s' = [s EXCEPT ![p] = [P EXCEPT !.holder = "none", !.hon[j] = @ \ {h},
                     !.stk = [woken EXCEPT ![h] = SetTop(@, [f EXCEPT !.st = "hook2"])]]]
        ELSE s' = [s EXCEPT ![p] = [P EXCEPT !.holder = "none", !.hon[j] = @ \ {h},
                     !.stk = [woken EXCEPT ![h] = <<>>], !.hpc[h] = "idle"]]
     /\ UNCHANGED forked

Hook2(p, h) ==
  LET f == Top(p, h) IN
  /\ At(p, h, "hook2")
  /\ s' = [s EXCEPT ![p] = [HookEffect(p, h, f.j) EXCEPT
                              !.stk[h] = <<>>, !.hpc[h] = "idle"]]
  /\ UNCHANGED forked

\* ---- Exit ------------------------------------------------------------------
Exit(p, c) ==
  /\ ExitOK /\ c \in Callers /\ p = "P" /\ s[p].live /\ c \in s[p].alive
  /\ s[p].cpc[c] = "end" /\ ~s[p].exiting
  /\ IF Variant = "exit_joins"
     THEN s' = [s EXCEPT ![p].exiting = TRUE]
     ELSE s' = [s EXCEPT ![p].live = FALSE]
  /\ UNCHANGED forked

ExitJoined(p) ==
  /\ s[p].live /\ s[p].exiting /\ s[p].created \cap s[p].alive = {}
  /\ s' = [s EXCEPT ![p].live = FALSE]
  /\ UNCHANGED forked

Done(p) == \/ ~s[p].live
           \/ p = "P" /\ ~s[p].exiting /\ \A c \in Callers : s[p].cpc[c] = "end"

Terminated == (\A p \in Procs : Done(p)) /\ UNCHANGED vars

Next ==
  \/ \E p \in Procs, t \in Threads :
       \/ Pre(p, t) \/ AcqPost(p, t) \/ Post(p, t) \/ IdleWait(p, t)
       \/ TimerStart(p, t) \/ TimerFailed(p, t) \/ CallerSpawn(p, t) \/ HelperSpawn(p, t)
       \/ Claim(p, t) \/ Run(p, t) \/ NestRet(p, t) \/ FailGen(p, t)
       \/ AcqJm(p, t) \/ RecFail(p, t) \/ After(p, t)
       \/ AcqRemove(p, t) \/ RemoveJob(p, t) \/ Recheck(p, t)
       \/ AcqPick(p, t) \/ Pick(p, t) \/ Timeout(p, t) \/ Hook(p, t) \/ AcqLeave(p, t)
       \/ Leave(p, t) \/ Hook2(p, t) \/ Exit(p, t)
  \/ \E p \in Procs : ExitJoined(p) \/ TimerAcq(p) \/ TimerWake(p) \/ TimerIdle(p) \/ TimerTimeout(p) \/ TimerSpawn(p)
  \/ Terminated

Spec == Init /\ [][Next]_vars

\* For the liveness check: the timer keeps going, and wakes a job it can.
FairSpec == Spec /\ WF_vars(Next) /\ \A p \in Procs : /\ WF_vars(TimerAcq(p)) /\ WF_vars(TimerTimeout(p))
                                      /\ WF_vars(TimerSpawn(p)) /\ SF_vars(TimerWake(p))

\* ---- Properties ------------------------------------------------------------
Live(p) == s[p].live

\* Every job completes or fails exactly once, and a completed job ran every
\* span exactly once.
ExactlyOnce ==
  \A p \in Procs, j \in Jobs :
    /\ s[p].fin[j] <= 1
    /\ \A k \in 1..Spans : s[p].ran[j][k] <= 1
    /\ s[p].jst[j] = "done" => \A k \in 1..Spans : s[p].ran[j][k] = 1

\* No thread-local state crosses jobs: a helper holds residue only of jobs
\* on its own stack, so an idle helper holds none.
NoCarry ==
  \A p \in Procs : Live(p) =>
    \A h \in Helpers \cap s[p].alive : s[p].res[h] \subseteq JobsOn(p, h)

\* No helper touches a job its caller has freed.
NoTouchAfterFree == \A p \in Procs : ~s[p].late

\* A failed job's report carries the trace of its failure.
TraceReported == \A p \in Procs : ~s[p].lost

\* No tracker entry is released while a value returned to the caller uses it.
NoUseAfterRelease == \A p \in Procs : ~s[p].uaf

\* After fork, no thread waits on a helper or a lock holder that does not
\* exist in its process.
WaitsOnlyOnLiveThreads ==
  \A p \in Procs : Live(p) =>
    \A t \in s[p].alive :
      /\ (At(p, t, "wait") \/ At(p, t, "recheck")) => s[p].hon[Top(p, t).j] \subseteq s[p].alive
      /\ (At(p, t, "acq_remove") \/ At(p, t, "acq_leave") \/ At(p, t, "recheck") \/ At(p, t, "tfail")
          \/ (At(p, t, "acq_post") /\ ~(GenCheck /\ s[p].inst # s[p].gen)))
         => s[p].holder \in s[p].alive \cup {"none"}
      /\ At(p, t, "acq_jm") => s[p].jm[Top(p, t).j] \in s[p].alive \cup {"none"}
      /\ At(p, t, "hook") /\ Top(p, t).j \in s[p].res[t]
         => s[p].jm[Top(p, t).j] \in s[p].alive \cup {"none"}

\* The pool never has more helpers than the ceiling.
CapRespected == \A p \in Procs : s[p].count <= Cap /\ Cardinality(s[p].created) <= Cap

\* The pool's helper count is its live helpers plus the starts it reserved.
CountMatches == \A p \in Procs : Live(p) /\ s[p].inst = s[p].gen =>
                  s[p].count = Cardinality(s[p].created) + s[p].nres

\* No wakeup is lost: while helpers sleep and a woken job has room, a helper
\* is awake to take it, or some thread holds the wake it will pass on.
NoSleepWithWork ==
  \A p \in Procs : Live(p) =>
    LET P == s[p] IN
    (Open(P) # {} /\ Sleepers(P) # {}) =>
      \/ Awake(P) # {}
      \/ \E h \in LiveHelpers(P) : At(p, h, "hspawn")
      \/ Timer \in P.alive /\ P.tpc \in {"scan", "scan_l", "tspawn"}
      \/ \E c \in Callers : At(p, c, "cspawn")

\* No helper start is lost: while a woken job has room below its helpers and
\* no start can fail, some thread is awake or holds the wake it will pass on.
NoStartLost ==
  \A p \in Procs : Live(p) /\ ~SpawnFailOK =>
    LET P == s[p] IN
    (\E j \in Open(P) : P.count < Grow(j)) =>
      \/ Awake(P) # {}
      \/ \E h \in LiveHelpers(P) : At(p, h, "hspawn")
      \/ Timer \in P.alive /\ P.tpc \in {"scan", "scan_l", "tspawn"}
      \/ \E c \in Callers : At(p, c, "cspawn")

\* Every job waiting to be woken is one the timer will look at: it is armed,
\* awake, or still being started, or its start has failed and the failure
\* is being handled.
JobsAreWatched ==
  \A p \in Procs : Live(p) /\ s[p].inst = s[p].gen =>
    \A j \in Waiting(s[p]) :
      \/ Timer \in s[p].alive /\ s[p].tpc # "wait_u"
      \/ \E c \in Callers : At(p, c, "tstart") \/ At(p, c, "tfail")

\* No helper takes a job the timer has not woken.
NoEarlyClaim == \A p \in Procs, j \in Jobs : s[p].hon[j] # {} => s[p].woken[j]

\* Round-robin: over consecutive picks a job stays eligible for, it is passed
\* over fewer times than there are jobs.
BoundedBypass == \A p \in Procs, j \in Jobs : s[p].bypass[j] < NJ

\* A job waiting to be woken is woken, or runs out of spans.
EventuallyWoken == \A j \in Jobs : (j \in Waiting(s["P"])) ~> (j \notin Waiting(s["P"]) \/ ~s["P"].live)

TypeOK == forked \in BOOLEAN
=============================================================================
