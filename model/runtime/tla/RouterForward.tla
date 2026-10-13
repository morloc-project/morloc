----------------------------- MODULE RouterForward ----------------------------
(* Requests forwarded by the router to one program's daemon, several at    *)
(* once (model/runtime/daemon.md DAEMON-12, DAEMON-16). A forwarder holds  *)
(* the program's lock only to manage the daemon: reap one that has exited *)
(* and start one when there is none. It connects and exchanges the request *)
(* outside the lock, so requests run concurrently. A connection reaches    *)
(* whichever daemon is listening on the program's socket path. It may fail *)
(* on the forwarder's side (no descriptor), or be refused by a listening   *)
(* daemon whose backlog is full. A failed connection is retried once      *)
(* under the lock: the forwarder reaps an exited daemon, and stops and     *)
(* replaces the program's daemon only when its socket is gone. A daemon    *)
(* may exit, stop listening, or lose its socket at any time; an exchange   *)
(* ends when the daemon replies or ends. Once a daemon is reaped the       *)
(* kernel may hand its pid to a stranger.                                  *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "unlocked_check": the check for an exited daemon reaps outside the lock.
\* "stop_on_refused": a refused connection stops the daemon it tried, when
\*   that daemon is still the program's.
\* "never_replace": a daemon whose socket is gone is never replaced.

Fwds == 1..2
Gens == 1..3
Requests == 2

(* --algorithm RouterForward
variables
    d = [x \in Gens |-> "none"],
    slot = 0,
    next = 1,
    lock = 0,
    reissued = [x \in Gens |-> FALSE],
    hitStranger = FALSE,
    stoppedListening = FALSE,
    keptGone = FALSE;

define
    Alive(x) == d[x] \in {"starting", "listening", "deaf", "unreachable"}
    OneDaemon == \A x, y \in Gens : Alive(x) /\ Alive(y) => x = y
    NoUntrackedDaemon == \A x \in Gens : Alive(x) => slot = x
    NoSignalReachesAStranger == ~hitStranger
    NoListeningDaemonStopped == ~stoppedListening
    NoGoneDaemonKept == ~keptGone
    ForwardsEnd == <>(\A f \in Fwds : pc[f] = "Done")
end define;

macro stop(k) begin
  if d[k] = "reaped" then
    if reissued[k] then hitStranger := TRUE; end if;
  elsif Alive(k) then
    if d[k] = "listening" then stoppedListening := TRUE; end if;
    d[k] := "zombie";
  end if;
end macro;

macro waitpid(k) begin
  if d[k] = "reaped" then
    if reissued[k] then hitStranger := TRUE; end if;
  else
    if slot = k then slot := 0; end if;
    d[k] := "reaped";
  end if;
end macro;

fair process Fwd \in Fwds
variables g = 0, tried = 0, seen = 0, conn = 0, retried = FALSE, reqs = 0;
begin
  Req:
    if reqs = Requests then
      goto Done;
    else
      reqs := reqs + 1;
      retried := FALSE;
      conn := 0;
      if Variant = "unlocked_check" then goto Check; end if;
    end if;
  Lock1:
    await lock = 0;
    lock := self;
  Check:
    if slot > 0 /\ d[slot] = "zombie" then seen := slot; else seen := 0; end if;
  Reap1:
    if seen > 0 then waitpid(seen); end if;
    if Variant # "unlocked_check" then goto StartIf; end if;
  Lock1b:
    await lock = 0;
    lock := self;
  StartIf:
    if slot # 0 then
      goto Got;
    elsif next > 3 then
      goto FailUnlock;
    else
      g := next;
      next := next + 1;
      d[g] := "starting";
      slot := g;
    end if;
  WaitReady:
    await d[g] # "starting";
    if d[g] = "listening" then
      goto Got;
    elsif d[g] \in {"deaf", "unreachable"} then
      stop(g);
    end if;
  ReapStart:
    await d[g] = "zombie";
    waitpid(g);
    goto FailUnlock;
  Got:
    tried := slot;
    lock := 0;
  Connect:
    if \E k \in Gens : d[k] = "listening" then
      either
        conn := CHOOSE k \in Gens : d[k] = "listening";
        goto Exchange;
      or
        goto Req;
      or
        skip;
      end either;
    end if;
  Refused:
    if retried then goto Req; else retried := TRUE; end if;
  Lock2:
    await lock = 0;
    lock := self;
  Stop:
    if Variant # "stop_on_refused" then
      if Variant = "design" /\ slot > 0 /\ d[slot] = "unreachable" then
        tried := slot;
        stop(slot);
        goto ReapStop;
      else
        if slot > 0 /\ d[slot] = "unreachable" then keptGone := TRUE; end if;
        goto Check;
      end if;
    elsif tried = 0 \/ slot # tried then
      goto StartIf;
    else
      stop(tried);
    end if;
  ReapStop:
    await d[tried] \in {"zombie", "reaped"};
    if d[tried] = "zombie" then waitpid(tried); end if;
    goto StartIf;
  FailUnlock:
    lock := 0;
    goto Req;
  Exchange:
    either
      await d[conn] \in {"listening", "deaf", "unreachable"};
    or
      await d[conn] \in {"zombie", "reaped"};
    end either;
    goto Req;
end process;

fair process Daemon \in {10 + x : x \in Gens}
begin
  Life:
    await d[self - 10] # "none";
    if d[self - 10] = "starting" then
      either d[self - 10] := "listening";
      or d[self - 10] := "deaf";
      or d[self - 10] := "zombie";
      end either;
    end if;
  Later:
    if d[self - 10] = "listening" then
      either d[self - 10] := "zombie";
      or d[self - 10] := "deaf";
      or d[self - 10] := "unreachable";
      or skip;
      end either;
    end if;
end process;

fair process Kernel = 101
begin
  Reissue:
    with h \in {x \in Gens : d[x] = "reaped" /\ ~reissued[x]} do
      reissued[h] := TRUE;
    end with;
    goto Reissue;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "d1b5c089" /\ chksum(tla) = "81417b75")
VARIABLES d, slot, next, lock, reissued, hitStranger, stoppedListening, 
          keptGone, pc

(* define statement *)
Alive(x) == d[x] \in {"starting", "listening", "deaf", "unreachable"}
OneDaemon == \A x, y \in Gens : Alive(x) /\ Alive(y) => x = y
NoUntrackedDaemon == \A x \in Gens : Alive(x) => slot = x
NoSignalReachesAStranger == ~hitStranger
NoListeningDaemonStopped == ~stoppedListening
NoGoneDaemonKept == ~keptGone
ForwardsEnd == <>(\A f \in Fwds : pc[f] = "Done")

VARIABLES g, tried, seen, conn, retried, reqs

vars == << d, slot, next, lock, reissued, hitStranger, stoppedListening, 
           keptGone, pc, g, tried, seen, conn, retried, reqs >>

ProcSet == (Fwds) \cup ({10 + x : x \in Gens}) \cup {101}

Init == (* Global variables *)
        /\ d = [x \in Gens |-> "none"]
        /\ slot = 0
        /\ next = 1
        /\ lock = 0
        /\ reissued = [x \in Gens |-> FALSE]
        /\ hitStranger = FALSE
        /\ stoppedListening = FALSE
        /\ keptGone = FALSE
        (* Process Fwd *)
        /\ g = [self \in Fwds |-> 0]
        /\ tried = [self \in Fwds |-> 0]
        /\ seen = [self \in Fwds |-> 0]
        /\ conn = [self \in Fwds |-> 0]
        /\ retried = [self \in Fwds |-> FALSE]
        /\ reqs = [self \in Fwds |-> 0]
        /\ pc = [self \in ProcSet |-> CASE self \in Fwds -> "Req"
                                        [] self \in {10 + x : x \in Gens} -> "Life"
                                        [] self = 101 -> "Reissue"]

Req(self) == /\ pc[self] = "Req"
             /\ IF reqs[self] = Requests
                   THEN /\ pc' = [pc EXCEPT ![self] = "Done"]
                        /\ UNCHANGED << conn, retried, reqs >>
                   ELSE /\ reqs' = [reqs EXCEPT ![self] = reqs[self] + 1]
                        /\ retried' = [retried EXCEPT ![self] = FALSE]
                        /\ conn' = [conn EXCEPT ![self] = 0]
                        /\ IF Variant = "unlocked_check"
                              THEN /\ pc' = [pc EXCEPT ![self] = "Check"]
                              ELSE /\ pc' = [pc EXCEPT ![self] = "Lock1"]
             /\ UNCHANGED << d, slot, next, lock, reissued, hitStranger, 
                             stoppedListening, keptGone, g, tried, seen >>

Lock1(self) == /\ pc[self] = "Lock1"
               /\ lock = 0
               /\ lock' = self
               /\ pc' = [pc EXCEPT ![self] = "Check"]
               /\ UNCHANGED << d, slot, next, reissued, hitStranger, 
                               stoppedListening, keptGone, g, tried, seen, 
                               conn, retried, reqs >>

Check(self) == /\ pc[self] = "Check"
               /\ IF slot > 0 /\ d[slot] = "zombie"
                     THEN /\ seen' = [seen EXCEPT ![self] = slot]
                     ELSE /\ seen' = [seen EXCEPT ![self] = 0]
               /\ pc' = [pc EXCEPT ![self] = "Reap1"]
               /\ UNCHANGED << d, slot, next, lock, reissued, hitStranger, 
                               stoppedListening, keptGone, g, tried, conn, 
                               retried, reqs >>

Reap1(self) == /\ pc[self] = "Reap1"
               /\ IF seen[self] > 0
                     THEN /\ IF d[seen[self]] = "reaped"
                                THEN /\ IF reissued[seen[self]]
                                           THEN /\ hitStranger' = TRUE
                                           ELSE /\ TRUE
                                                /\ UNCHANGED hitStranger
                                     /\ UNCHANGED << d, slot >>
                                ELSE /\ IF slot = seen[self]
                                           THEN /\ slot' = 0
                                           ELSE /\ TRUE
                                                /\ slot' = slot
                                     /\ d' = [d EXCEPT ![seen[self]] = "reaped"]
                                     /\ UNCHANGED hitStranger
                     ELSE /\ TRUE
                          /\ UNCHANGED << d, slot, hitStranger >>
               /\ IF Variant # "unlocked_check"
                     THEN /\ pc' = [pc EXCEPT ![self] = "StartIf"]
                     ELSE /\ pc' = [pc EXCEPT ![self] = "Lock1b"]
               /\ UNCHANGED << next, lock, reissued, stoppedListening, 
                               keptGone, g, tried, seen, conn, retried, reqs >>

Lock1b(self) == /\ pc[self] = "Lock1b"
                /\ lock = 0
                /\ lock' = self
                /\ pc' = [pc EXCEPT ![self] = "StartIf"]
                /\ UNCHANGED << d, slot, next, reissued, hitStranger, 
                                stoppedListening, keptGone, g, tried, seen, 
                                conn, retried, reqs >>

StartIf(self) == /\ pc[self] = "StartIf"
                 /\ IF slot # 0
                       THEN /\ pc' = [pc EXCEPT ![self] = "Got"]
                            /\ UNCHANGED << d, slot, next, g >>
                       ELSE /\ IF next > 3
                                  THEN /\ pc' = [pc EXCEPT ![self] = "FailUnlock"]
                                       /\ UNCHANGED << d, slot, next, g >>
                                  ELSE /\ g' = [g EXCEPT ![self] = next]
                                       /\ next' = next + 1
                                       /\ d' = [d EXCEPT ![g'[self]] = "starting"]
                                       /\ slot' = g'[self]
                                       /\ pc' = [pc EXCEPT ![self] = "WaitReady"]
                 /\ UNCHANGED << lock, reissued, hitStranger, stoppedListening, 
                                 keptGone, tried, seen, conn, retried, reqs >>

WaitReady(self) == /\ pc[self] = "WaitReady"
                   /\ d[g[self]] # "starting"
                   /\ IF d[g[self]] = "listening"
                         THEN /\ pc' = [pc EXCEPT ![self] = "Got"]
                              /\ UNCHANGED << d, hitStranger, stoppedListening >>
                         ELSE /\ IF d[g[self]] \in {"deaf", "unreachable"}
                                    THEN /\ IF d[g[self]] = "reaped"
                                               THEN /\ IF reissued[g[self]]
                                                          THEN /\ hitStranger' = TRUE
                                                          ELSE /\ TRUE
                                                               /\ UNCHANGED hitStranger
                                                    /\ UNCHANGED << d, 
                                                                    stoppedListening >>
                                               ELSE /\ IF Alive(g[self])
                                                          THEN /\ IF d[g[self]] = "listening"
                                                                     THEN /\ stoppedListening' = TRUE
                                                                     ELSE /\ TRUE
                                                                          /\ UNCHANGED stoppedListening
                                                               /\ d' = [d EXCEPT ![g[self]] = "zombie"]
                                                          ELSE /\ TRUE
                                                               /\ UNCHANGED << d, 
                                                                               stoppedListening >>
                                                    /\ UNCHANGED hitStranger
                                    ELSE /\ TRUE
                                         /\ UNCHANGED << d, hitStranger, 
                                                         stoppedListening >>
                              /\ pc' = [pc EXCEPT ![self] = "ReapStart"]
                   /\ UNCHANGED << slot, next, lock, reissued, keptGone, g, 
                                   tried, seen, conn, retried, reqs >>

ReapStart(self) == /\ pc[self] = "ReapStart"
                   /\ d[g[self]] = "zombie"
                   /\ IF d[g[self]] = "reaped"
                         THEN /\ IF reissued[g[self]]
                                    THEN /\ hitStranger' = TRUE
                                    ELSE /\ TRUE
                                         /\ UNCHANGED hitStranger
                              /\ UNCHANGED << d, slot >>
                         ELSE /\ IF slot = g[self]
                                    THEN /\ slot' = 0
                                    ELSE /\ TRUE
                                         /\ slot' = slot
                              /\ d' = [d EXCEPT ![g[self]] = "reaped"]
                              /\ UNCHANGED hitStranger
                   /\ pc' = [pc EXCEPT ![self] = "FailUnlock"]
                   /\ UNCHANGED << next, lock, reissued, stoppedListening, 
                                   keptGone, g, tried, seen, conn, retried, 
                                   reqs >>

Got(self) == /\ pc[self] = "Got"
             /\ tried' = [tried EXCEPT ![self] = slot]
             /\ lock' = 0
             /\ pc' = [pc EXCEPT ![self] = "Connect"]
             /\ UNCHANGED << d, slot, next, reissued, hitStranger, 
                             stoppedListening, keptGone, g, seen, conn, 
                             retried, reqs >>

Connect(self) == /\ pc[self] = "Connect"
                 /\ IF \E k \in Gens : d[k] = "listening"
                       THEN /\ \/ /\ conn' = [conn EXCEPT ![self] = CHOOSE k \in Gens : d[k] = "listening"]
                                  /\ pc' = [pc EXCEPT ![self] = "Exchange"]
                               \/ /\ pc' = [pc EXCEPT ![self] = "Req"]
                                  /\ conn' = conn
                               \/ /\ TRUE
                                  /\ pc' = [pc EXCEPT ![self] = "Refused"]
                                  /\ conn' = conn
                       ELSE /\ pc' = [pc EXCEPT ![self] = "Refused"]
                            /\ conn' = conn
                 /\ UNCHANGED << d, slot, next, lock, reissued, hitStranger, 
                                 stoppedListening, keptGone, g, tried, seen, 
                                 retried, reqs >>

Refused(self) == /\ pc[self] = "Refused"
                 /\ IF retried[self]
                       THEN /\ pc' = [pc EXCEPT ![self] = "Req"]
                            /\ UNCHANGED retried
                       ELSE /\ retried' = [retried EXCEPT ![self] = TRUE]
                            /\ pc' = [pc EXCEPT ![self] = "Lock2"]
                 /\ UNCHANGED << d, slot, next, lock, reissued, hitStranger, 
                                 stoppedListening, keptGone, g, tried, seen, 
                                 conn, reqs >>

Lock2(self) == /\ pc[self] = "Lock2"
               /\ lock = 0
               /\ lock' = self
               /\ pc' = [pc EXCEPT ![self] = "Stop"]
               /\ UNCHANGED << d, slot, next, reissued, hitStranger, 
                               stoppedListening, keptGone, g, tried, seen, 
                               conn, retried, reqs >>

Stop(self) == /\ pc[self] = "Stop"
              /\ IF Variant # "stop_on_refused"
                    THEN /\ IF Variant = "design" /\ slot > 0 /\ d[slot] = "unreachable"
                               THEN /\ tried' = [tried EXCEPT ![self] = slot]
                                    /\ IF d[slot] = "reaped"
                                          THEN /\ IF reissued[slot]
                                                     THEN /\ hitStranger' = TRUE
                                                     ELSE /\ TRUE
                                                          /\ UNCHANGED hitStranger
                                               /\ UNCHANGED << d, 
                                                               stoppedListening >>
                                          ELSE /\ IF Alive(slot)
                                                     THEN /\ IF d[slot] = "listening"
                                                                THEN /\ stoppedListening' = TRUE
                                                                ELSE /\ TRUE
                                                                     /\ UNCHANGED stoppedListening
                                                          /\ d' = [d EXCEPT ![slot] = "zombie"]
                                                     ELSE /\ TRUE
                                                          /\ UNCHANGED << d, 
                                                                          stoppedListening >>
                                               /\ UNCHANGED hitStranger
                                    /\ pc' = [pc EXCEPT ![self] = "ReapStop"]
                                    /\ UNCHANGED keptGone
                               ELSE /\ IF slot > 0 /\ d[slot] = "unreachable"
                                          THEN /\ keptGone' = TRUE
                                          ELSE /\ TRUE
                                               /\ UNCHANGED keptGone
                                    /\ pc' = [pc EXCEPT ![self] = "Check"]
                                    /\ UNCHANGED << d, hitStranger, 
                                                    stoppedListening, tried >>
                    ELSE /\ IF tried[self] = 0 \/ slot # tried[self]
                               THEN /\ pc' = [pc EXCEPT ![self] = "StartIf"]
                                    /\ UNCHANGED << d, hitStranger, 
                                                    stoppedListening >>
                               ELSE /\ IF d[tried[self]] = "reaped"
                                          THEN /\ IF reissued[tried[self]]
                                                     THEN /\ hitStranger' = TRUE
                                                     ELSE /\ TRUE
                                                          /\ UNCHANGED hitStranger
                                               /\ UNCHANGED << d, 
                                                               stoppedListening >>
                                          ELSE /\ IF Alive(tried[self])
                                                     THEN /\ IF d[tried[self]] = "listening"
                                                                THEN /\ stoppedListening' = TRUE
                                                                ELSE /\ TRUE
                                                                     /\ UNCHANGED stoppedListening
                                                          /\ d' = [d EXCEPT ![tried[self]] = "zombie"]
                                                     ELSE /\ TRUE
                                                          /\ UNCHANGED << d, 
                                                                          stoppedListening >>
                                               /\ UNCHANGED hitStranger
                                    /\ pc' = [pc EXCEPT ![self] = "ReapStop"]
                         /\ UNCHANGED << keptGone, tried >>
              /\ UNCHANGED << slot, next, lock, reissued, g, seen, conn, 
                              retried, reqs >>

ReapStop(self) == /\ pc[self] = "ReapStop"
                  /\ d[tried[self]] \in {"zombie", "reaped"}
                  /\ IF d[tried[self]] = "zombie"
                        THEN /\ IF d[tried[self]] = "reaped"
                                   THEN /\ IF reissued[tried[self]]
                                              THEN /\ hitStranger' = TRUE
                                              ELSE /\ TRUE
                                                   /\ UNCHANGED hitStranger
                                        /\ UNCHANGED << d, slot >>
                                   ELSE /\ IF slot = tried[self]
                                              THEN /\ slot' = 0
                                              ELSE /\ TRUE
                                                   /\ slot' = slot
                                        /\ d' = [d EXCEPT ![tried[self]] = "reaped"]
                                        /\ UNCHANGED hitStranger
                        ELSE /\ TRUE
                             /\ UNCHANGED << d, slot, hitStranger >>
                  /\ pc' = [pc EXCEPT ![self] = "StartIf"]
                  /\ UNCHANGED << next, lock, reissued, stoppedListening, 
                                  keptGone, g, tried, seen, conn, retried, 
                                  reqs >>

FailUnlock(self) == /\ pc[self] = "FailUnlock"
                    /\ lock' = 0
                    /\ pc' = [pc EXCEPT ![self] = "Req"]
                    /\ UNCHANGED << d, slot, next, reissued, hitStranger, 
                                    stoppedListening, keptGone, g, tried, seen, 
                                    conn, retried, reqs >>

Exchange(self) == /\ pc[self] = "Exchange"
                  /\ \/ /\ d[conn[self]] \in {"listening", "deaf", "unreachable"}
                     \/ /\ d[conn[self]] \in {"zombie", "reaped"}
                  /\ pc' = [pc EXCEPT ![self] = "Req"]
                  /\ UNCHANGED << d, slot, next, lock, reissued, hitStranger, 
                                  stoppedListening, keptGone, g, tried, seen, 
                                  conn, retried, reqs >>

Fwd(self) == Req(self) \/ Lock1(self) \/ Check(self) \/ Reap1(self)
                \/ Lock1b(self) \/ StartIf(self) \/ WaitReady(self)
                \/ ReapStart(self) \/ Got(self) \/ Connect(self)
                \/ Refused(self) \/ Lock2(self) \/ Stop(self)
                \/ ReapStop(self) \/ FailUnlock(self) \/ Exchange(self)

Life(self) == /\ pc[self] = "Life"
              /\ d[self - 10] # "none"
              /\ IF d[self - 10] = "starting"
                    THEN /\ \/ /\ d' = [d EXCEPT ![self - 10] = "listening"]
                            \/ /\ d' = [d EXCEPT ![self - 10] = "deaf"]
                            \/ /\ d' = [d EXCEPT ![self - 10] = "zombie"]
                    ELSE /\ TRUE
                         /\ d' = d
              /\ pc' = [pc EXCEPT ![self] = "Later"]
              /\ UNCHANGED << slot, next, lock, reissued, hitStranger, 
                              stoppedListening, keptGone, g, tried, seen, conn, 
                              retried, reqs >>

Later(self) == /\ pc[self] = "Later"
               /\ IF d[self - 10] = "listening"
                     THEN /\ \/ /\ d' = [d EXCEPT ![self - 10] = "zombie"]
                             \/ /\ d' = [d EXCEPT ![self - 10] = "deaf"]
                             \/ /\ d' = [d EXCEPT ![self - 10] = "unreachable"]
                             \/ /\ TRUE
                                /\ d' = d
                     ELSE /\ TRUE
                          /\ d' = d
               /\ pc' = [pc EXCEPT ![self] = "Done"]
               /\ UNCHANGED << slot, next, lock, reissued, hitStranger, 
                               stoppedListening, keptGone, g, tried, seen, 
                               conn, retried, reqs >>

Daemon(self) == Life(self) \/ Later(self)

Reissue == /\ pc[101] = "Reissue"
           /\ \E h \in {x \in Gens : d[x] = "reaped" /\ ~reissued[x]}:
                reissued' = [reissued EXCEPT ![h] = TRUE]
           /\ pc' = [pc EXCEPT ![101] = "Reissue"]
           /\ UNCHANGED << d, slot, next, lock, hitStranger, stoppedListening, 
                           keptGone, g, tried, seen, conn, retried, reqs >>

Kernel == Reissue

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Kernel
           \/ (\E self \in Fwds: Fwd(self))
           \/ (\E self \in {10 + x : x \in Gens}: Daemon(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Fwds : WF_vars(Fwd(self))
        /\ \A self \in {10 + x : x \in Gens} : WF_vars(Daemon(self))
        /\ WF_vars(Kernel)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 

=============================================================================
