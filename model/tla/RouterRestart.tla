----------------------------- MODULE RouterRestart ----------------------------
(* The router's daemon for one program (model/daemon.md DAEMON-12). The     *)
(* forwarder starts a daemon and records it in the program's slot. A daemon *)
(* may exit at any time, or never become ready. When it is not ready or a   *)
(* connection fails, the forwarder stops the daemon (SIGTERM, then SIGKILL) *)
(* and reaps it, clearing the slot first, before starting another. A signal *)
(* handler on another thread may send SIGTERM to the program's daemon at    *)
(* any time. Once a daemon is reaped the kernel may hand its pid to a       *)
(* stranger.                                                               *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "restart_without_stop": a restart clears the slot and starts another daemon.
\* "raw_pid": the handler reads the pid, then signals it in a later step.

Gens == 1..2

(* --algorithm RouterRestart
variables
    d = [g \in Gens |-> "none"],
    slot = 0,
    reissued = [g \in Gens |-> FALSE],
    hitStranger = FALSE;

define
    NoUntrackedDaemon == \A x \in Gens : d[x] = "alive" => slot = x
    NoSignalReachesAStranger == ~hitStranger
end define;

macro signal(k, sig) begin
  if d[k] = "reaped" then
    if reissued[k] then hitStranger := TRUE; end if;
  elsif d[k] = "alive" then
    if sig = "KILL" then
      d[k] := "zombie";
    else
      either d[k] := "zombie"; or skip; end either;
    end if;
  end if;
end macro;

macro reap(k) begin
  slot := 0;
  d[k] := "reaped";
end macro;

fair process Forwarder = 100
variables gen = 1;
begin
  Start:
    d[gen] := "alive";
    slot := gen;
  Ready:
    either
      goto Done1;
    or
      skip;
    end either;
  Restart:
    if Variant = "restart_without_stop" then
      slot := 0;
      goto Advance;
    end if;
  Term:
    if d[gen] = "alive" then signal(gen, "TERM"); end if;
  Kill:
    if d[gen] = "alive" then signal(gen, "KILL"); end if;
  Reap:
    await d[gen] = "zombie";
    reap(gen);
  Advance:
    if gen < 2 then
      gen := gen + 1;
      goto Start;
    end if;
  Done1:
    skip;
end process;

fair process Daemon \in {10 + x : x \in Gens}
begin
  Exit:
    await d[self - 10] # "none";
    either
      if d[self - 10] = "alive" then d[self - 10] := "zombie"; end if;
    or
      skip;
    end either;
end process;

fair process Kernel = 101
begin
  Reissue:
    with h \in Gens do
      await d[h] = "reaped";
      reissued[h] := TRUE;
    end with;
end process;

fair process Handler = 102
variables seen = 0;
begin
  Read:
    seen := slot;
    if Variant # "raw_pid" /\ seen > 0 then
      signal(seen, "TERM");
      goto HDone;
    end if;
  Send:
    if seen > 0 then signal(seen, "TERM"); end if;
  HDone:
    skip;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "77016a3e" /\ chksum(tla) = "cce37909")
VARIABLES d, slot, reissued, hitStranger, pc

(* define statement *)
NoUntrackedDaemon == \A x \in Gens : d[x] = "alive" => slot = x
NoSignalReachesAStranger == ~hitStranger

VARIABLES gen, seen

vars == << d, slot, reissued, hitStranger, pc, gen, seen >>

ProcSet == {100} \cup ({10 + x : x \in Gens}) \cup {101} \cup {102}

Init == (* Global variables *)
        /\ d = [g \in Gens |-> "none"]
        /\ slot = 0
        /\ reissued = [g \in Gens |-> FALSE]
        /\ hitStranger = FALSE
        (* Process Forwarder *)
        /\ gen = 1
        (* Process Handler *)
        /\ seen = 0
        /\ pc = [self \in ProcSet |-> CASE self = 100 -> "Start"
                                        [] self \in {10 + x : x \in Gens} -> "Exit"
                                        [] self = 101 -> "Reissue"
                                        [] self = 102 -> "Read"]

Start == /\ pc[100] = "Start"
         /\ d' = [d EXCEPT ![gen] = "alive"]
         /\ slot' = gen
         /\ pc' = [pc EXCEPT ![100] = "Ready"]
         /\ UNCHANGED << reissued, hitStranger, gen, seen >>

Ready == /\ pc[100] = "Ready"
         /\ \/ /\ pc' = [pc EXCEPT ![100] = "Done1"]
            \/ /\ TRUE
               /\ pc' = [pc EXCEPT ![100] = "Restart"]
         /\ UNCHANGED << d, slot, reissued, hitStranger, gen, seen >>

Restart == /\ pc[100] = "Restart"
           /\ IF Variant = "restart_without_stop"
                 THEN /\ slot' = 0
                      /\ pc' = [pc EXCEPT ![100] = "Advance"]
                 ELSE /\ pc' = [pc EXCEPT ![100] = "Term"]
                      /\ slot' = slot
           /\ UNCHANGED << d, reissued, hitStranger, gen, seen >>

Term == /\ pc[100] = "Term"
        /\ IF d[gen] = "alive"
              THEN /\ IF d[gen] = "reaped"
                         THEN /\ IF reissued[gen]
                                    THEN /\ hitStranger' = TRUE
                                    ELSE /\ TRUE
                                         /\ UNCHANGED hitStranger
                              /\ d' = d
                         ELSE /\ IF d[gen] = "alive"
                                    THEN /\ IF "TERM" = "KILL"
                                               THEN /\ d' = [d EXCEPT ![gen] = "zombie"]
                                               ELSE /\ \/ /\ d' = [d EXCEPT ![gen] = "zombie"]
                                                       \/ /\ TRUE
                                                          /\ d' = d
                                    ELSE /\ TRUE
                                         /\ d' = d
                              /\ UNCHANGED hitStranger
              ELSE /\ TRUE
                   /\ UNCHANGED << d, hitStranger >>
        /\ pc' = [pc EXCEPT ![100] = "Kill"]
        /\ UNCHANGED << slot, reissued, gen, seen >>

Kill == /\ pc[100] = "Kill"
        /\ IF d[gen] = "alive"
              THEN /\ IF d[gen] = "reaped"
                         THEN /\ IF reissued[gen]
                                    THEN /\ hitStranger' = TRUE
                                    ELSE /\ TRUE
                                         /\ UNCHANGED hitStranger
                              /\ d' = d
                         ELSE /\ IF d[gen] = "alive"
                                    THEN /\ IF "KILL" = "KILL"
                                               THEN /\ d' = [d EXCEPT ![gen] = "zombie"]
                                               ELSE /\ \/ /\ d' = [d EXCEPT ![gen] = "zombie"]
                                                       \/ /\ TRUE
                                                          /\ d' = d
                                    ELSE /\ TRUE
                                         /\ d' = d
                              /\ UNCHANGED hitStranger
              ELSE /\ TRUE
                   /\ UNCHANGED << d, hitStranger >>
        /\ pc' = [pc EXCEPT ![100] = "Reap"]
        /\ UNCHANGED << slot, reissued, gen, seen >>

Reap == /\ pc[100] = "Reap"
        /\ d[gen] = "zombie"
        /\ slot' = 0
        /\ d' = [d EXCEPT ![gen] = "reaped"]
        /\ pc' = [pc EXCEPT ![100] = "Advance"]
        /\ UNCHANGED << reissued, hitStranger, gen, seen >>

Advance == /\ pc[100] = "Advance"
           /\ IF gen < 2
                 THEN /\ gen' = gen + 1
                      /\ pc' = [pc EXCEPT ![100] = "Start"]
                 ELSE /\ pc' = [pc EXCEPT ![100] = "Done1"]
                      /\ gen' = gen
           /\ UNCHANGED << d, slot, reissued, hitStranger, seen >>

Done1 == /\ pc[100] = "Done1"
         /\ TRUE
         /\ pc' = [pc EXCEPT ![100] = "Done"]
         /\ UNCHANGED << d, slot, reissued, hitStranger, gen, seen >>

Forwarder == Start \/ Ready \/ Restart \/ Term \/ Kill \/ Reap \/ Advance
                \/ Done1

Exit(self) == /\ pc[self] = "Exit"
              /\ d[self - 10] # "none"
              /\ \/ /\ IF d[self - 10] = "alive"
                          THEN /\ d' = [d EXCEPT ![self - 10] = "zombie"]
                          ELSE /\ TRUE
                               /\ d' = d
                 \/ /\ TRUE
                    /\ d' = d
              /\ pc' = [pc EXCEPT ![self] = "Done"]
              /\ UNCHANGED << slot, reissued, hitStranger, gen, seen >>

Daemon(self) == Exit(self)

Reissue == /\ pc[101] = "Reissue"
           /\ \E h \in Gens:
                /\ d[h] = "reaped"
                /\ reissued' = [reissued EXCEPT ![h] = TRUE]
           /\ pc' = [pc EXCEPT ![101] = "Done"]
           /\ UNCHANGED << d, slot, hitStranger, gen, seen >>

Kernel == Reissue

Read == /\ pc[102] = "Read"
        /\ seen' = slot
        /\ IF Variant # "raw_pid" /\ seen' > 0
              THEN /\ IF d[seen'] = "reaped"
                         THEN /\ IF reissued[seen']
                                    THEN /\ hitStranger' = TRUE
                                    ELSE /\ TRUE
                                         /\ UNCHANGED hitStranger
                              /\ d' = d
                         ELSE /\ IF d[seen'] = "alive"
                                    THEN /\ IF "TERM" = "KILL"
                                               THEN /\ d' = [d EXCEPT ![seen'] = "zombie"]
                                               ELSE /\ \/ /\ d' = [d EXCEPT ![seen'] = "zombie"]
                                                       \/ /\ TRUE
                                                          /\ d' = d
                                    ELSE /\ TRUE
                                         /\ d' = d
                              /\ UNCHANGED hitStranger
                   /\ pc' = [pc EXCEPT ![102] = "HDone"]
              ELSE /\ pc' = [pc EXCEPT ![102] = "Send"]
                   /\ UNCHANGED << d, hitStranger >>
        /\ UNCHANGED << slot, reissued, gen >>

Send == /\ pc[102] = "Send"
        /\ IF seen > 0
              THEN /\ IF d[seen] = "reaped"
                         THEN /\ IF reissued[seen]
                                    THEN /\ hitStranger' = TRUE
                                    ELSE /\ TRUE
                                         /\ UNCHANGED hitStranger
                              /\ d' = d
                         ELSE /\ IF d[seen] = "alive"
                                    THEN /\ IF "TERM" = "KILL"
                                               THEN /\ d' = [d EXCEPT ![seen] = "zombie"]
                                               ELSE /\ \/ /\ d' = [d EXCEPT ![seen] = "zombie"]
                                                       \/ /\ TRUE
                                                          /\ d' = d
                                    ELSE /\ TRUE
                                         /\ d' = d
                              /\ UNCHANGED hitStranger
              ELSE /\ TRUE
                   /\ UNCHANGED << d, hitStranger >>
        /\ pc' = [pc EXCEPT ![102] = "HDone"]
        /\ UNCHANGED << slot, reissued, gen, seen >>

HDone == /\ pc[102] = "HDone"
         /\ TRUE
         /\ pc' = [pc EXCEPT ![102] = "Done"]
         /\ UNCHANGED << d, slot, reissued, hitStranger, gen, seen >>

Handler == Read \/ Send \/ HDone

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Forwarder \/ Kernel \/ Handler
           \/ (\E self \in {10 + x : x \in Gens}: Daemon(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Forwarder)
        /\ \A self \in {10 + x : x \in Gens} : WF_vars(Daemon(self))
        /\ WF_vars(Kernel)
        /\ WF_vars(Handler)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
