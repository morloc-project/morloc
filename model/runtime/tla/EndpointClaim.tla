----------------------------- MODULE EndpointClaim ----------------------------
(* Three daemons start at the same socket path (model/runtime/network.md NET-6). The *)
(* path may hold nothing, a stale socket file left by a dead daemon, or a  *)
(* live daemon's socket. A daemon opens the lock file beside the path,     *)
(* then takes its lock without waiting, and starts over if the file it    *)
(* locked is no longer the one at that path; it binds, and on finding the path  *)
(* in use connects to it: an answer means a live listener, and it refuses; *)
(* a refusal means a stale file, which it unlinks before binding again. A  *)
(* serving daemon may die, leaving its socket file stale and its lock      *)
(* released, or exit cleanly, removing its socket file and then its lock   *)
(* file while it still holds the lock. A removed lock file is replaced by  *)
(* a new one on the next open.                                             *)
EXTENDS Integers

CONSTANT Variant
\* "design": the design.
\* "unlocked": the probe and unlink run without the lock.
\* "unconditional": the daemon unlinks the path, then binds.
\* "release_then_unlink": a clean exit releases the lock, then removes the lock file.
\* "unverified": the daemon does not check that the file it locked is still the lock file.

D == {1, 2, 3}
None == 0
Stale == -1
Inodes == 1..4

(* --algorithm EndpointClaim
variables
    file \in {None, Stale},
    listening = [i \in D |-> FALSE],
    lockFile = 1,
    holder = [n \in Inodes |-> 0],
    stolen = FALSE;

define
    NoLiveListenerUnlinked == ~stolen
    Claiming(i) == pc[i] \in {"Bind", "Probe", "Unlink", "Rebind"}
    OneClaimant == \A i, j \in D : i # j => ~(Claiming(i) /\ Claiming(j))
    Live(f) == f \in D /\ listening[f]
end define;

fair process Daemon \in D
variables mine = 0;
begin
  Open:
    if Variant = "unconditional" then
      goto Unlink;
    else
      mine := lockFile;
    end if;
  Lock:
    if Variant \in {"design", "release_then_unlink", "unverified"} then
      if holder[mine] = 0 then
        holder[mine] := self;
      else
        goto Finish;
      end if;
    end if;
  Verify:
    if Variant # "unverified" /\ Variant # "unconditional" /\ Variant # "unlocked" /\ lockFile # mine then
      holder[mine] := 0;
      goto Open;
    end if;
  Bind:
    if file = None then
      file := self;
      listening[self] := TRUE;
      goto Serve;
    end if;
  Probe:
    if Live(file) then goto Release; end if;
  Unlink:
    if Live(file) then stolen := TRUE; end if;
    file := None;
  Rebind:
    if file = None then
      file := self;
      listening[self] := TRUE;
      goto Serve;
    else
      goto Release;
    end if;
  Serve:
    either
      listening[self] := FALSE;
      goto Release;
    or
      listening[self] := FALSE;
      if file = self then file := None; end if;
    end either;
  Tidy:
    if Variant = "release_then_unlink" then
      if mine > 0 then holder[mine] := 0; end if;
    elsif mine > 0 /\ lockFile = mine /\ lockFile < 4 then
      lockFile := lockFile + 1;
    end if;
  Tidy2:
    if Variant = "release_then_unlink" then
      if lockFile = mine /\ lockFile < 4 then lockFile := lockFile + 1; end if;
      goto Finish;
    end if;
  Release:
    if mine > 0 /\ holder[mine] = self then holder[mine] := 0; end if;
  Finish:
    skip;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "6482f960" /\ chksum(tla) = "b8fb6e72")
VARIABLES file, listening, lockFile, holder, stolen, pc

(* define statement *)
NoLiveListenerUnlinked == ~stolen
Claiming(i) == pc[i] \in {"Bind", "Probe", "Unlink", "Rebind"}
OneClaimant == \A i, j \in D : i # j => ~(Claiming(i) /\ Claiming(j))
Live(f) == f \in D /\ listening[f]

VARIABLE mine

vars == << file, listening, lockFile, holder, stolen, pc, mine >>

ProcSet == (D)

Init == (* Global variables *)
        /\ file \in {None, Stale}
        /\ listening = [i \in D |-> FALSE]
        /\ lockFile = 1
        /\ holder = [n \in Inodes |-> 0]
        /\ stolen = FALSE
        (* Process Daemon *)
        /\ mine = [self \in D |-> 0]
        /\ pc = [self \in ProcSet |-> "Open"]

Open(self) == /\ pc[self] = "Open"
              /\ IF Variant = "unconditional"
                    THEN /\ pc' = [pc EXCEPT ![self] = "Unlink"]
                         /\ mine' = mine
                    ELSE /\ mine' = [mine EXCEPT ![self] = lockFile]
                         /\ pc' = [pc EXCEPT ![self] = "Lock"]
              /\ UNCHANGED << file, listening, lockFile, holder, stolen >>

Lock(self) == /\ pc[self] = "Lock"
              /\ IF Variant \in {"design", "release_then_unlink", "unverified"}
                    THEN /\ IF holder[mine[self]] = 0
                               THEN /\ holder' = [holder EXCEPT ![mine[self]] = self]
                                    /\ pc' = [pc EXCEPT ![self] = "Verify"]
                               ELSE /\ pc' = [pc EXCEPT ![self] = "Finish"]
                                    /\ UNCHANGED holder
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Verify"]
                         /\ UNCHANGED holder
              /\ UNCHANGED << file, listening, lockFile, stolen, mine >>

Verify(self) == /\ pc[self] = "Verify"
                /\ IF Variant # "unverified" /\ Variant # "unconditional" /\ Variant # "unlocked" /\ lockFile # mine[self]
                      THEN /\ holder' = [holder EXCEPT ![mine[self]] = 0]
                           /\ pc' = [pc EXCEPT ![self] = "Open"]
                      ELSE /\ pc' = [pc EXCEPT ![self] = "Bind"]
                           /\ UNCHANGED holder
                /\ UNCHANGED << file, listening, lockFile, stolen, mine >>

Bind(self) == /\ pc[self] = "Bind"
              /\ IF file = None
                    THEN /\ file' = self
                         /\ listening' = [listening EXCEPT ![self] = TRUE]
                         /\ pc' = [pc EXCEPT ![self] = "Serve"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Probe"]
                         /\ UNCHANGED << file, listening >>
              /\ UNCHANGED << lockFile, holder, stolen, mine >>

Probe(self) == /\ pc[self] = "Probe"
               /\ IF Live(file)
                     THEN /\ pc' = [pc EXCEPT ![self] = "Release"]
                     ELSE /\ pc' = [pc EXCEPT ![self] = "Unlink"]
               /\ UNCHANGED << file, listening, lockFile, holder, stolen, mine >>

Unlink(self) == /\ pc[self] = "Unlink"
                /\ IF Live(file)
                      THEN /\ stolen' = TRUE
                      ELSE /\ TRUE
                           /\ UNCHANGED stolen
                /\ file' = None
                /\ pc' = [pc EXCEPT ![self] = "Rebind"]
                /\ UNCHANGED << listening, lockFile, holder, mine >>

Rebind(self) == /\ pc[self] = "Rebind"
                /\ IF file = None
                      THEN /\ file' = self
                           /\ listening' = [listening EXCEPT ![self] = TRUE]
                           /\ pc' = [pc EXCEPT ![self] = "Serve"]
                      ELSE /\ pc' = [pc EXCEPT ![self] = "Release"]
                           /\ UNCHANGED << file, listening >>
                /\ UNCHANGED << lockFile, holder, stolen, mine >>

Serve(self) == /\ pc[self] = "Serve"
               /\ \/ /\ listening' = [listening EXCEPT ![self] = FALSE]
                     /\ pc' = [pc EXCEPT ![self] = "Release"]
                     /\ file' = file
                  \/ /\ listening' = [listening EXCEPT ![self] = FALSE]
                     /\ IF file = self
                           THEN /\ file' = None
                           ELSE /\ TRUE
                                /\ file' = file
                     /\ pc' = [pc EXCEPT ![self] = "Tidy"]
               /\ UNCHANGED << lockFile, holder, stolen, mine >>

Tidy(self) == /\ pc[self] = "Tidy"
              /\ IF Variant = "release_then_unlink"
                    THEN /\ IF mine[self] > 0
                               THEN /\ holder' = [holder EXCEPT ![mine[self]] = 0]
                               ELSE /\ TRUE
                                    /\ UNCHANGED holder
                         /\ UNCHANGED lockFile
                    ELSE /\ IF mine[self] > 0 /\ lockFile = mine[self] /\ lockFile < 4
                               THEN /\ lockFile' = lockFile + 1
                               ELSE /\ TRUE
                                    /\ UNCHANGED lockFile
                         /\ UNCHANGED holder
              /\ pc' = [pc EXCEPT ![self] = "Tidy2"]
              /\ UNCHANGED << file, listening, stolen, mine >>

Tidy2(self) == /\ pc[self] = "Tidy2"
               /\ IF Variant = "release_then_unlink"
                     THEN /\ IF lockFile = mine[self] /\ lockFile < 4
                                THEN /\ lockFile' = lockFile + 1
                                ELSE /\ TRUE
                                     /\ UNCHANGED lockFile
                          /\ pc' = [pc EXCEPT ![self] = "Finish"]
                     ELSE /\ pc' = [pc EXCEPT ![self] = "Release"]
                          /\ UNCHANGED lockFile
               /\ UNCHANGED << file, listening, holder, stolen, mine >>

Release(self) == /\ pc[self] = "Release"
                 /\ IF mine[self] > 0 /\ holder[mine[self]] = self
                       THEN /\ holder' = [holder EXCEPT ![mine[self]] = 0]
                       ELSE /\ TRUE
                            /\ UNCHANGED holder
                 /\ pc' = [pc EXCEPT ![self] = "Finish"]
                 /\ UNCHANGED << file, listening, lockFile, stolen, mine >>

Finish(self) == /\ pc[self] = "Finish"
                /\ TRUE
                /\ pc' = [pc EXCEPT ![self] = "Done"]
                /\ UNCHANGED << file, listening, lockFile, holder, stolen, 
                                mine >>

Daemon(self) == Open(self) \/ Lock(self) \/ Verify(self) \/ Bind(self)
                   \/ Probe(self) \/ Unlink(self) \/ Rebind(self)
                   \/ Serve(self) \/ Tidy(self) \/ Tidy2(self)
                   \/ Release(self) \/ Finish(self)

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == (\E self \in D: Daemon(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in D : WF_vars(Daemon(self))

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
