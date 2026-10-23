----------------------------- MODULE StreamQueue -----------------------------
(* A written stream under the custodian (model/runtime/streams.md SLOT-10..15).  *)
(* Writers append to one shared buffer under the slot lock. A full buffer is  *)
(* moved onto a bounded queue, which has its own lock, still under the slot  *)
(* lock; a writer that finds the queue full waits there for the custodian.   *)
(* The closer, which also stands for the finalize at run exit, queues the    *)
(* tail and a close marker. The custodian pops in order, writes, and writes  *)
(* the footer at the marker, or a failed footer when a writer died inside    *)
(* the lock. A reader opens the file after the close returned and waits      *)
(* until everything queued is written. A process forked by a writer is one   *)
(* more writer: it holds no stream state of its own.                         *)
EXTENDS Naturals, Sequences

CONSTANTS Writers, N, BufCap, QDepth, Variant
\* "design": the design.
\* "pop_locks": the custodian's pop takes the slot lock.
\* "enqueue_unlocked": a writer drops the slot lock to queue a full buffer.
\* "close_before_tail": the close queues its marker before the tail.
\* "writer_frees": a dying writer's batches still on the queue are freed.
\* "reader_no_wait": the reader does not wait for the custodian.

Id(e) == <<e[1], e[2]>>

(* --algorithm StreamQueue
variables
    lock = "none",
    poisoned = FALSE,
    closed = FALSE,
    buf = <<>>,
    queue = <<>>,
    appended = <<>>,
    file = <<>>,
    footer = "none",
    queued = 0,
    written = 0,
    sawUnfinished = FALSE;

define
    FileFollowsLockOrder == file = SubSeq(appended, 1, Len(file))
    WritesAreContiguous ==
        \A p, q, r \in 1..Len(appended) :
            (p < q /\ q < r /\ Id(appended[p]) = Id(appended[r]))
                => Id(appended[q]) = Id(appended[p])
    AClosedFileHoldsEveryWrite == footer = "closed" => file = appended
    AReaderSeesAFinishedFile == ~sawUnfinished
    EveryStreamEnds == <>(footer /= "none")
end define;

fair process Writer \in Writers
variables i = 0, pend = <<>>;
begin
  Lock:
    either
      await lock = "none";
      lock := self;
    or
      goto Done;
    end either;
  Check:
    if closed \/ poisoned then
      lock := "none";
      goto Done;
    end if;
  Fill:
    while i < N do
      Room:
        either
          if Len(buf) = BufCap then
            if Variant = "enqueue_unlocked" then
              pend := buf;
              buf := <<>>;
              lock := "none";
              Enq:
                await Len(queue) < QDepth;
                queue := Append(queue, [kind |-> "data", elems |-> pend, by |-> self]);
                queued := queued + 1;
              Relock:
                await lock = "none";
                lock := self;
            else
              await Len(queue) < QDepth;
              queue := Append(queue, [kind |-> "data", elems |-> buf, by |-> self]);
              queued := queued + 1;
              buf := <<>>;
            end if;
          end if;
        or
          poisoned := TRUE;
          lock := "none";
          if Variant = "writer_frees" then
            queue := SelectSeq(queue, LAMBDA b : b.by /= self);
          end if;
          goto Done;
        end either;
      Put:
        buf := Append(buf, <<self, 1, i>>);
        appended := Append(appended, <<self, 1, i>>);
        i := i + 1;
    end while;
  Unlock:
    lock := "none";
  After:
    either
      skip;
    or
      if Variant = "writer_frees" then
        queue := SelectSeq(queue, LAMBDA b : b.by /= self);
      end if;
    end either;
end process;

fair process Closer = "closer"
begin
  Close:
    await lock = "none";
    lock := "closer";
  Decide:
    if poisoned then
      lock := "none";
      goto Done;
    end if;
  First:
    if Variant = "close_before_tail" then
      await Len(queue) < QDepth;
      queue := Append(queue, [kind |-> "close", elems |-> <<>>, by |-> "closer"]);
      queued := queued + 1;
    elsif buf /= <<>> then
      await Len(queue) < QDepth;
      queue := Append(queue, [kind |-> "data", elems |-> buf, by |-> "closer"]);
      queued := queued + 1;
      buf := <<>>;
    end if;
  Second:
    if Variant = "close_before_tail" then
      if buf /= <<>> then
        await Len(queue) < QDepth;
        queue := Append(queue, [kind |-> "data", elems |-> buf, by |-> "closer"]);
        queued := queued + 1;
        buf := <<>>;
      end if;
    else
      await Len(queue) < QDepth;
      queue := Append(queue, [kind |-> "close", elems |-> <<>>, by |-> "closer"]);
      queued := queued + 1;
    end if;
  Closed:
    closed := TRUE;
    lock := "none";
end process;

fair process Custodian = "custodian"
variables batch = [kind |-> "none", elems |-> <<>>, by |-> "none"];
begin
  Loop:
    while footer = "none" do
      Pop:
        either
          await queue /= <<>>;
          await Variant /= "pop_locks" \/ lock = "none";
          batch := Head(queue);
          queue := Tail(queue);
          Write:
            if footer = "none" then
              if batch.kind = "data" then
                file := file \o batch.elems;
              else
                footer := "closed";
              end if;
            end if;
            written := written + 1;
        or
          await poisoned /\ queue = <<>>;
          footer := "failed";
        end either;
    end while;
end process;

fair process Reader = "reader"
begin
  Open:
    await pc["closer"] = "Done";
    if closed then
      Wait:
        await Variant = "reader_no_wait" \/ written = queued;
      Read:
        sawUnfinished := footer = "none";
    end if;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "7aa19b96" /\ chksum(tla) = "2c06dd55")
VARIABLES lock, poisoned, closed, buf, queue, appended, file, footer, queued, 
          written, sawUnfinished, pc

(* define statement *)
FileFollowsLockOrder == file = SubSeq(appended, 1, Len(file))
WritesAreContiguous ==
    \A p, q, r \in 1..Len(appended) :
        (p < q /\ q < r /\ Id(appended[p]) = Id(appended[r]))
            => Id(appended[q]) = Id(appended[p])
AClosedFileHoldsEveryWrite == footer = "closed" => file = appended
AReaderSeesAFinishedFile == ~sawUnfinished
EveryStreamEnds == <>(footer /= "none")

VARIABLES i, pend, batch

vars == << lock, poisoned, closed, buf, queue, appended, file, footer, queued, 
           written, sawUnfinished, pc, i, pend, batch >>

ProcSet == (Writers) \cup {"closer"} \cup {"custodian"} \cup {"reader"}

Init == (* Global variables *)
        /\ lock = "none"
        /\ poisoned = FALSE
        /\ closed = FALSE
        /\ buf = <<>>
        /\ queue = <<>>
        /\ appended = <<>>
        /\ file = <<>>
        /\ footer = "none"
        /\ queued = 0
        /\ written = 0
        /\ sawUnfinished = FALSE
        (* Process Writer *)
        /\ i = [self \in Writers |-> 0]
        /\ pend = [self \in Writers |-> <<>>]
        (* Process Custodian *)
        /\ batch = [kind |-> "none", elems |-> <<>>, by |-> "none"]
        /\ pc = [self \in ProcSet |-> CASE self \in Writers -> "Lock"
                                        [] self = "closer" -> "Close"
                                        [] self = "custodian" -> "Loop"
                                        [] self = "reader" -> "Open"]

Lock(self) == /\ pc[self] = "Lock"
              /\ \/ /\ lock = "none"
                    /\ lock' = self
                    /\ pc' = [pc EXCEPT ![self] = "Check"]
                 \/ /\ pc' = [pc EXCEPT ![self] = "Done"]
                    /\ lock' = lock
              /\ UNCHANGED << poisoned, closed, buf, queue, appended, file, 
                              footer, queued, written, sawUnfinished, i, pend, 
                              batch >>

Check(self) == /\ pc[self] = "Check"
               /\ IF closed \/ poisoned
                     THEN /\ lock' = "none"
                          /\ pc' = [pc EXCEPT ![self] = "Done"]
                     ELSE /\ pc' = [pc EXCEPT ![self] = "Fill"]
                          /\ lock' = lock
               /\ UNCHANGED << poisoned, closed, buf, queue, appended, file, 
                               footer, queued, written, sawUnfinished, i, pend, 
                               batch >>

Fill(self) == /\ pc[self] = "Fill"
              /\ IF i[self] < N
                    THEN /\ pc' = [pc EXCEPT ![self] = "Room"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Unlock"]
              /\ UNCHANGED << lock, poisoned, closed, buf, queue, appended, 
                              file, footer, queued, written, sawUnfinished, i, 
                              pend, batch >>

Room(self) == /\ pc[self] = "Room"
              /\ \/ /\ IF Len(buf) = BufCap
                          THEN /\ IF Variant = "enqueue_unlocked"
                                     THEN /\ pend' = [pend EXCEPT ![self] = buf]
                                          /\ buf' = <<>>
                                          /\ lock' = "none"
                                          /\ pc' = [pc EXCEPT ![self] = "Enq"]
                                          /\ UNCHANGED << queue, queued >>
                                     ELSE /\ Len(queue) < QDepth
                                          /\ queue' = Append(queue, [kind |-> "data", elems |-> buf, by |-> self])
                                          /\ queued' = queued + 1
                                          /\ buf' = <<>>
                                          /\ pc' = [pc EXCEPT ![self] = "Put"]
                                          /\ UNCHANGED << lock, pend >>
                          ELSE /\ pc' = [pc EXCEPT ![self] = "Put"]
                               /\ UNCHANGED << lock, buf, queue, queued, pend >>
                    /\ UNCHANGED poisoned
                 \/ /\ poisoned' = TRUE
                    /\ lock' = "none"
                    /\ IF Variant = "writer_frees"
                          THEN /\ queue' = SelectSeq(queue, LAMBDA b : b.by /= self)
                          ELSE /\ TRUE
                               /\ queue' = queue
                    /\ pc' = [pc EXCEPT ![self] = "Done"]
                    /\ UNCHANGED <<buf, queued, pend>>
              /\ UNCHANGED << closed, appended, file, footer, written, 
                              sawUnfinished, i, batch >>

Enq(self) == /\ pc[self] = "Enq"
             /\ Len(queue) < QDepth
             /\ queue' = Append(queue, [kind |-> "data", elems |-> pend[self], by |-> self])
             /\ queued' = queued + 1
             /\ pc' = [pc EXCEPT ![self] = "Relock"]
             /\ UNCHANGED << lock, poisoned, closed, buf, appended, file, 
                             footer, written, sawUnfinished, i, pend, batch >>

Relock(self) == /\ pc[self] = "Relock"
                /\ lock = "none"
                /\ lock' = self
                /\ pc' = [pc EXCEPT ![self] = "Put"]
                /\ UNCHANGED << poisoned, closed, buf, queue, appended, file, 
                                footer, queued, written, sawUnfinished, i, 
                                pend, batch >>

Put(self) == /\ pc[self] = "Put"
             /\ buf' = Append(buf, <<self, 1, i[self]>>)
             /\ appended' = Append(appended, <<self, 1, i[self]>>)
             /\ i' = [i EXCEPT ![self] = i[self] + 1]
             /\ pc' = [pc EXCEPT ![self] = "Fill"]
             /\ UNCHANGED << lock, poisoned, closed, queue, file, footer, 
                             queued, written, sawUnfinished, pend, batch >>

Unlock(self) == /\ pc[self] = "Unlock"
                /\ lock' = "none"
                /\ pc' = [pc EXCEPT ![self] = "After"]
                /\ UNCHANGED << poisoned, closed, buf, queue, appended, file, 
                                footer, queued, written, sawUnfinished, i, 
                                pend, batch >>

After(self) == /\ pc[self] = "After"
               /\ \/ /\ TRUE
                     /\ queue' = queue
                  \/ /\ IF Variant = "writer_frees"
                           THEN /\ queue' = SelectSeq(queue, LAMBDA b : b.by /= self)
                           ELSE /\ TRUE
                                /\ queue' = queue
               /\ pc' = [pc EXCEPT ![self] = "Done"]
               /\ UNCHANGED << lock, poisoned, closed, buf, appended, file, 
                               footer, queued, written, sawUnfinished, i, pend, 
                               batch >>

Writer(self) == Lock(self) \/ Check(self) \/ Fill(self) \/ Room(self)
                   \/ Enq(self) \/ Relock(self) \/ Put(self)
                   \/ Unlock(self) \/ After(self)

Close == /\ pc["closer"] = "Close"
         /\ lock = "none"
         /\ lock' = "closer"
         /\ pc' = [pc EXCEPT !["closer"] = "Decide"]
         /\ UNCHANGED << poisoned, closed, buf, queue, appended, file, footer, 
                         queued, written, sawUnfinished, i, pend, batch >>

Decide == /\ pc["closer"] = "Decide"
          /\ IF poisoned
                THEN /\ lock' = "none"
                     /\ pc' = [pc EXCEPT !["closer"] = "Done"]
                ELSE /\ pc' = [pc EXCEPT !["closer"] = "First"]
                     /\ lock' = lock
          /\ UNCHANGED << poisoned, closed, buf, queue, appended, file, footer, 
                          queued, written, sawUnfinished, i, pend, batch >>

First == /\ pc["closer"] = "First"
         /\ IF Variant = "close_before_tail"
               THEN /\ Len(queue) < QDepth
                    /\ queue' = Append(queue, [kind |-> "close", elems |-> <<>>, by |-> "closer"])
                    /\ queued' = queued + 1
                    /\ buf' = buf
               ELSE /\ IF buf /= <<>>
                          THEN /\ Len(queue) < QDepth
                               /\ queue' = Append(queue, [kind |-> "data", elems |-> buf, by |-> "closer"])
                               /\ queued' = queued + 1
                               /\ buf' = <<>>
                          ELSE /\ TRUE
                               /\ UNCHANGED << buf, queue, queued >>
         /\ pc' = [pc EXCEPT !["closer"] = "Second"]
         /\ UNCHANGED << lock, poisoned, closed, appended, file, footer, 
                         written, sawUnfinished, i, pend, batch >>

Second == /\ pc["closer"] = "Second"
          /\ IF Variant = "close_before_tail"
                THEN /\ IF buf /= <<>>
                           THEN /\ Len(queue) < QDepth
                                /\ queue' = Append(queue, [kind |-> "data", elems |-> buf, by |-> "closer"])
                                /\ queued' = queued + 1
                                /\ buf' = <<>>
                           ELSE /\ TRUE
                                /\ UNCHANGED << buf, queue, queued >>
                ELSE /\ Len(queue) < QDepth
                     /\ queue' = Append(queue, [kind |-> "close", elems |-> <<>>, by |-> "closer"])
                     /\ queued' = queued + 1
                     /\ buf' = buf
          /\ pc' = [pc EXCEPT !["closer"] = "Closed"]
          /\ UNCHANGED << lock, poisoned, closed, appended, file, footer, 
                          written, sawUnfinished, i, pend, batch >>

Closed == /\ pc["closer"] = "Closed"
          /\ closed' = TRUE
          /\ lock' = "none"
          /\ pc' = [pc EXCEPT !["closer"] = "Done"]
          /\ UNCHANGED << poisoned, buf, queue, appended, file, footer, queued, 
                          written, sawUnfinished, i, pend, batch >>

Closer == Close \/ Decide \/ First \/ Second \/ Closed

Loop == /\ pc["custodian"] = "Loop"
        /\ IF footer = "none"
              THEN /\ pc' = [pc EXCEPT !["custodian"] = "Pop"]
              ELSE /\ pc' = [pc EXCEPT !["custodian"] = "Done"]
        /\ UNCHANGED << lock, poisoned, closed, buf, queue, appended, file, 
                        footer, queued, written, sawUnfinished, i, pend, batch >>

Pop == /\ pc["custodian"] = "Pop"
       /\ \/ /\ queue /= <<>>
             /\ Variant /= "pop_locks" \/ lock = "none"
             /\ batch' = Head(queue)
             /\ queue' = Tail(queue)
             /\ pc' = [pc EXCEPT !["custodian"] = "Write"]
             /\ UNCHANGED footer
          \/ /\ poisoned /\ queue = <<>>
             /\ footer' = "failed"
             /\ pc' = [pc EXCEPT !["custodian"] = "Loop"]
             /\ UNCHANGED <<queue, batch>>
       /\ UNCHANGED << lock, poisoned, closed, buf, appended, file, queued, 
                       written, sawUnfinished, i, pend >>

Write == /\ pc["custodian"] = "Write"
         /\ IF footer = "none"
               THEN /\ IF batch.kind = "data"
                          THEN /\ file' = file \o batch.elems
                               /\ UNCHANGED footer
                          ELSE /\ footer' = "closed"
                               /\ file' = file
               ELSE /\ TRUE
                    /\ UNCHANGED << file, footer >>
         /\ written' = written + 1
         /\ pc' = [pc EXCEPT !["custodian"] = "Loop"]
         /\ UNCHANGED << lock, poisoned, closed, buf, queue, appended, queued, 
                         sawUnfinished, i, pend, batch >>

Custodian == Loop \/ Pop \/ Write

Open == /\ pc["reader"] = "Open"
        /\ pc["closer"] = "Done"
        /\ IF closed
              THEN /\ pc' = [pc EXCEPT !["reader"] = "Wait"]
              ELSE /\ pc' = [pc EXCEPT !["reader"] = "Done"]
        /\ UNCHANGED << lock, poisoned, closed, buf, queue, appended, file, 
                        footer, queued, written, sawUnfinished, i, pend, batch >>

Wait == /\ pc["reader"] = "Wait"
        /\ Variant = "reader_no_wait" \/ written = queued
        /\ pc' = [pc EXCEPT !["reader"] = "Read"]
        /\ UNCHANGED << lock, poisoned, closed, buf, queue, appended, file, 
                        footer, queued, written, sawUnfinished, i, pend, batch >>

Read == /\ pc["reader"] = "Read"
        /\ sawUnfinished' = (footer = "none")
        /\ pc' = [pc EXCEPT !["reader"] = "Done"]
        /\ UNCHANGED << lock, poisoned, closed, buf, queue, appended, file, 
                        footer, queued, written, i, pend, batch >>

Reader == Open \/ Wait \/ Read

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Closer \/ Custodian \/ Reader
           \/ (\E self \in Writers: Writer(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Writers : WF_vars(Writer(self))
        /\ WF_vars(Closer)
        /\ WF_vars(Custodian)
        /\ WF_vars(Reader)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
