---------------------------- MODULE EncodeInPlace ----------------------------
(* Encoding a @write batch in place in a stream's write buffer             *)
(* (model/runtime/streams.md SLOT-35, SLOT-11, SLOT-12, SLOT-14, SLOT-20).  *)
(* Writers take the slot lock and place their batch's elements into the     *)
(* slot's block, element by element; any element may fail to encode. A      *)
(* full block is sealed into the writer's private pending list and a spare  *)
(* or fresh block takes its place in the slot; `kept` holds the earlier     *)
(* batches' elements a sealed first block carries until it is queued. Block growth (a bigger block, the old    *)
(* one freed) and index growth (the bytes laid out for a bigger index) are  *)
(* published to the slot at once; the element counts and the queue are     *)
(* touched only at commit. Abort frees the writer's private blocks and      *)
(* puts back the first sealed block with the counts the batch started from. *)
(* A first block that fills before any of the batch's elements is placed  *)
(* holds earlier batches' elements alone and is handed on at once, as     *)
(* outside a batch; a seal waits for queue room before taking a block, so *)
(* the custodian's spare is usually there to take.                        *)
(* Each element's room is zeroed when it is placed, so neither abort nor    *)
(* the custodian zeroes anything. The custodian writes queued sub-packets   *)
(* to the file and gives each block back as a spare; it is the spare        *)
(* ring's only producer. With Py, the writers are Python threads of   *)
(* one process sharing a GIL with a third thread that mutates their         *)
(* objects; a writer encodes holding the GIL and never waits for the slot   *)
(* lock holding it. With Death, a writer can die at any step inside the     *)
(* lock, which fails the stream (SLOT-14) and leaks its private blocks.     *)
(* With CustFail, the custodian can fail (a closed pipe, a full disk): it   *)
(* writes nothing more and every later push is refused. A commit refused    *)
(* after its first push cannot be undone, since the custodian owns what it  *)
(* took: the writer frees its unpushed blocks and leaves the slot empty.   *)
(* Every private block holds some of the batch's own elements.            *)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS Variant, Py, Death, CustFail, Cap, Depth, NB, K
\* "design": the protocol above.
\* "seal_queues": a sealed block is queued at once, not at commit.
\* "counters_early": the slot's counts follow each placement.
\* "abort_gives_spare": abort pushes its blocks onto the spare ring.
\* "room_free_before_commit": a grown block stays private until commit
\*   (the old one is freed at once).
\* "grow_not_kept": abort puts back the index capacity it started with.
\* "place_no_zero": a room is placed on as it is, not zeroed first.
\* "gil_held_blocking": one Python writer waits for the slot lock holding
\*   the GIL.
\* "gil_released_between_size_and_write": a Python writer releases the GIL
\*   between sizing an element and writing it.
\* "refused_aborts": a commit refused after its first push aborts, putting
\*   back its first block.
\* "hand_on_ours": a first sealed block is handed on at once even when it
\*   holds the batch's own elements.

W == {"w1", "w2"}
BL == [w \in W |-> IF CustFail /\ w = "w2" THEN 1 ELSE 3]
Batch(w) == [i \in 1..BL[w] |-> <<w, i>>]
RECURSIVE Cat(_)
Cat(s) == IF s = <<>> THEN <<>> ELSE Batch(Head(s)) \o Cat(Tail(s))
RECURSIVE Elems(_)
Elems(q) == IF q = <<>> THEN <<>> ELSE Head(q).e \o Elems(Tail(q))
IsPrefix(a, b) == Len(a) <= Len(b) /\ SubSeq(b, 1, Len(a)) = a
Range(s) == {s[i] : i \in 1..Len(s)}
MUT == "mut"
NONE == "none"

(* --algorithm EncodeInPlace
variables
    lock = NONE,
    failed = FALSE,
    qfailed = FALSE,
    slotBlk = 1,
    slotBuf = <<>>,
    kept = <<>>,
    slotCap = 1,
    slotLayout = 1,
    used = [b \in 1..NB |-> b = 1],
    top = [b \in 1..NB |-> 0],
    queue = <<>>,
    file = <<>>,
    lost = <<>>,
    refused = NONE,
    rslot = [k \in 1..K |-> 0],
    rhead = 0,
    rtail = 0,
    done = <<>>,
    committing = NONE,
    gil = NONE,
    obj = [w \in W |-> 0],
    badPlace = FALSE,
    sizeMismatch = FALSE;

define
    View == file \o lost \o Elems(queue) \o kept \o slotBuf
    InCommit == IF committing = NONE THEN <<>> ELSE Batch(committing)
    Refd == IF refused = NONE THEN <<>> ELSE Batch(refused)
    BatchWholeOrAbsent ==
        IsPrefix(Cat(done), View) /\ IsPrefix(View, Cat(done) \o InCommit \o Refd)
    PlacedOnClean == ~badPlace
    SizedIsWritten == ~sizeMismatch
    SlotNamesLiveBlock == (lock = NONE /\ slotBlk /= 0) => used[slotBlk]
    LayoutAgrees == slotCap = slotLayout
    RingFits == rtail <= K
end define;

macro alloc(v) begin
    if rhead < rtail then
        v := rslot[rhead + 1];
        rhead := rhead + 1;
    else
        with b \in {x \in 1..NB : ~used[x]} do
            v := b;
            used[b] := TRUE;
        end with;
    end if;
end macro;

macro die() begin
    if Death then
        either skip;
        or
            failed := TRUE;
            lock := NONE;
            goto Done;
        end either;
    end if;
end macro;

fair process Wr \in W
variables wbuf = <<>>, wblk = 0, pend = <<>>, sealed = FALSE, i = 1,
    start = <<>>, startBlk = 0, startCap = 1, sz = 0, grown = FALSE, roomed = FALSE,
    nb = 0, out = <<>>, pt = 0, pushed = FALSE, firstBlk = 0,
    handed = FALSE;
begin
  GilFirst:
    if Py /\ Variant = "gil_held_blocking" /\ self = "w2" then
        await gil = NONE;
        gil := self;
    end if;
  Lock:
    await lock = NONE;
    if failed \/ qfailed then
        gil := IF gil = self THEN NONE ELSE gil;
        goto Done;
    else
        lock := self;
        start := slotBuf;
        startBlk := slotBlk;
        startCap := slotCap;
        wbuf := slotBuf;
        wblk := slotBlk;
    end if;
  Gil:
    if Py /\ gil /= self then
        await gil = NONE;
        gil := self;
    end if;
  Place:
    while i <= BL[self] do
      Choose:
        die();
      Choose2:
        either
            goto Abort;
        or
            if ~grown then
                either skip;
                or
                    grown := TRUE;
                    slotCap := 2;
                    slotLayout := 2;
                end either;
            end if;
        end either;
      Size:
        sz := obj[self];
        if Py /\ Variant = "gil_released_between_size_and_write" then
            gil := NONE;
        end if;
      Regain:
        if Py /\ Variant = "gil_released_between_size_and_write" then
            await gil = NONE;
            gil := self;
        end if;
      Seal:
        die();
      Seal2:
        if Len(wbuf) = Cap then
            if Variant = "seal_queues" then
                await Len(queue) < Depth;
                queue := Append(queue, [b |-> wblk, e |-> wbuf]);
                if wblk = slotBlk then
                    slotBuf := <<>>;
                    slotBlk := 0;
                end if;
                sealed := TRUE;
            elsif ~sealed /\ (wbuf = start \/ Variant = "hand_on_ours") then
                await Len(queue) < Depth \/ qfailed;
                if qfailed then
                    goto Abort;
                else
                    queue := Append(queue, [b |-> wblk, e |-> wbuf]);
                    slotBuf := <<>>;
                    slotBlk := 0;
                    start := <<>>;
                    handed := TRUE;
                end if;
            else
                await Len(queue) < Depth \/ qfailed;
                if qfailed then
                    goto Abort;
                else
                    pend := Append(pend, [b |-> wblk, e |-> wbuf]);
                    if ~sealed then
                        kept := slotBuf;
                        slotBuf := <<>>;
                        firstBlk := wblk;
                    end if;
                    sealed := TRUE;
                end if;
            end if;
        else
            nb := 0;
        end if;
      Alloc:
        if Len(wbuf) = Cap then
            wbuf := <<>>;
            alloc(nb);
        end if;
      NewBlock:
        if nb /= 0 then
            wblk := nb;
            if Variant /= "seal_queues" \/ slotBlk = 0 then
                slotBlk := nb;
            end if;
            if handed then
                startBlk := nb;
                handed := FALSE;
            end if;
            nb := 0;
        end if;
      Room:
        either skip;
        or
            await ~roomed;
            roomed := TRUE;
            with b \in {x \in 1..NB : ~used[x]} do
                used := [used EXCEPT ![b] = TRUE, ![wblk] = FALSE];
                top := [top EXCEPT ![b] = top[wblk], ![wblk] = 0];
                if wblk = slotBlk /\ Variant /= "room_free_before_commit" then
                    slotBlk := b;
                    startBlk := IF startBlk = wblk THEN b ELSE startBlk;
                end if;
                wblk := b;
            end with;
        end either;
      Zero:
        if Variant /= "place_no_zero" then
            top[wblk] := Len(wbuf);
        end if;
      Write:
        die();
      Write2:
        if top[wblk] /= Len(wbuf) then badPlace := TRUE; end if;
        if obj[self] /= sz then sizeMismatch := TRUE; end if;
        top[wblk] := Len(wbuf) + 1;
        wbuf := Append(wbuf, <<self, i>>);
        if Variant = "counters_early" /\ wblk = slotBlk then
            slotBuf := wbuf;
        end if;
        i := i + 1;
    end while;
  Commit:
    committing := self;
    gil := IF gil = self THEN NONE ELSE gil;
  Queue:
    while pend /= <<>> do
        die();
      Queue2:
        await Len(queue) < Depth \/ qfailed;
        if qfailed then
            if ~pushed \/ Variant = "refused_aborts" then
                goto Abort;
            else
                out := [k \in 1..Len(pend) |-> pend[k].b] \o <<wblk>>;
                pend := <<>>;
                wblk := 0;
                slotBlk := 0;
                slotBuf := <<>>;
                goto Refused;
            end if;
        else
            queue := Append(queue, Head(pend));
            pushed := TRUE;
            if Head(pend).b = firstBlk then
                kept := <<>>;
                firstBlk := 0;
            end if;
            pend := Tail(pend);
        end if;
    end while;
  Install:
    slotBlk := wblk;
    wblk := 0;
    slotBuf := wbuf;
    done := Append(done, self);
    committing := NONE;
    lock := NONE;
    goto Done;
  Refused:
    refused := self;
  Free:
    while out /= <<>> do
        used[Head(out)] := FALSE;
        top[Head(out)] := 0;
        out := Tail(out);
    end while;
  Unlock:
    committing := NONE;
    lock := NONE;
    goto Done;
  Abort:
    out := SelectSeq(
        [k \in 1..Len(pend) |-> pend[k].b] \o (IF sealed THEN <<wblk>> ELSE <<>>),
        LAMBDA x: x /= startBlk);
    pend := <<>>;
    wblk := 0;
    slotBlk := startBlk;
  Drop:
    while out /= <<>> do
        if Variant = "abort_gives_spare" then
            pt := rtail;
          Push:
            rslot[pt + 1] := Head(out);
            rtail := pt + 1;
            out := Tail(out);
        else
            used[Head(out)] := FALSE;
            top[Head(out)] := 0;
            out := Tail(out);
        end if;
    end while;
  Restore:
    slotBuf := start;
    kept := <<>>;
    firstBlk := 0;
    if Variant = "grow_not_kept" then
        slotCap := startCap;
    end if;
    committing := NONE;
    lock := NONE;
    gil := IF gil = self THEN NONE ELSE gil;
end process;

fair process Mut = MUT
begin
  M1:
    if Py then
        await gil = NONE;
        gil := MUT;
      M2:
        with w \in W do obj[w] := obj[w] + 1; end with;
      M3:
        gil := NONE;
    end if;
end process;

fair process Closer = "closer"
begin
  C1:
    await \A w \in W : pc[w] = "Done";
    if ~failed /\ ~qfailed /\ slotBuf /= <<>> then
        await Len(queue) < Depth;
        queue := Append(queue, [b |-> slotBlk, e |-> slotBuf]);
        slotBuf := <<>>;
        slotBlk := 0;
    end if;
end process;

fair process Cust = "cust"
variables cb = 0, ct = 0;
begin
  Pop:
    while TRUE do
        either
            await queue /= <<>>;
            cb := Head(queue).b;
            if ~qfailed then
                file := file \o Head(queue).e;
            else
                lost := lost \o Head(queue).e;
            end if;
            queue := Tail(queue);
        or
            await CustFail /\ ~qfailed;
            qfailed := TRUE;
            goto Pop;
        or
            await pc["closer"] = "Done" /\ queue = <<>>;
            goto Done;
        end either;
      Ring:
        ct := rtail;
      Give:
        rslot[ct + 1] := cb;
        rtail := ct + 1;
        cb := 0;
    end while;
end process;

end algorithm; *)

\* BEGIN TRANSLATION
VARIABLES lock, failed, qfailed, slotBlk, slotBuf, kept, slotCap, slotLayout, 
          used, top, queue, file, lost, refused, rslot, rhead, rtail, done, 
          committing, gil, obj, badPlace, sizeMismatch, pc

(* define statement *)
View == file \o lost \o Elems(queue) \o kept \o slotBuf
InCommit == IF committing = NONE THEN <<>> ELSE Batch(committing)
Refd == IF refused = NONE THEN <<>> ELSE Batch(refused)
BatchWholeOrAbsent ==
    IsPrefix(Cat(done), View) /\ IsPrefix(View, Cat(done) \o InCommit \o Refd)
PlacedOnClean == ~badPlace
SizedIsWritten == ~sizeMismatch
SlotNamesLiveBlock == (lock = NONE /\ slotBlk /= 0) => used[slotBlk]
LayoutAgrees == slotCap = slotLayout
RingFits == rtail <= K

VARIABLES wbuf, wblk, pend, sealed, i, start, startBlk, startCap, sz, grown, 
          roomed, nb, out, pt, pushed, firstBlk, handed, cb, ct

vars == << lock, failed, qfailed, slotBlk, slotBuf, kept, slotCap, slotLayout, 
           used, top, queue, file, lost, refused, rslot, rhead, rtail, done, 
           committing, gil, obj, badPlace, sizeMismatch, pc, wbuf, wblk, pend, 
           sealed, i, start, startBlk, startCap, sz, grown, roomed, nb, out, 
           pt, pushed, firstBlk, handed, cb, ct >>

ProcSet == (W) \cup {MUT} \cup {"closer"} \cup {"cust"}

Init == (* Global variables *)
        /\ lock = NONE
        /\ failed = FALSE
        /\ qfailed = FALSE
        /\ slotBlk = 1
        /\ slotBuf = <<>>
        /\ kept = <<>>
        /\ slotCap = 1
        /\ slotLayout = 1
        /\ used = [b \in 1..NB |-> b = 1]
        /\ top = [b \in 1..NB |-> 0]
        /\ queue = <<>>
        /\ file = <<>>
        /\ lost = <<>>
        /\ refused = NONE
        /\ rslot = [k \in 1..K |-> 0]
        /\ rhead = 0
        /\ rtail = 0
        /\ done = <<>>
        /\ committing = NONE
        /\ gil = NONE
        /\ obj = [w \in W |-> 0]
        /\ badPlace = FALSE
        /\ sizeMismatch = FALSE
        (* Process Wr *)
        /\ wbuf = [self \in W |-> <<>>]
        /\ wblk = [self \in W |-> 0]
        /\ pend = [self \in W |-> <<>>]
        /\ sealed = [self \in W |-> FALSE]
        /\ i = [self \in W |-> 1]
        /\ start = [self \in W |-> <<>>]
        /\ startBlk = [self \in W |-> 0]
        /\ startCap = [self \in W |-> 1]
        /\ sz = [self \in W |-> 0]
        /\ grown = [self \in W |-> FALSE]
        /\ roomed = [self \in W |-> FALSE]
        /\ nb = [self \in W |-> 0]
        /\ out = [self \in W |-> <<>>]
        /\ pt = [self \in W |-> 0]
        /\ pushed = [self \in W |-> FALSE]
        /\ firstBlk = [self \in W |-> 0]
        /\ handed = [self \in W |-> FALSE]
        (* Process Cust *)
        /\ cb = 0
        /\ ct = 0
        /\ pc = [self \in ProcSet |-> CASE self \in W -> "GilFirst"
                                        [] self = MUT -> "M1"
                                        [] self = "closer" -> "C1"
                                        [] self = "cust" -> "Pop"]

GilFirst(self) == /\ pc[self] = "GilFirst"
                  /\ IF Py /\ Variant = "gil_held_blocking" /\ self = "w2"
                        THEN /\ gil = NONE
                             /\ gil' = self
                        ELSE /\ TRUE
                             /\ gil' = gil
                  /\ pc' = [pc EXCEPT ![self] = "Lock"]
                  /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, 
                                  kept, slotCap, slotLayout, used, top, queue, 
                                  file, lost, refused, rslot, rhead, rtail, 
                                  done, committing, obj, badPlace, 
                                  sizeMismatch, wbuf, wblk, pend, sealed, i, 
                                  start, startBlk, startCap, sz, grown, roomed, 
                                  nb, out, pt, pushed, firstBlk, handed, cb, 
                                  ct >>

Lock(self) == /\ pc[self] = "Lock"
              /\ lock = NONE
              /\ IF failed \/ qfailed
                    THEN /\ gil' = (IF gil = self THEN NONE ELSE gil)
                         /\ pc' = [pc EXCEPT ![self] = "Done"]
                         /\ UNCHANGED << lock, wbuf, wblk, start, startBlk, 
                                         startCap >>
                    ELSE /\ lock' = self
                         /\ start' = [start EXCEPT ![self] = slotBuf]
                         /\ startBlk' = [startBlk EXCEPT ![self] = slotBlk]
                         /\ startCap' = [startCap EXCEPT ![self] = slotCap]
                         /\ wbuf' = [wbuf EXCEPT ![self] = slotBuf]
                         /\ wblk' = [wblk EXCEPT ![self] = slotBlk]
                         /\ pc' = [pc EXCEPT ![self] = "Gil"]
                         /\ gil' = gil
              /\ UNCHANGED << failed, qfailed, slotBlk, slotBuf, kept, slotCap, 
                              slotLayout, used, top, queue, file, lost, 
                              refused, rslot, rhead, rtail, done, committing, 
                              obj, badPlace, sizeMismatch, pend, sealed, i, sz, 
                              grown, roomed, nb, out, pt, pushed, firstBlk, 
                              handed, cb, ct >>

Gil(self) == /\ pc[self] = "Gil"
             /\ IF Py /\ gil /= self
                   THEN /\ gil = NONE
                        /\ gil' = self
                   ELSE /\ TRUE
                        /\ gil' = gil
             /\ pc' = [pc EXCEPT ![self] = "Place"]
             /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                             slotCap, slotLayout, used, top, queue, file, lost, 
                             refused, rslot, rhead, rtail, done, committing, 
                             obj, badPlace, sizeMismatch, wbuf, wblk, pend, 
                             sealed, i, start, startBlk, startCap, sz, grown, 
                             roomed, nb, out, pt, pushed, firstBlk, handed, cb, 
                             ct >>

Place(self) == /\ pc[self] = "Place"
               /\ IF i[self] <= BL[self]
                     THEN /\ pc' = [pc EXCEPT ![self] = "Choose"]
                     ELSE /\ pc' = [pc EXCEPT ![self] = "Commit"]
               /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                               slotCap, slotLayout, used, top, queue, file, 
                               lost, refused, rslot, rhead, rtail, done, 
                               committing, gil, obj, badPlace, sizeMismatch, 
                               wbuf, wblk, pend, sealed, i, start, startBlk, 
                               startCap, sz, grown, roomed, nb, out, pt, 
                               pushed, firstBlk, handed, cb, ct >>

Choose(self) == /\ pc[self] = "Choose"
                /\ IF Death
                      THEN /\ \/ /\ TRUE
                                 /\ pc' = [pc EXCEPT ![self] = "Choose2"]
                                 /\ UNCHANGED <<lock, failed>>
                              \/ /\ failed' = TRUE
                                 /\ lock' = NONE
                                 /\ pc' = [pc EXCEPT ![self] = "Done"]
                      ELSE /\ pc' = [pc EXCEPT ![self] = "Choose2"]
                           /\ UNCHANGED << lock, failed >>
                /\ UNCHANGED << qfailed, slotBlk, slotBuf, kept, slotCap, 
                                slotLayout, used, top, queue, file, lost, 
                                refused, rslot, rhead, rtail, done, committing, 
                                gil, obj, badPlace, sizeMismatch, wbuf, wblk, 
                                pend, sealed, i, start, startBlk, startCap, sz, 
                                grown, roomed, nb, out, pt, pushed, firstBlk, 
                                handed, cb, ct >>

Choose2(self) == /\ pc[self] = "Choose2"
                 /\ \/ /\ pc' = [pc EXCEPT ![self] = "Abort"]
                       /\ UNCHANGED <<slotCap, slotLayout, grown>>
                    \/ /\ IF ~grown[self]
                             THEN /\ \/ /\ TRUE
                                        /\ UNCHANGED <<slotCap, slotLayout, grown>>
                                     \/ /\ grown' = [grown EXCEPT ![self] = TRUE]
                                        /\ slotCap' = 2
                                        /\ slotLayout' = 2
                             ELSE /\ TRUE
                                  /\ UNCHANGED << slotCap, slotLayout, grown >>
                       /\ pc' = [pc EXCEPT ![self] = "Size"]
                 /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                                 used, top, queue, file, lost, refused, rslot, 
                                 rhead, rtail, done, committing, gil, obj, 
                                 badPlace, sizeMismatch, wbuf, wblk, pend, 
                                 sealed, i, start, startBlk, startCap, sz, 
                                 roomed, nb, out, pt, pushed, firstBlk, handed, 
                                 cb, ct >>

Size(self) == /\ pc[self] = "Size"
              /\ sz' = [sz EXCEPT ![self] = obj[self]]
              /\ IF Py /\ Variant = "gil_released_between_size_and_write"
                    THEN /\ gil' = NONE
                    ELSE /\ TRUE
                         /\ gil' = gil
              /\ pc' = [pc EXCEPT ![self] = "Regain"]
              /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                              slotCap, slotLayout, used, top, queue, file, 
                              lost, refused, rslot, rhead, rtail, done, 
                              committing, obj, badPlace, sizeMismatch, wbuf, 
                              wblk, pend, sealed, i, start, startBlk, startCap, 
                              grown, roomed, nb, out, pt, pushed, firstBlk, 
                              handed, cb, ct >>

Regain(self) == /\ pc[self] = "Regain"
                /\ IF Py /\ Variant = "gil_released_between_size_and_write"
                      THEN /\ gil = NONE
                           /\ gil' = self
                      ELSE /\ TRUE
                           /\ gil' = gil
                /\ pc' = [pc EXCEPT ![self] = "Seal"]
                /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                                slotCap, slotLayout, used, top, queue, file, 
                                lost, refused, rslot, rhead, rtail, done, 
                                committing, obj, badPlace, sizeMismatch, wbuf, 
                                wblk, pend, sealed, i, start, startBlk, 
                                startCap, sz, grown, roomed, nb, out, pt, 
                                pushed, firstBlk, handed, cb, ct >>

Seal(self) == /\ pc[self] = "Seal"
              /\ IF Death
                    THEN /\ \/ /\ TRUE
                               /\ pc' = [pc EXCEPT ![self] = "Seal2"]
                               /\ UNCHANGED <<lock, failed>>
                            \/ /\ failed' = TRUE
                               /\ lock' = NONE
                               /\ pc' = [pc EXCEPT ![self] = "Done"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Seal2"]
                         /\ UNCHANGED << lock, failed >>
              /\ UNCHANGED << qfailed, slotBlk, slotBuf, kept, slotCap, 
                              slotLayout, used, top, queue, file, lost, 
                              refused, rslot, rhead, rtail, done, committing, 
                              gil, obj, badPlace, sizeMismatch, wbuf, wblk, 
                              pend, sealed, i, start, startBlk, startCap, sz, 
                              grown, roomed, nb, out, pt, pushed, firstBlk, 
                              handed, cb, ct >>

Seal2(self) == /\ pc[self] = "Seal2"
               /\ IF Len(wbuf[self]) = Cap
                     THEN /\ IF Variant = "seal_queues"
                                THEN /\ Len(queue) < Depth
                                     /\ queue' = Append(queue, [b |-> wblk[self], e |-> wbuf[self]])
                                     /\ IF wblk[self] = slotBlk
                                           THEN /\ slotBuf' = <<>>
                                                /\ slotBlk' = 0
                                           ELSE /\ TRUE
                                                /\ UNCHANGED << slotBlk, 
                                                                slotBuf >>
                                     /\ sealed' = [sealed EXCEPT ![self] = TRUE]
                                     /\ pc' = [pc EXCEPT ![self] = "Alloc"]
                                     /\ UNCHANGED << kept, pend, start, 
                                                     firstBlk, handed >>
                                ELSE /\ IF ~sealed[self] /\ (wbuf[self] = start[self] \/ Variant = "hand_on_ours")
                                           THEN /\ Len(queue) < Depth \/ qfailed
                                                /\ IF qfailed
                                                      THEN /\ pc' = [pc EXCEPT ![self] = "Abort"]
                                                           /\ UNCHANGED << slotBlk, 
                                                                           slotBuf, 
                                                                           queue, 
                                                                           start, 
                                                                           handed >>
                                                      ELSE /\ queue' = Append(queue, [b |-> wblk[self], e |-> wbuf[self]])
                                                           /\ slotBuf' = <<>>
                                                           /\ slotBlk' = 0
                                                           /\ start' = [start EXCEPT ![self] = <<>>]
                                                           /\ handed' = [handed EXCEPT ![self] = TRUE]
                                                           /\ pc' = [pc EXCEPT ![self] = "Alloc"]
                                                /\ UNCHANGED << kept, pend, 
                                                                sealed, 
                                                                firstBlk >>
                                           ELSE /\ Len(queue) < Depth \/ qfailed
                                                /\ IF qfailed
                                                      THEN /\ pc' = [pc EXCEPT ![self] = "Abort"]
                                                           /\ UNCHANGED << slotBuf, 
                                                                           kept, 
                                                                           pend, 
                                                                           sealed, 
                                                                           firstBlk >>
                                                      ELSE /\ pend' = [pend EXCEPT ![self] = Append(pend[self], [b |-> wblk[self], e |-> wbuf[self]])]
                                                           /\ IF ~sealed[self]
                                                                 THEN /\ kept' = slotBuf
                                                                      /\ slotBuf' = <<>>
                                                                      /\ firstBlk' = [firstBlk EXCEPT ![self] = wblk[self]]
                                                                 ELSE /\ TRUE
                                                                      /\ UNCHANGED << slotBuf, 
                                                                                      kept, 
                                                                                      firstBlk >>
                                                           /\ sealed' = [sealed EXCEPT ![self] = TRUE]
                                                           /\ pc' = [pc EXCEPT ![self] = "Alloc"]
                                                /\ UNCHANGED << slotBlk, queue, 
                                                                start, handed >>
                          /\ nb' = nb
                     ELSE /\ nb' = [nb EXCEPT ![self] = 0]
                          /\ pc' = [pc EXCEPT ![self] = "Alloc"]
                          /\ UNCHANGED << slotBlk, slotBuf, kept, queue, pend, 
                                          sealed, start, firstBlk, handed >>
               /\ UNCHANGED << lock, failed, qfailed, slotCap, slotLayout, 
                               used, top, file, lost, refused, rslot, rhead, 
                               rtail, done, committing, gil, obj, badPlace, 
                               sizeMismatch, wbuf, wblk, i, startBlk, startCap, 
                               sz, grown, roomed, out, pt, pushed, cb, ct >>

Alloc(self) == /\ pc[self] = "Alloc"
               /\ IF Len(wbuf[self]) = Cap
                     THEN /\ wbuf' = [wbuf EXCEPT ![self] = <<>>]
                          /\ IF rhead < rtail
                                THEN /\ nb' = [nb EXCEPT ![self] = rslot[rhead + 1]]
                                     /\ rhead' = rhead + 1
                                     /\ used' = used
                                ELSE /\ \E b \in {x \in 1..NB : ~used[x]}:
                                          /\ nb' = [nb EXCEPT ![self] = b]
                                          /\ used' = [used EXCEPT ![b] = TRUE]
                                     /\ rhead' = rhead
                     ELSE /\ TRUE
                          /\ UNCHANGED << used, rhead, wbuf, nb >>
               /\ pc' = [pc EXCEPT ![self] = "NewBlock"]
               /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                               slotCap, slotLayout, top, queue, file, lost, 
                               refused, rslot, rtail, done, committing, gil, 
                               obj, badPlace, sizeMismatch, wblk, pend, sealed, 
                               i, start, startBlk, startCap, sz, grown, roomed, 
                               out, pt, pushed, firstBlk, handed, cb, ct >>

NewBlock(self) == /\ pc[self] = "NewBlock"
                  /\ IF nb[self] /= 0
                        THEN /\ wblk' = [wblk EXCEPT ![self] = nb[self]]
                             /\ IF Variant /= "seal_queues" \/ slotBlk = 0
                                   THEN /\ slotBlk' = nb[self]
                                   ELSE /\ TRUE
                                        /\ UNCHANGED slotBlk
                             /\ IF handed[self]
                                   THEN /\ startBlk' = [startBlk EXCEPT ![self] = nb[self]]
                                        /\ handed' = [handed EXCEPT ![self] = FALSE]
                                   ELSE /\ TRUE
                                        /\ UNCHANGED << startBlk, handed >>
                             /\ nb' = [nb EXCEPT ![self] = 0]
                        ELSE /\ TRUE
                             /\ UNCHANGED << slotBlk, wblk, startBlk, nb, 
                                             handed >>
                  /\ pc' = [pc EXCEPT ![self] = "Room"]
                  /\ UNCHANGED << lock, failed, qfailed, slotBuf, kept, 
                                  slotCap, slotLayout, used, top, queue, file, 
                                  lost, refused, rslot, rhead, rtail, done, 
                                  committing, gil, obj, badPlace, sizeMismatch, 
                                  wbuf, pend, sealed, i, start, startCap, sz, 
                                  grown, roomed, out, pt, pushed, firstBlk, cb, 
                                  ct >>

Room(self) == /\ pc[self] = "Room"
              /\ \/ /\ TRUE
                    /\ UNCHANGED <<slotBlk, used, top, wblk, startBlk, roomed>>
                 \/ /\ ~roomed[self]
                    /\ roomed' = [roomed EXCEPT ![self] = TRUE]
                    /\ \E b \in {x \in 1..NB : ~used[x]}:
                         /\ used' = [used EXCEPT ![b] = TRUE, ![wblk[self]] = FALSE]
                         /\ top' = [top EXCEPT ![b] = top[wblk[self]], ![wblk[self]] = 0]
                         /\ IF wblk[self] = slotBlk /\ Variant /= "room_free_before_commit"
                               THEN /\ slotBlk' = b
                                    /\ startBlk' = [startBlk EXCEPT ![self] = IF startBlk[self] = wblk[self] THEN b ELSE startBlk[self]]
                               ELSE /\ TRUE
                                    /\ UNCHANGED << slotBlk, startBlk >>
                         /\ wblk' = [wblk EXCEPT ![self] = b]
              /\ pc' = [pc EXCEPT ![self] = "Zero"]
              /\ UNCHANGED << lock, failed, qfailed, slotBuf, kept, slotCap, 
                              slotLayout, queue, file, lost, refused, rslot, 
                              rhead, rtail, done, committing, gil, obj, 
                              badPlace, sizeMismatch, wbuf, pend, sealed, i, 
                              start, startCap, sz, grown, nb, out, pt, pushed, 
                              firstBlk, handed, cb, ct >>

Zero(self) == /\ pc[self] = "Zero"
              /\ IF Variant /= "place_no_zero"
                    THEN /\ top' = [top EXCEPT ![wblk[self]] = Len(wbuf[self])]
                    ELSE /\ TRUE
                         /\ top' = top
              /\ pc' = [pc EXCEPT ![self] = "Write"]
              /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                              slotCap, slotLayout, used, queue, file, lost, 
                              refused, rslot, rhead, rtail, done, committing, 
                              gil, obj, badPlace, sizeMismatch, wbuf, wblk, 
                              pend, sealed, i, start, startBlk, startCap, sz, 
                              grown, roomed, nb, out, pt, pushed, firstBlk, 
                              handed, cb, ct >>

Write(self) == /\ pc[self] = "Write"
               /\ IF Death
                     THEN /\ \/ /\ TRUE
                                /\ pc' = [pc EXCEPT ![self] = "Write2"]
                                /\ UNCHANGED <<lock, failed>>
                             \/ /\ failed' = TRUE
                                /\ lock' = NONE
                                /\ pc' = [pc EXCEPT ![self] = "Done"]
                     ELSE /\ pc' = [pc EXCEPT ![self] = "Write2"]
                          /\ UNCHANGED << lock, failed >>
               /\ UNCHANGED << qfailed, slotBlk, slotBuf, kept, slotCap, 
                               slotLayout, used, top, queue, file, lost, 
                               refused, rslot, rhead, rtail, done, committing, 
                               gil, obj, badPlace, sizeMismatch, wbuf, wblk, 
                               pend, sealed, i, start, startBlk, startCap, sz, 
                               grown, roomed, nb, out, pt, pushed, firstBlk, 
                               handed, cb, ct >>

Write2(self) == /\ pc[self] = "Write2"
                /\ IF top[wblk[self]] /= Len(wbuf[self])
                      THEN /\ badPlace' = TRUE
                      ELSE /\ TRUE
                           /\ UNCHANGED badPlace
                /\ IF obj[self] /= sz[self]
                      THEN /\ sizeMismatch' = TRUE
                      ELSE /\ TRUE
                           /\ UNCHANGED sizeMismatch
                /\ top' = [top EXCEPT ![wblk[self]] = Len(wbuf[self]) + 1]
                /\ wbuf' = [wbuf EXCEPT ![self] = Append(wbuf[self], <<self, i[self]>>)]
                /\ IF Variant = "counters_early" /\ wblk[self] = slotBlk
                      THEN /\ slotBuf' = wbuf'[self]
                      ELSE /\ TRUE
                           /\ UNCHANGED slotBuf
                /\ i' = [i EXCEPT ![self] = i[self] + 1]
                /\ pc' = [pc EXCEPT ![self] = "Place"]
                /\ UNCHANGED << lock, failed, qfailed, slotBlk, kept, slotCap, 
                                slotLayout, used, queue, file, lost, refused, 
                                rslot, rhead, rtail, done, committing, gil, 
                                obj, wblk, pend, sealed, start, startBlk, 
                                startCap, sz, grown, roomed, nb, out, pt, 
                                pushed, firstBlk, handed, cb, ct >>

Commit(self) == /\ pc[self] = "Commit"
                /\ committing' = self
                /\ gil' = (IF gil = self THEN NONE ELSE gil)
                /\ pc' = [pc EXCEPT ![self] = "Queue"]
                /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                                slotCap, slotLayout, used, top, queue, file, 
                                lost, refused, rslot, rhead, rtail, done, obj, 
                                badPlace, sizeMismatch, wbuf, wblk, pend, 
                                sealed, i, start, startBlk, startCap, sz, 
                                grown, roomed, nb, out, pt, pushed, firstBlk, 
                                handed, cb, ct >>

Queue(self) == /\ pc[self] = "Queue"
               /\ IF pend[self] /= <<>>
                     THEN /\ IF Death
                                THEN /\ \/ /\ TRUE
                                           /\ pc' = [pc EXCEPT ![self] = "Queue2"]
                                           /\ UNCHANGED <<lock, failed>>
                                        \/ /\ failed' = TRUE
                                           /\ lock' = NONE
                                           /\ pc' = [pc EXCEPT ![self] = "Done"]
                                ELSE /\ pc' = [pc EXCEPT ![self] = "Queue2"]
                                     /\ UNCHANGED << lock, failed >>
                     ELSE /\ pc' = [pc EXCEPT ![self] = "Install"]
                          /\ UNCHANGED << lock, failed >>
               /\ UNCHANGED << qfailed, slotBlk, slotBuf, kept, slotCap, 
                               slotLayout, used, top, queue, file, lost, 
                               refused, rslot, rhead, rtail, done, committing, 
                               gil, obj, badPlace, sizeMismatch, wbuf, wblk, 
                               pend, sealed, i, start, startBlk, startCap, sz, 
                               grown, roomed, nb, out, pt, pushed, firstBlk, 
                               handed, cb, ct >>

Queue2(self) == /\ pc[self] = "Queue2"
                /\ Len(queue) < Depth \/ qfailed
                /\ IF qfailed
                      THEN /\ IF ~pushed[self] \/ Variant = "refused_aborts"
                                 THEN /\ pc' = [pc EXCEPT ![self] = "Abort"]
                                      /\ UNCHANGED << slotBlk, slotBuf, wblk, 
                                                      pend, out >>
                                 ELSE /\ out' = [out EXCEPT ![self] = [k \in 1..Len(pend[self]) |-> pend[self][k].b] \o <<wblk[self]>>]
                                      /\ pend' = [pend EXCEPT ![self] = <<>>]
                                      /\ wblk' = [wblk EXCEPT ![self] = 0]
                                      /\ slotBlk' = 0
                                      /\ slotBuf' = <<>>
                                      /\ pc' = [pc EXCEPT ![self] = "Refused"]
                           /\ UNCHANGED << kept, queue, pushed, firstBlk >>
                      ELSE /\ queue' = Append(queue, Head(pend[self]))
                           /\ pushed' = [pushed EXCEPT ![self] = TRUE]
                           /\ IF Head(pend[self]).b = firstBlk[self]
                                 THEN /\ kept' = <<>>
                                      /\ firstBlk' = [firstBlk EXCEPT ![self] = 0]
                                 ELSE /\ TRUE
                                      /\ UNCHANGED << kept, firstBlk >>
                           /\ pend' = [pend EXCEPT ![self] = Tail(pend[self])]
                           /\ pc' = [pc EXCEPT ![self] = "Queue"]
                           /\ UNCHANGED << slotBlk, slotBuf, wblk, out >>
                /\ UNCHANGED << lock, failed, qfailed, slotCap, slotLayout, 
                                used, top, file, lost, refused, rslot, rhead, 
                                rtail, done, committing, gil, obj, badPlace, 
                                sizeMismatch, wbuf, sealed, i, start, startBlk, 
                                startCap, sz, grown, roomed, nb, pt, handed, 
                                cb, ct >>

Install(self) == /\ pc[self] = "Install"
                 /\ slotBlk' = wblk[self]
                 /\ wblk' = [wblk EXCEPT ![self] = 0]
                 /\ slotBuf' = wbuf[self]
                 /\ done' = Append(done, self)
                 /\ committing' = NONE
                 /\ lock' = NONE
                 /\ pc' = [pc EXCEPT ![self] = "Done"]
                 /\ UNCHANGED << failed, qfailed, kept, slotCap, slotLayout, 
                                 used, top, queue, file, lost, refused, rslot, 
                                 rhead, rtail, gil, obj, badPlace, 
                                 sizeMismatch, wbuf, pend, sealed, i, start, 
                                 startBlk, startCap, sz, grown, roomed, nb, 
                                 out, pt, pushed, firstBlk, handed, cb, ct >>

Refused(self) == /\ pc[self] = "Refused"
                 /\ refused' = self
                 /\ pc' = [pc EXCEPT ![self] = "Free"]
                 /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                                 slotCap, slotLayout, used, top, queue, file, 
                                 lost, rslot, rhead, rtail, done, committing, 
                                 gil, obj, badPlace, sizeMismatch, wbuf, wblk, 
                                 pend, sealed, i, start, startBlk, startCap, 
                                 sz, grown, roomed, nb, out, pt, pushed, 
                                 firstBlk, handed, cb, ct >>

Free(self) == /\ pc[self] = "Free"
              /\ IF out[self] /= <<>>
                    THEN /\ used' = [used EXCEPT ![Head(out[self])] = FALSE]
                         /\ top' = [top EXCEPT ![Head(out[self])] = 0]
                         /\ out' = [out EXCEPT ![self] = Tail(out[self])]
                         /\ pc' = [pc EXCEPT ![self] = "Free"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Unlock"]
                         /\ UNCHANGED << used, top, out >>
              /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                              slotCap, slotLayout, queue, file, lost, refused, 
                              rslot, rhead, rtail, done, committing, gil, obj, 
                              badPlace, sizeMismatch, wbuf, wblk, pend, sealed, 
                              i, start, startBlk, startCap, sz, grown, roomed, 
                              nb, pt, pushed, firstBlk, handed, cb, ct >>

Unlock(self) == /\ pc[self] = "Unlock"
                /\ committing' = NONE
                /\ lock' = NONE
                /\ pc' = [pc EXCEPT ![self] = "Done"]
                /\ UNCHANGED << failed, qfailed, slotBlk, slotBuf, kept, 
                                slotCap, slotLayout, used, top, queue, file, 
                                lost, refused, rslot, rhead, rtail, done, gil, 
                                obj, badPlace, sizeMismatch, wbuf, wblk, pend, 
                                sealed, i, start, startBlk, startCap, sz, 
                                grown, roomed, nb, out, pt, pushed, firstBlk, 
                                handed, cb, ct >>

Abort(self) == /\ pc[self] = "Abort"
               /\ out' = [out EXCEPT ![self] =    SelectSeq(
                                               [k \in 1..Len(pend[self]) |-> pend[self][k].b] \o (IF sealed[self] THEN <<wblk[self]>> ELSE <<>>),
                                               LAMBDA x: x /= startBlk[self])]
               /\ pend' = [pend EXCEPT ![self] = <<>>]
               /\ wblk' = [wblk EXCEPT ![self] = 0]
               /\ slotBlk' = startBlk[self]
               /\ pc' = [pc EXCEPT ![self] = "Drop"]
               /\ UNCHANGED << lock, failed, qfailed, slotBuf, kept, slotCap, 
                               slotLayout, used, top, queue, file, lost, 
                               refused, rslot, rhead, rtail, done, committing, 
                               gil, obj, badPlace, sizeMismatch, wbuf, sealed, 
                               i, start, startBlk, startCap, sz, grown, roomed, 
                               nb, pt, pushed, firstBlk, handed, cb, ct >>

Drop(self) == /\ pc[self] = "Drop"
              /\ IF out[self] /= <<>>
                    THEN /\ IF Variant = "abort_gives_spare"
                               THEN /\ pt' = [pt EXCEPT ![self] = rtail]
                                    /\ pc' = [pc EXCEPT ![self] = "Push"]
                                    /\ UNCHANGED << used, top, out >>
                               ELSE /\ used' = [used EXCEPT ![Head(out[self])] = FALSE]
                                    /\ top' = [top EXCEPT ![Head(out[self])] = 0]
                                    /\ out' = [out EXCEPT ![self] = Tail(out[self])]
                                    /\ pc' = [pc EXCEPT ![self] = "Drop"]
                                    /\ pt' = pt
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Restore"]
                         /\ UNCHANGED << used, top, out, pt >>
              /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                              slotCap, slotLayout, queue, file, lost, refused, 
                              rslot, rhead, rtail, done, committing, gil, obj, 
                              badPlace, sizeMismatch, wbuf, wblk, pend, sealed, 
                              i, start, startBlk, startCap, sz, grown, roomed, 
                              nb, pushed, firstBlk, handed, cb, ct >>

Push(self) == /\ pc[self] = "Push"
              /\ rslot' = [rslot EXCEPT ![pt[self] + 1] = Head(out[self])]
              /\ rtail' = pt[self] + 1
              /\ out' = [out EXCEPT ![self] = Tail(out[self])]
              /\ pc' = [pc EXCEPT ![self] = "Drop"]
              /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, 
                              slotCap, slotLayout, used, top, queue, file, 
                              lost, refused, rhead, done, committing, gil, obj, 
                              badPlace, sizeMismatch, wbuf, wblk, pend, sealed, 
                              i, start, startBlk, startCap, sz, grown, roomed, 
                              nb, pt, pushed, firstBlk, handed, cb, ct >>

Restore(self) == /\ pc[self] = "Restore"
                 /\ slotBuf' = start[self]
                 /\ kept' = <<>>
                 /\ firstBlk' = [firstBlk EXCEPT ![self] = 0]
                 /\ IF Variant = "grow_not_kept"
                       THEN /\ slotCap' = startCap[self]
                       ELSE /\ TRUE
                            /\ UNCHANGED slotCap
                 /\ committing' = NONE
                 /\ lock' = NONE
                 /\ gil' = (IF gil = self THEN NONE ELSE gil)
                 /\ pc' = [pc EXCEPT ![self] = "Done"]
                 /\ UNCHANGED << failed, qfailed, slotBlk, slotLayout, used, 
                                 top, queue, file, lost, refused, rslot, rhead, 
                                 rtail, done, obj, badPlace, sizeMismatch, 
                                 wbuf, wblk, pend, sealed, i, start, startBlk, 
                                 startCap, sz, grown, roomed, nb, out, pt, 
                                 pushed, handed, cb, ct >>

Wr(self) == GilFirst(self) \/ Lock(self) \/ Gil(self) \/ Place(self)
               \/ Choose(self) \/ Choose2(self) \/ Size(self)
               \/ Regain(self) \/ Seal(self) \/ Seal2(self) \/ Alloc(self)
               \/ NewBlock(self) \/ Room(self) \/ Zero(self) \/ Write(self)
               \/ Write2(self) \/ Commit(self) \/ Queue(self)
               \/ Queue2(self) \/ Install(self) \/ Refused(self)
               \/ Free(self) \/ Unlock(self) \/ Abort(self) \/ Drop(self)
               \/ Push(self) \/ Restore(self)

M1 == /\ pc[MUT] = "M1"
      /\ IF Py
            THEN /\ gil = NONE
                 /\ gil' = MUT
                 /\ pc' = [pc EXCEPT ![MUT] = "M2"]
            ELSE /\ pc' = [pc EXCEPT ![MUT] = "Done"]
                 /\ gil' = gil
      /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, slotCap, 
                      slotLayout, used, top, queue, file, lost, refused, rslot, 
                      rhead, rtail, done, committing, obj, badPlace, 
                      sizeMismatch, wbuf, wblk, pend, sealed, i, start, 
                      startBlk, startCap, sz, grown, roomed, nb, out, pt, 
                      pushed, firstBlk, handed, cb, ct >>

M2 == /\ pc[MUT] = "M2"
      /\ \E w \in W:
           obj' = [obj EXCEPT ![w] = obj[w] + 1]
      /\ pc' = [pc EXCEPT ![MUT] = "M3"]
      /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, slotCap, 
                      slotLayout, used, top, queue, file, lost, refused, rslot, 
                      rhead, rtail, done, committing, gil, badPlace, 
                      sizeMismatch, wbuf, wblk, pend, sealed, i, start, 
                      startBlk, startCap, sz, grown, roomed, nb, out, pt, 
                      pushed, firstBlk, handed, cb, ct >>

M3 == /\ pc[MUT] = "M3"
      /\ gil' = NONE
      /\ pc' = [pc EXCEPT ![MUT] = "Done"]
      /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, slotCap, 
                      slotLayout, used, top, queue, file, lost, refused, rslot, 
                      rhead, rtail, done, committing, obj, badPlace, 
                      sizeMismatch, wbuf, wblk, pend, sealed, i, start, 
                      startBlk, startCap, sz, grown, roomed, nb, out, pt, 
                      pushed, firstBlk, handed, cb, ct >>

Mut == M1 \/ M2 \/ M3

C1 == /\ pc["closer"] = "C1"
      /\ \A w \in W : pc[w] = "Done"
      /\ IF ~failed /\ ~qfailed /\ slotBuf /= <<>>
            THEN /\ Len(queue) < Depth
                 /\ queue' = Append(queue, [b |-> slotBlk, e |-> slotBuf])
                 /\ slotBuf' = <<>>
                 /\ slotBlk' = 0
            ELSE /\ TRUE
                 /\ UNCHANGED << slotBlk, slotBuf, queue >>
      /\ pc' = [pc EXCEPT !["closer"] = "Done"]
      /\ UNCHANGED << lock, failed, qfailed, kept, slotCap, slotLayout, used, 
                      top, file, lost, refused, rslot, rhead, rtail, done, 
                      committing, gil, obj, badPlace, sizeMismatch, wbuf, wblk, 
                      pend, sealed, i, start, startBlk, startCap, sz, grown, 
                      roomed, nb, out, pt, pushed, firstBlk, handed, cb, ct >>

Closer == C1

Pop == /\ pc["cust"] = "Pop"
       /\ \/ /\ queue /= <<>>
             /\ cb' = Head(queue).b
             /\ IF ~qfailed
                   THEN /\ file' = file \o Head(queue).e
                        /\ lost' = lost
                   ELSE /\ lost' = lost \o Head(queue).e
                        /\ file' = file
             /\ queue' = Tail(queue)
             /\ pc' = [pc EXCEPT !["cust"] = "Ring"]
             /\ UNCHANGED qfailed
          \/ /\ CustFail /\ ~qfailed
             /\ qfailed' = TRUE
             /\ pc' = [pc EXCEPT !["cust"] = "Pop"]
             /\ UNCHANGED <<queue, file, lost, cb>>
          \/ /\ pc["closer"] = "Done" /\ queue = <<>>
             /\ pc' = [pc EXCEPT !["cust"] = "Done"]
             /\ UNCHANGED <<qfailed, queue, file, lost, cb>>
       /\ UNCHANGED << lock, failed, slotBlk, slotBuf, kept, slotCap, 
                       slotLayout, used, top, refused, rslot, rhead, rtail, 
                       done, committing, gil, obj, badPlace, sizeMismatch, 
                       wbuf, wblk, pend, sealed, i, start, startBlk, startCap, 
                       sz, grown, roomed, nb, out, pt, pushed, firstBlk, 
                       handed, ct >>

Ring == /\ pc["cust"] = "Ring"
        /\ ct' = rtail
        /\ pc' = [pc EXCEPT !["cust"] = "Give"]
        /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, slotCap, 
                        slotLayout, used, top, queue, file, lost, refused, 
                        rslot, rhead, rtail, done, committing, gil, obj, 
                        badPlace, sizeMismatch, wbuf, wblk, pend, sealed, i, 
                        start, startBlk, startCap, sz, grown, roomed, nb, out, 
                        pt, pushed, firstBlk, handed, cb >>

Give == /\ pc["cust"] = "Give"
        /\ rslot' = [rslot EXCEPT ![ct + 1] = cb]
        /\ rtail' = ct + 1
        /\ cb' = 0
        /\ pc' = [pc EXCEPT !["cust"] = "Pop"]
        /\ UNCHANGED << lock, failed, qfailed, slotBlk, slotBuf, kept, slotCap, 
                        slotLayout, used, top, queue, file, lost, refused, 
                        rhead, done, committing, gil, obj, badPlace, 
                        sizeMismatch, wbuf, wblk, pend, sealed, i, start, 
                        startBlk, startCap, sz, grown, roomed, nb, out, pt, 
                        pushed, firstBlk, handed, ct >>

Cust == Pop \/ Ring \/ Give

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Mut \/ Closer \/ Cust
           \/ (\E self \in W: Wr(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in W : WF_vars(Wr(self))
        /\ WF_vars(Mut)
        /\ WF_vars(Closer)
        /\ WF_vars(Cust)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION

QB == {queue[k].b : k \in 1..Len(queue)}
RB == {rslot[k] : k \in (rhead + 1)..rtail}
WB == UNION {({pend[w][k].b : k \in 1..Len(pend[w])} \cup {wblk[w], nb[w], firstBlk[w]} \cup Range(out[w])) : w \in W}
Held == ({slotBlk, cb} \cup QB \cup RB \cup WB) \ {0}
NoBlockLost == \A b \in 1..NB : used[b] => b \in Held
NoFreedHeld == \A b \in Held : used[b]
ReturnedSurvive ==
    ((\A p \in ProcSet : pc[p] = "Done") /\ ~failed /\ ~qfailed) => file = Cat(done)
SlotExclusive == slotBlk /= 0 => slotBlk \notin (QB \cup RB \cup {cb})
=============================================================================
