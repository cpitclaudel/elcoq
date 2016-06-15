Ltac slow := do 1000 (do 1000 idtac).
Ltac wrong := intro.

Lemma a: True.
Proof. slow. slow. slow. slow. wrong. slow. slow. auto. Qed.

Lemma b: True.
Proof. slow. slow. slow. auto. Qed.

Lemma c: True.
Proof. slow. slow. slow. wrong. auto. Qed.

Lemma d: True.
Proof. slow. slow. slow.


