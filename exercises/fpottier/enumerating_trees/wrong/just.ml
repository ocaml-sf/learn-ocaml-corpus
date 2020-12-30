let just x s =
  Seq.singleton x (* wrong: [x] replicated at all sizes *)
