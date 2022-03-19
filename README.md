# Combinators
Theorem prover for combinatory logic developed while reading Ramond Smullyan's To Mock a Mocking Bird. 
By running main one can see some proofs like the following: 

  *Prove:
  L 1 2 = 1 (2 2)
  Assumptions: 
  B 1 2 3 = 1 (2 3)
  T 1 2 = 2 1
  M 1 = 1 1
  Proof: 
  B (T M) B 1 2 = 1 (2 2)*

where we derive the combinator L by using B, T, and M.
