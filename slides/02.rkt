#lang algeff

-- let comp = 
λ ignore
  let a = Set 13 in
  +(Get 0, Get (Set 29))
-- in (handle comp 0 with
--| Get ignore r -> λ s (r s s)
--| Set state r -> λ old (r 0 state)
--| return x -> λ s x
--end) 0
