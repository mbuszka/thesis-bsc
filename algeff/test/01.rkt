#lang algeff

-- handle + (Get 0) ((λ i Get 0) (Set 7)) with
-- | Set s r -> λ old r 0 s
-- | Get x r -> λ s r s s
-- | return x -> λ s x
-- end 3
letrec f x =
  if == x 0
    then 1
    else * x (f (- x 1))
  end in
  f 5