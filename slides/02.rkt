#lang algeff

(handle +(Get 0, Get (Set 29)) with
| Get i r -> λ s (r s s)
| Set s r -> λ i (r 0 s)
| return x -> λ s x
end) 13
