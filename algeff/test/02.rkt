#lang algeff

let handleState = λ x handle x 0 with
  | Get ignore r -> λ s (r s s)
  | Set state r -> λ old (r 0 state)
  | return x -> λ s x
  end in (handleState λ ignore
    let x = Get 0 in
    let y = + x 7 in
    let z = Set y in
    + (Get 0) (Get 0)) 5