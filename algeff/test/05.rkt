#lang algeff

letrec fromTo x =
  λ y if ==(x, y) then nil()
    else cons(x, fromTo +(x, 1) y)
    end in 
let map = λ f
  letrec loop xs = 
    if nil?(xs) then nil()
    else cons(f hd(xs), aux tl(xs))
    end in 
  loop in
let exists = λ p λ xs
  let f = λ x if p x then Break true else x end in
  handle map f xs with
  | Break x r -> true
  | return x  -> false
  end in
exists (λ x ==(x, 3)) (fromTo 0 10)
