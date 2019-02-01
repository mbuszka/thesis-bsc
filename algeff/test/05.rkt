#lang algeff

letrec fromTo x =
  λ y if ==(x, y) then nil() else cons(x, fromTo +(x, 1) y) end
in let map = λ f
  letrec aux xs = if nil?(xs) then nil() else cons(f hd(xs), aux tl(xs)) end
  in aux
in let exists = λ p λ xs
    handle map (λ x if (lift Break (p x)) then Break true else x end) xs with
    | Break x r -> true
    | return x  -> false
    end
in exists (λ x ==(x, 3)) (fromTo 0 10)
