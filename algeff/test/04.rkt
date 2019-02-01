#lang algeff

handle
  letrec loop x =
    if >=(42, x) then Throw 42
    else loop +(x, 1) end
  in loop 0
with
| Throw n r -> n
| return x -> x
end