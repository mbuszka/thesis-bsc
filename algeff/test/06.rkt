#lang algeff

handle +(Tick 0,
  handle +(Tick 0, lift Tock (Tock 1)) with
  | Tick x r -> r 0
  | Tock x r -> 12
  | return x -> x
  end) with
| Tick x r -> r 2
| Tock x r -> r 40
| return x -> x
end
  
