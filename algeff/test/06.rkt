#lang algeff

handle +(Tick 0,
  handle +(Tick 0, lift Tick (Tick 1)) with
  | Tick x r -> if ==(x, 0) then r 0 else 11 end
  | return x -> x
  end) with
| Tick x r -> if ==(x, 0) then r 2 else r 40 end
| return x -> x
end
  
