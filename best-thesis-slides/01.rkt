#lang algeff

handle +(Magic -(2,1), 1) with
| Magic x r -> r 42
| return x -> -(x, 1)
end