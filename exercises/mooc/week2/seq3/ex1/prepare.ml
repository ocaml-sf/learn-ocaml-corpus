module Array = struct
  include Array
  let counter = ref 0
  let get a i =
    incr counter ;
    Array.get a i
end
