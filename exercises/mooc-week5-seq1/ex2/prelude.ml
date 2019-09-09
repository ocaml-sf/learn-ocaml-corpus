type report = message list
and message = string * status
and status = Successful | Failed

type 'a result = Ok of 'a | Error of exn
