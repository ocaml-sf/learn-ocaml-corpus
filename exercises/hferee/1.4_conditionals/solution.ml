let phrase1 = 4

let phrase2 = Erreur

let phrase3 = Erreur


let simplifie1 x = x <= 3

let simplifie2 x y = if x then not y else y

let edt day time =
    if day = "monday" && 13 * 60 + 30 <= time && time < 15 * 60 + 30 then "practical"
    else if day = "thursday" && 8 * 60 + 30 <= time && time < 10 * 60 + 30 then "lecture-tutorial"
    else "Nothing interesting"

let approximately t = "Approximately " ^
    if t >= 24*3600 then string_of_int (t / (24 * 3600)) ^ " days"
    else if t >= 3600 then string_of_int (t / 3600) ^ " hours"
    else if t >= 60 then string_of_int (t / 60) ^ " minutes"
    else if t >= 0 then string_of_int t ^ " seconds"
    else "Negative number"


let approximately_bonus t =
    let (n, s) =
        if t >= 24*3600 then (t / (24 * 3600), "day")
        else if t >= 3600 then (t / 3600,  "hour")
        else if t >= 60 then (t / 60, "minute")
        else (t, "second")
    in
    if n < 0 then "Negative number" else
    let pluriel = if n <> 1 then "s" else "" in
    "Approximately " ^ string_of_int n ^ " " ^ s ^ pluriel
