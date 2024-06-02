include Option

let valuef ~default = function
  | Some v -> v
  | None -> default ()
