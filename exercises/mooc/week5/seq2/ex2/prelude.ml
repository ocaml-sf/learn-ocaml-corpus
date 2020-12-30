type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list
