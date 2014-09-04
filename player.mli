
type gender = 
  Male | Female

type t = { name: string; country: string; gender: gender }

type db

val make_db: unit -> db

val pick: string -> db -> t
