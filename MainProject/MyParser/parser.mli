type fname = string

(** Parsuje podany plik jako listę klauzul *)
val parse_file : fname -> Ast.program

(** Parsuje pojedyncze zapytanie prologowe, wczytując je z podanego
  * strumienia *)
val parse_query : in_channel -> Ast.program

(** Parsuje napis jako pojedyncze zapytanie prologowe. *)
val parse_query_string : string -> Ast.program
