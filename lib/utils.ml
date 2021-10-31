let bin_of_char c =
  match c with
  | '0' -> "0000" | '1' -> "0001" | '2' -> "0010" | '3' -> "0011"
  | '4' -> "0100" | '5' -> "0101" | '6' -> "0110" | '7' -> "0111"
  | '8' -> "1000" | '9' -> "1001" | 'a' -> "1010" | 'b' -> "1011"
  | 'c' -> "1100" | 'd' -> "1101" | 'e' -> "1110" | 'f' -> "1111"
  | _ -> failwith "hash error"

let hex_to_bin s =
  let rec aux l i acc =
    if i = l then acc
    else aux l (succ i) (bin_of_char s.[i]) ^ acc
  in
    aux (String.length s) 0 "";;

let rec repeat s n =
  if n = 0 then "" else s ^ repeat s (n - 1)

(* if i update to ocaml 4.13.^ dont need this *)
let starts_with ~prefix s =
  let len_s = String.length s
  and len_pre = String.length prefix in
  let rec aux i =
    if i = len_pre then true
    else if String.get s i != String.get prefix i then false
    else aux (i + 1)
  in len_s >= len_pre && aux 0

let get_block block n = 
  let rec aux block' n' =
    if n' = 0 then block'
    else aux (Yojson.Safe.Util.member "previous_block" block' |> Yojson.Safe.Util.index 1) (n' - 1)
  in
    aux (Yojson.Safe.Util.index 1 block) n;;