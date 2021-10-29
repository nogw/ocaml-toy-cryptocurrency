let bin c = 
  match c with
  | '0' -> "0000" | '1' -> "0001" | '2' -> "0010" | '3' -> "0011" 
  | '4' -> "0100" | '5' -> "0101" | '6' -> "0110" | '7' -> "0111" 
  | '8' -> "1000" | '9' -> "1001" | 'a' -> "1010" | 'b' -> "1011" 
  | 'c' -> "1100" | 'd' -> "1101" | 'e' -> "1110" | 'f' -> "1111" 
  | _ -> failwith "hash error"

let hex_to_bin s =
  let rec aux l i acc =
    if i = l then acc 
    else aux l (succ i) (bin s.[i]) ^ acc
  in
    aux (String.length s) 0 "";;

let index_of block =
  match block with
  | GenesisBlock b -> b.index
  | Block b -> b.index

let hash_of block =
  match block with
  | GenesisBlock b -> b.hash
  | Block b -> b.hash

let timestamp_of block =
  match block with
  | GenesisBlock b -> b.timestamp
  | Block b -> b.timestamp

let data_of block =
  match block with
  | GenesisBlock b -> b.data
  | Block b -> b.data

let previous_of block =
  match block with
  | GenesisBlock _ -> failwith "Error: tried to obtain previous hash of genesis block"
  | Block b -> b.previous_block