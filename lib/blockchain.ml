open Utils

type timestamp = [%import: Unix.tm] [@@deriving yojson]

type block = 
  | GenesisBlock of {
    index: int;
    hash: string;
    timestamp: timestamp;
    data: string;
  }
  | Block of {
    index: int;
    hash: string;
    timestamp: timestamp;
    data: string;
    previous_block: block 
  }
  [@@deriving yojson]

let block index hash timestamp data previous_block =
  Block {
    index;
    hash;
    timestamp;
    data;
    previous_block;
  }

let hash_block index timestamp ?(previous_hash = "") data =
  [ string_of_int index;
    Unix.mktime timestamp |> fst |> string_of_float;
    data;
    previous_hash
  ] |> String.concat "" |> Sha256.string |> Sha256.to_hex

let initial_block data = 
  let timestamp = Unix.time () |> Unix.gmtime in
  GenesisBlock {
    index = 0;
    hash = hash_block 0 timestamp data;
    timestamp;
    data;
  }  

let add_next_block data previous_block = 
  let timestamp = Unix.time () |> Unix.gmtime
  and index = index_of previous_block + 1 in
  Block {
    index;
    hash = hash_block index timestamp ~previous_hash:(hash_of previous_block) data;
    timestamp;
    data;
    previous_block;
  }

let validate_next_block block =
  match block with
  | GenesisBlock _ -> true
  | Block b -> 
    let previous_hash = hash_of (previous_of block) in
    let hash = hash_block b.index b.timestamp ~previous_hash b.data in
    let previous_index = 
      match b.previous_block with 
      | GenesisBlock pb -> pb.index
      | Block pb -> pb.index
    in
      b.index = previous_index + 1 && hash = b.hash 

let hash_matches_difficulty hash difficulty =
  let hash_binary = hex_to_bin hash in

let validate_chain chain = 
  let rec aux chain' res = 
    match (chain', res) with
    | _, false -> false
    | GenesisBlock _, _ -> true
    | Block c, _  -> aux c.previous_block (validate_next_block chain')
  in
    aux chain true

let replace new_chain chain =
  if index_of new_chain > index_of chain && validate_chain new_chain then 
    new_chain
  else chain