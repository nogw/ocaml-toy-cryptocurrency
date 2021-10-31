type timestamp = [%import: Unix.tm] [@@deriving yojson]

type block = 
  | GenesisBlock of {
    index: int;
    hash: string;
    timestamp: timestamp;
    data: string;
    difficulty: int;
    nonce: int;
  }
  | Block of {
    index: int;
    hash: string;
    timestamp: timestamp;
    data: string;
    difficulty: int;
    nonce: int;
    previous_block: block 
  }
  [@@deriving yojson]

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

let difficulty_of block =
  match block with
  | GenesisBlock b -> b.difficulty
  | Block b -> b.difficulty

let nonce_of block =
  match block with
  | GenesisBlock b -> b.nonce
  | Block b -> b.nonce

let previous_of block =
  match block with
  | GenesisBlock _ -> failwith "Error: tried to obtain previous hash of genesis block"
  | Block b -> b.previous_block

let block index hash timestamp data difficulty nonce previous_block =
  Block {
    index;
    hash;
    timestamp;
    data;
    difficulty;
    nonce;
    previous_block;
  }

let hash_block index timestamp ?(previous_hash = "") data difficulty nonce =
  [ string_of_int index;
    Unix.mktime timestamp |> fst |> string_of_float;
    data;
    string_of_int difficulty;
    string_of_int nonce;
    previous_hash
  ] |> String.concat "" |> Sha256.string |> Sha256.to_hex

let initial_block data = 
  let timestamp = Unix.time () |> Unix.gmtime in
  GenesisBlock {
    index = 0;
    hash = hash_block 0 timestamp data 0 0 ;
    timestamp;
    data;
    difficulty = 0;
    nonce = 0
  }  

let block_generation_interval = 10
let difficulty_adjustment_interval = 10

let get_adjusted_difficulty block previous_block = 
  let prev_adjustment_block = Utils.get_block block (index_of block - difficulty_adjustment_interval) in
  let time_expected = block_generation_interval * difficulty_adjustment_interval in
  let time_taken = timestamp_of previous_block - timestamp_of prev_adjustment_block in
  if time_taken < time_expected / 2 then difficulty_of prev_adjustment_block + 1
  else if time_taken > time_expected * 2 then difficulty_of prev_adjustment_block - 1
  else difficulty_of prev_adjustment_block

let get_difficulty block =
  let previous_block = previous_of block in
  if (index_of previous_block mod difficulty_adjustment_interval = 0) && index_of previous_block != 0 then
    get_adjusted_difficulty previous_block block
  else difficulty_of previous_block

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
      b.index = succ previous_index && hash = b.hash 

let hash_matches_difficulty hash difficulty =
  let hash_binary = Utils.hex_to_bin hash
  and required_prefix = Utils.repeat "0" difficulty in
  Utils.starts_with ~prefix: required_prefix hash_binary  

let rec find_block index previous_block timestamp data difficulty =
  let rec aux nonce =
    let hash = hash_block index timestamp ~previous_hash:(hash_of previous_block) data difficulty nonce;
    if hash_matches_difficulty hash difficulty then
      block (index hash timestamp data difficulty nonce previous_block)
    else aux (succ nonce)
  in aux 0

let add_next_block data previous_block = 
  let timestamp = Unix.time () |> Unix.gmtime
  and index = index_of previous_block + 1 in
  find_block index previous_block timestamp data difficulty

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