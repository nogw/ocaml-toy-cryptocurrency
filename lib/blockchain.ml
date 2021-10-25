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

let hash_block index timestamp ?(previous_hash = "") data =
  [ string_of_int index;
    Unix.mktime timestamp |> fst |> string_of_float;
    data;
    previous_hash
  ] |> String.concat "" |> Sha256.string |> Sha256.to_hex

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