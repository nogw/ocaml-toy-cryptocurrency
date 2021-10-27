open Blockchain

type message = 
  | QueryAll
  | ResponseBlockchain of string
  [@@deriving yojson]

type peer = {
  socket: Dream.websocket;
  connection_time: float
}

type state = {
  mutable peers: peer list;
  mutable chain: block;
}

let write peer message =
  message 
  |> yojson_of_message
  |> Yojson.Safe.to_string
  |> Dream.send peer.socket 

let broadcast peers message =
  List.iter (fun peer -> write peer message |> ignore) peers

let chain_message b = 
  let data = c 
    |> yojson_of_block
    |> Yojson.Safe.to_string 
  in ResponseBlockchain data

let handle_blockchain_response state received = 
  if index_of received > index_of state.chain then
    if (hash_of received) = (hash_of state.chain) then
      begin
        received |> chain_message |> broadcast state.peers;
        received
      end
    else 
      replace received state.chain
  else 
    state.chain

let init_message_handle state peer =
  match%lwt Dream.received peer.socket with
  | Some m -> 
      let message = m 
        |> Yojson.Safe.from_string
        |> message_of_yojson
      in 
        begin
          match message with
          | QueryAll -> state.chain |> chain_message |> write peer
          | ResponseBlockchain chain_string ->
            state.chain <- chain_string 
            |> Yojson.Safe.from_string
            |> block_of_yojson
            |> handle_blockchain_response state;
            Lwt.return_unit
        end
  | None -> state.peers <- List.filter ( 
      fun { socket = _; connection_time } -> connection_time <> peer.connection_time 
    ) state.chain;
    Dream.close_websocket peer.socket

let init_connection state peer =
  state.peers <- peer :: state.peers;
  state.chain
  |> chain_message
  |> write peer
  |> ignore;
  init_message_handle state peer

let init_p2p_server state =
  Dream.websocket (
    fun ws -> {
      { socket = ws; connection_time = Unix.time () }
      |> init_connection state 
    }
  )

let init_server port =
  Dream.run ~port
  @@ Dream.logger
  @@ Dream.router [
    Dream.get  "/blocks" (
      fun _ ->
      state.chain
      |> yojson_of_block
      |> Yojson.Safe.to_string
      |> Dream.json
    );
    Dream.get  "/block/:hash" 
      (fun _ -> Dream.html ("."));
    Dream.get  "/transaction/:id" 
      (fun _ -> Dream.html ("."));
    Dream.get  "/address/:address" 
      (fun _ -> Dream.html ("."));
    Dream.get  "/unspentTransactionOutputs" 
      (fun _ -> Dream.html ("."));
    Dream.get  "/myUnspentTransactionOutputs" 
      (fun _ -> Dream.html ("."));
    Dream.post "/mineRawBlock" 
      (fun _ -> Dream.html ("."));
    Dream.post "/mineBlock" (
      fun _ -> 
      let%lwt body = Dream.body req in
      store.chain <- add_next_block body store.chain;
      store.chain
      |> yojson_of_block
      |> Yojson.Safe.to_string
      |> Dream.json
    );
    Dream.get  "/balance" 
      (fun _ -> Dream.html ("."));
    Dream.get  "/address" 
      (fun _ -> Dream.html ("."));
    Dream.post "/mineTransaction" 
      (fun _ -> Dream.html ("."));
    Dream.post "/sendTransaction" 
      (fun _ -> Dream.html ("."));
    Dream.get  "/transactionPool" 
      (fun _ -> Dream.html ("."));
    Dream.get  "/peers" 
      (fun _ -> Dream.html ("."));
    Dream.post "/addPeer" ( fun _ -> init_p2p_server state );
    Dream.post "/stop" 
      (fun _ -> Dream.html ("."));
  ]
  @@ Dream.not_found