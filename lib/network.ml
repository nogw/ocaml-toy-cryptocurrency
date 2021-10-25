let init_server port =
  Dream.run ~port
  @@ Dream.logger
  @@ Dream.router [
    Dream.get  "/blocks"  
      (fun _ -> Dream.html ("."));
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
    Dream.post "/mineBlock" 
      (fun _ -> Dream.html ("."));
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
    Dream.post "/addPeer" 
      (fun _ -> Dream.html ("."));
    Dream.post "/stop" 
      (fun _ -> Dream.html ("."));
  ]
  @@ Dream.not_found