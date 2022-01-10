(*
    Distributed Operating Systems: Twitter Clone Project
    Bonus is added.
    Teamates: Kavya Gopal, Rema Veeranna Gowda
*)

module UserAPIs

open System
open System.Security.Cryptography
open Authentication
open Message
open UserInterface
open WebSocketSharp
open FSharp.Json
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.Collections.Generic

//Web Socket Creation with Rest API
let createWebsocketDB serverWebsockAddr =
    let wssActorDB = new Dictionary<string, WebSocket>()
    (wssActorDB.Add("Register", new WebSocket(serverWebsockAddr      +   "/register")))
    (wssActorDB.Add("PostTweet", new WebSocket(serverWebsockAddr     +   "/tweet/send")))
    (wssActorDB.Add("Retweet", new WebSocket(serverWebsockAddr       +   "/tweet/retweet")))
    (wssActorDB.Add("Subscribe", new WebSocket(serverWebsockAddr     +   "/subscribe")))
    (wssActorDB.Add("Login", new WebSocket(serverWebsockAddr       +   "/connect")))
    (wssActorDB.Add("Logout", new WebSocket(serverWebsockAddr    +   "/disconnect")))
    (wssActorDB.Add("AllTweets", new WebSocket(serverWebsockAddr  +   "/tweet/query")))
    (wssActorDB.Add("SearchMention", new WebSocket(serverWebsockAddr  +   "/mention/query")))
    (wssActorDB.Add("SearchHashTag", new WebSocket(serverWebsockAddr      +   "/tag/query")))
    (wssActorDB.Add("SearchSubscribers", new WebSocket(serverWebsockAddr +  "/subscribe/query")))
    wssActorDB
// Addition of Connection 
let enableWss (wssDB:Dictionary<string, WebSocket>) =
    if (not wssDB.["PostTweet"].IsAlive) then (wssDB.["PostTweet"].Connect())
    if (not wssDB.["Retweet"].IsAlive) then (wssDB.["Retweet"].Connect())
    if (not wssDB.["Subscribe"].IsAlive) then (wssDB.["Subscribe"].Connect())
    if (not wssDB.["Logout"].IsAlive) then (wssDB.["Logout"].Connect())
    if (not wssDB.["AllTweets"].IsAlive) then (wssDB.["AllTweets"].Connect())
    if (not wssDB.["SearchMention"].IsAlive) then (wssDB.["SearchMention"].Connect())
    if (not wssDB.["SearchHashTag"].IsAlive) then (wssDB.["SearchHashTag"].Connect())
    if (not wssDB.["SearchSubscribers"].IsAlive) then (wssDB.["SearchSubscribers"].Connect())
// Closing Of Connection
let disableWss (wssDB:Dictionary<string, WebSocket>) =
    if (wssDB.["PostTweet"].IsAlive) then (wssDB.["PostTweet"].Close())
    if (wssDB.["Retweet"].IsAlive) then (wssDB.["Retweet"].Close())
    if (wssDB.["Subscribe"].IsAlive) then (wssDB.["Subscribe"].Close())
    if (wssDB.["Logout"].IsAlive) then (wssDB.["Logout"].Close())
    if (wssDB.["AllTweets"].IsAlive) then (wssDB.["AllTweets"].Close())
    if (wssDB.["SearchMention"].IsAlive) then (wssDB.["SearchMention"].Close())
    if (wssDB.["SearchHashTag"].IsAlive) then (wssDB.["SearchHashTag"].Close())
    if (wssDB.["SearchSubscribers"].IsAlive) then (wssDB.["SearchSubscribers"].Close())

// 
// Websocket OnMessage Callback Functions
// 

// Register reuqest callback
let regCallback (nodeName, wssDB:Dictionary<string,WebSocket>, isSimulation:bool) = fun (msg:MessageEventArgs) ->
    let replyInfo = (Json.deserialize<RegReply> msg.Data)
    let isSuccess = if (replyInfo.Status = "Success") then (true) else (false)

    if isSuccess then
        enableWss (wssDB)
        if isSimulation then 
        //On success registraion complete
            printfn "[%s] USERID \"%s\" REGISTRATION SUCCESSFUL. AUTO LOGIN COMPLETE" nodeName (replyInfo.Desc.Value)
        else 
        serverPublicKey <- replyInfo.ServerPublicKey
        if isAuthDebug then
            printfn "----------SERVER'S PUBLIC KEY: %A ------------" serverPublicKey
        isUserModeLoginSuccess <- Success
    else
    //else registration failure
        if isSimulation then printfn "[%s] REGISTRATION FAILED!\n" nodeName
        else isUserModeLoginSuccess <- Fail
    // Close the session for /register
    wssDB.["Register"].Close()

let connectCallback (nodeID, wssDB:Dictionary<string,WebSocket>, ecdh: ECDiffieHellman) = fun (msg:MessageEventArgs) ->
    let replyInfo = (Json.deserialize<LoginReply> msg.Data)
    match replyInfo.Status with
    //Reply status on success
    | "Success" -> 
        enableWss (wssDB)
        
        wssDB.["Login"].Close()
        if isAuthDebug then
            printfn "[User%i] AUTHENTICATION SUCCESSFUL!" nodeID
      //automatically displays all the tweets of user
        let (queryMsg:SearchInfo) = {
            ReqType = "AllTweets" ;
            UserID = (replyInfo.Desc.Value|> int) ;
            Tag = "" ;
        }
        wssDB.["AllTweets"].Send(Json.serialize queryMsg)
        isUserModeLoginSuccess <- Success
    | "Authentication" -> 
    //Public Key Authentication
        let challenge = replyInfo.Authentication |> Convert.FromBase64String
        if isAuthDebug then
            printfn "RECIEVED CHALLENGE FROM SERVER: %A" replyInfo.Authentication
            printfn "APPENDED TIME PADDING TO CHALLENGE\nDIGITAL SIGN COMPLETE\n"
        let signature = getSignature challenge ecdh
        if isAuthDebug then 
            printfn "SIGNING WITH USERS PRIVATE KEY IS COMPLETE\nSIGNATURE : %A" signature
        let (authMsg:LoginInfo) = {
            UserID = replyInfo.Desc.Value|> int;
            ReqType = "Authentication";
            Signature = signature;
        }
        wssDB.["Login"].Send(Json.serialize authMsg)
    | _ ->
        isUserModeLoginSuccess <- Fail
        printBanner (sprintf "CONNECTION FAILURE UserID: %i\nERROR: %A" nodeID (replyInfo.Desc.Value))
        wssDB.["Login"].Close()
//Diconnecting
let disconnectCallback (nodeName, wssDB:Dictionary<string,WebSocket>) = fun (msg:MessageEventArgs) ->
    disableWss (wssDB)
    isUserModeLoginSuccess <- Success


let replyCallback (nodeName) = fun (msg:MessageEventArgs) ->
    let replyInfo = (Json.deserialize<ReplyInfo> msg.Data)
    let isSuccess = if (replyInfo.Status = "Success") then (true) else (false)
    if isSuccess then
        isUserModeLoginSuccess <- Success
        //printfn "[%s] %s" nodeName (replyInfo.Desc.Value)
        printBanner (sprintf "[%s] %s" nodeName (replyInfo.Desc.Value))
    else 
        isUserModeLoginSuccess <- Fail
        printBanner (sprintf "[%s] ERROR\n%s" nodeName (replyInfo.Desc.Value))
//twwet printing
let printTweet message = 
    let tweetReplyInfo = (Json.deserialize<TweetReply> message)
    let tweetInfo = tweetReplyInfo.TweetInfo
    printfn "\n------------------------------------"
    printfn "Index: %i      \nTime: %s" (tweetReplyInfo.Status) (tweetInfo.Time.ToString())
    printfn "Author: User%i" (tweetInfo.UserID)
    let mentionStr = if (tweetInfo.Mention < 0) then "@N/A" else ("@User"+tweetInfo.Mention.ToString())
    let tagStr = if (tweetInfo.Tag = "") then "#N/A" else (tweetInfo.Tag)
    printfn "TWEET: %s %s %s  \nRetweet times: %i" (tweetInfo.Content) (tagStr) (mentionStr) (tweetInfo.RetweetTimes)
    printfn "TWEER ID: %s" (tweetInfo.TweetID)
//Printing subscibers
let printSubscribe message nodeName =
    let SubscribeReplyInfo = (Json.deserialize<SubscribeReply> message)
    printfn "\n------------------------------------"
    printfn "Name: %s" ("User" + (SubscribeReplyInfo.TargetUserID.ToString()))
    //prints who is fllowin and who are the followers for the user ID
    printf "Following: " 
    for id in SubscribeReplyInfo.Subscriber do
        printf "UserID %i " id
    printf "\nFollowers: "
    for id in SubscribeReplyInfo.Publisher do
        printf "UserID %i " id
    printfn "\n"
    printBanner (sprintf "[%s]]User%i's SUBSCRIBERS SHOWN" nodeName SubscribeReplyInfo.TargetUserID)
//Showing all tweets 
let queryCallback (nodeName) = fun (msg:MessageEventArgs) ->
    let  jsonMsg = JsonValue.Parse(msg.Data)
    let  reqType = jsonMsg?Type.AsString()
    if reqType = "ShowTweet" then
        printTweet (msg.Data)
    else if reqType = "ShowSub" then 
        printSubscribe (msg.Data) (nodeName)
    else
        let isSuccess = if (jsonMsg?Status.AsString() = "Success") then (true) else (false)
        if isSuccess then 
            isUserModeLoginSuccess <- Success
            printBanner (sprintf "[%s]\n%s" nodeName (jsonMsg?Desc.AsString()))
        else 
            isUserModeLoginSuccess <- Fail
            printBanner (sprintf "[%s]\n%s" nodeName (jsonMsg?Desc.AsString()))




// 
// Client Actor Node Helper Functinos
// 

let sendRegMsgToServer (msg:string, isSimulation, wssReg:WebSocket, nodeID, publicKey) =
    wssReg.Connect()
    if isSimulation then
        let regMsg:RegJson = { 
            ReqType = "Register" ; 
            UserID = nodeID ; 
            UserName = "User"+ (nodeID.ToString()) ; 
            PublicKey = publicKey ;
        }
        let data = (Json.serialize regMsg)
        wssReg.Send(data)
    else
        //wssReg.Send(msg)
        let message = Json.deserialize<RegJson> msg
        let regMsg:RegJson = { 
            ReqType = message.ReqType; 
            UserID = message.UserID; 
            UserName = message.UserName ; 
            PublicKey = publicKey ;
        }
        let data = (Json.serialize regMsg)
        wssReg.Send(data)
   

let sendRequestMsgToServer (msg:string, reqType, wssDB:Dictionary<string,WebSocket>, nodeName) =
    if not (wssDB.[reqType].IsAlive) then
        if reqType = "Logout" then
            wssDB.[reqType].Connect()
            wssDB.[reqType].Send(msg)
            printBanner (sprintf "[%s]\nSERVER DISCONNECTED" nodeName)
            isUserModeLoginSuccess <- SessionTimeout
        else
            isUserModeLoginSuccess <- SessionTimeout
    else 
        wssDB.[reqType].Send(msg)

let PostTweetToServer (msg:string, ws:WebSocket, nodeName, ecdh: ECDiffieHellman) =
    if not (ws.IsAlive) then
        isUserModeLoginSuccess <- SessionTimeout        
    else 
        let key = getSharedSecretKey ecdh serverPublicKey

        let signature = getHMACSignature msg key
        if isAuthDebug then
            printfn "Generate shared secret key with server with user's private key and server's public key:"
            printfn "Shared secret key (it won't send to the server):\n %A" (key|>Convert.ToBase64String)
            printfn "Origin message: %A" msg
            printfn "HMAC sign the org message with shared secret key before sending to server"
            printfn "HMAC signatrue: %A" signature
        let (signedMsg:SignedTweet) = {
            UnsignedJson = msg
            HMACSignature = signature
        }
        let data = (Json.serialize signedMsg)
        ws.Send(data)