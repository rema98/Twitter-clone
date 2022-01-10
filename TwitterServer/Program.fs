(*
    Distributed Operating Systems: Twitter Clone Project
    Bonus is added.
    Teamates: Kavya Gopal, Rema Veeranna Gowda
*)

open ActorOFDB
open Authentication
open TwitterServerCollections
open WebSocketSharp.Server
open UpdateDBActors

open System
open System.Diagnostics
open System.Security.Cryptography
open System.Globalization
open System.Collections.Generic
open System.Text
open Akka.Actor
open Akka.FSharp

// Json Libraries
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Json

let system = ActorSystem.Create("TwitterEngine")

// Child Actors to perform tasks
let noOfActors = 1000

// Actors to perform search operation
let searchActors (clientNum: int) = 
    [1 .. clientNum]
    |> List.map (fun id -> (spawn system ("Q"+id.ToString()) searchActorNode))
    |> List.toArray

let searchChildActor = searchActors noOfActors

// Choose a random actor
let chooseRandomActor () =
    let randomNumber = Random()
    searchChildActor.[randomNumber.Next(noOfActors)]

// Actor for sending connection request
// List of online users is empty
let mutable onlineListOfUsersSet = Set.empty
let updateOnlineListOfUsersDB userID option = 
    let isConnected = onlineListOfUsersSet.Contains(userID)

    // When user is logged in, add userID to onlineListOfUsersSet
    if option = "Login" && not isConnected then
        if isValidUser userID then
            onlineListOfUsersSet <- onlineListOfUsersSet.Add(userID)
            0
        else
            -1
    // When user is logged out, remove userID from onlineListOfUsersSet
    else if option = "Logout" && isConnected then
        onlineListOfUsersSet <- onlineListOfUsersSet.Remove(userID)
        0
    else
        0


// Connection to the actors to start the login service and call child actors
let LoginActor (serverMailbox:Actor<ConActorMsg>) =
    let nodeName = serverMailbox.Self.Path.Name
    
    
    let rec loop() = actor {

        let! (message: ConActorMsg) = serverMailbox.Receive()
        match message with
        | WebSocketToLoginActor (msg, sessionManager, sid) ->
        // receives information from websocket regarding connection establishment,which user and the request he is making
            let loginInfo = (Json.deserialize<LoginInfo> msg)
            let userID = loginInfo.UserID
            let requestType = loginInfo.ReqType           
            
            match requestType with
            | "Login" ->
                //If successful login only it continues to query or throws an exception
                if not (onlineListOfUsersSet.Contains(userID)) && isValidUser userID then
                //challenge is generated for authorization 
                    let ch = generateChallenge
                    challengeCaching userID ch |> Async.Start
                    if isAuthDebug then
                        printfn "CHALLENGE FOR CLIENT: %A\nThe challenge is cached for a second" ch
                        printfn "[timestamp] %A\n" (DateTime.Now)
                    let (reply:LoginReply) = { 
                        ReqType = "Reply" ;
                        Type = requestType ;
                        Status =  "Authentication" ;
                        Authentication = ch;
                        Desc =  Some (userID.ToString());
                    }
                    let data = (Json.serialize reply)
                    sessionManager.SendTo(data,sid)  
                else 
                // Failed connection if not sucessful login
                    let (reply:LoginReply) = { 
                        ReqType = "Reply" ;
                        Type = requestType ;
                        Status =  "Fail" ;
                        Authentication = "";
                        Desc =  Some ("Please register first") ;
                    }
                    let data = (Json.serialize reply)
                    sessionManager.SendTo(data,sid)  
            // BONUS PART                       
            | "Authentication" ->
            // authorization using timestamp,signature
                let keyInfo = keyMap.[userID]
                if (challengeCache.ContainsKey userID) then
                    let answer = challengeCache.[userID]
                    let signature = loginInfo.Signature
                    if isAuthDebug then
                    //Prints out all information regarding the authorization.
                        printfn "Server authentication reply: Time:  %A" DateTime.Now
                        printfn "Server is veryfing challenge from User%i..." userID
                        printfn "Retriving the cached challenge and verfiying with user's public key"
                    //Verifies signature with public key
                    if (verifySignature answer signature keyInfo.UserPublicKey) then
                    //if successful it connectes and process continues
                        let (reply:LoginReply) = { 
                            ReqType = "Reply" ;
                            Type = requestType ;
                            //status set to success
                            Status =  "Success" ;
                            Authentication = "";
                            Desc =  Some (userID.ToString());
                        }
                        let data = (Json.serialize reply)
                        sessionManager.SendTo(data,sid)  
                        // serverMailbox.Self <! AutoConnect userID
                    else
                    //Wont login if authorization fails and displays a error message
                        printfn "\n[Error] Authentication failed for User%i\n" userID
                        let (reply:LoginReply) = { 
                            ReqType = "Reply" ;
                            Type = requestType ;
                            //status set to fail
                            Status =  "Fail" ;
                            Desc =  Some "Authentication failed!" ;
                            Authentication = "";
                        }
                        let data = (Json.serialize reply)
                        sessionManager.SendTo(data,sid)
                else
                    let (reply:LoginReply) = { 
                        ReqType = "Reply" ;
                        Type = requestType ;
                        Status =  "Fail" ;
                        Desc =  Some "Authentication failed!" ;
                        Authentication = "";
                    }
                    let data = (Json.serialize reply)
                    sessionManager.SendTo(data,sid)
            | _ ->
                //On getting disconnected ,user cannot send or query anything
                (updateOnlineListOfUsersDB userID "Logout") |> ignore
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = requestType ;
                    //status set to success
                    Status =  "Success" ;
                    Desc =   Some (userID.ToString()) ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)
    
        // Login to existing user
        | AutoConnect userID ->
            // Check if user is registered and then login
            let ret = updateOnlineListOfUsersDB userID "Login"
            // When the user is not registered, ret < 0 and login fails 
            if ret < 0 then printfn "[USERID:%i] LOGIN FAILED " userID
            else printfn "[USERID:%i] LOGIN SUCCESSFUL!" userID

        return! loop()
    }
    loop() 





// WEBSOCKET IMPLEMENTATION

// websocket server
let wss = WebSocketServer("ws://localhost:9001")

// Reference to actors for each operation
let registrationActorRef = spawn system "Register-DB-Worker" registerActor
let tweetActorRef = spawn system "Tweet-DB-Worker" tweetActor
let retweetActorRef = spawn system "ReTweet-DB-Worker" retweetActor
let subscribeActorRef = spawn system "Subscribe-DB-Worker" subscribeActor
let loginActorRef = spawn system "Login-DB-Worker" LoginActor
let allTweetsActorRef = spawn system "AllTweets-DB-Worker" AllTweetsActor
let searchMentionsActorRef = spawn system "SearchMentions-DB-Worker" SearchMentionActor
let SearchHashTagActorRef = spawn system "SearchHashTag-DB-Worker" SearchHashTagActor
let displaySubscribersActorRef = spawn system "DisplaySubscribers-DB-Worker" querySubActor

// websocket behavior for register login
type Register () =
//inherits the websocket functionalities
    inherit WebSocketBehavior()
    // on message passed,websocket call
    override wssm.OnMessage message = 
        printfn "\n[/registertion data]\nData:%s\n" message.Data 
        registrationActorRef <! WebSocketToRegisterActor (message.Data, loginActorRef, wssm.Sessions, wssm.ID)

// websocket behavior for tweet
type Tweet () =
//inherits the websocket functionalities
    inherit WebSocketBehavior()
    override wssm.OnMessage message = // on message passed, websocket call
        printfn "\n[/tweet/send]\nData:%s\n" message.Data 
        tweetActorRef <! WebSocketToActor (message.Data, wssm.Sessions, wssm.ID)

type Retweet () =
    inherit WebSocketBehavior()
    override wssm.OnMessage message = 
    // on message passed,websocket call
        printfn "\n[/tweet/retweet]\nData:%s\n"  message.Data 
        retweetActorRef <! WebSocketToActor (message.Data, wssm.Sessions, wssm.ID)

type Subscribe () =
    inherit WebSocketBehavior()
    override wssm.OnMessage message = 
    // on message passed,websocket call
        printfn "\n[/subscribe]\nData:%s\n" message.Data 
        subscribeActorRef <! WebSocketToActor (message.Data,wssm.Sessions,wssm.ID)

type Connection () =
    inherit WebSocketBehavior()
    override wssm.OnMessage message = 
        printfn "\n[/connection]\nData:%s\n" (message.Data)// on message passed,websocket call
        loginActorRef <! WebSocketToLoginActor (message.Data,wssm.Sessions,wssm.ID)

type QueryHis () =
    inherit WebSocketBehavior()
    override wssm.OnMessage message = 
    // on message passed,websocket call
        printfn "\n[/tweet/query]\nData:%s\n" message.Data
        allTweetsActorRef <! WebSocketToSearchActor (message.Data, chooseRandomActor(), wssm.Sessions, wssm.ID)

type QueryMen () =
    inherit WebSocketBehavior()
    override wssm.OnMessage message = 
    // on message passed,websocket call
        printfn "\n[/mention/query]\nData:%s\n" message.Data
        searchMentionsActorRef <! WebSocketToSearchActor (message.Data, chooseRandomActor(), wssm.Sessions, wssm.ID)

type SearchHashTag () =
    inherit WebSocketBehavior()
    override wssm.OnMessage message = 
    // on message passed,websocket call
        printfn "\n[/tag/query]\nData:%s\n" message.Data
        SearchHashTagActorRef <! WebSocketToSearchActor (message.Data, chooseRandomActor(), wssm.Sessions, wssm.ID)
type QuerySub () =
    inherit WebSocketBehavior()
    override wssm.OnMessage message = 
    // on message passed,websocket call
        printfn "\n[/subscribe/query]\nData:%s\n" message.Data
        displaySubscribersActorRef <! WebSocketToSearchActor (message.Data, chooseRandomActor() , wssm.Sessions, wssm.ID)




[<EntryPoint>]
let main argv =
    try
        if argv.Length <> 0 then
        //checks if aunthentication passed or not
            isAuthDebug <- 
                match (argv.[0]) with
                | "debug" -> true
                | _ -> false
        //webservices added for each functionality
        wss.AddWebSocketService<Register> ("/register")
        wss.AddWebSocketService<Tweet> ("/tweet/send")
        wss.AddWebSocketService<Retweet> ("/tweet/retweet")
        wss.AddWebSocketService<Subscribe> ("/subscribe")
        wss.AddWebSocketService<Connection> ("/connect")
        wss.AddWebSocketService<Connection> ("/disconnect")
        wss.AddWebSocketService<QueryHis> ("/tweet/query")
        wss.AddWebSocketService<QueryMen> ("/mention/query")
        wss.AddWebSocketService<SearchHashTag> ("/tag/query")
        wss.AddWebSocketService<QuerySub> ("/subscribe/query")
        wss.Start ()
        printfn "\n-------------------------------------"
        if isAuthDebug then
            printfn "SERVER STARTED"
        else
            printfn "Twitter server started...."
        printfn "-------------------------------------\n"
        Console.ReadLine() |> ignore
        wss.Stop()
 
    //error handling
    with | :? IndexOutOfRangeException ->
    //index out of bounds exception message printed out
            printfn "\n[ERROR] Index Out Of Range Exception!\n"
    // Format eroor displayed if encountered
         | :?  FormatException ->
            printfn "\n[ERROR] Incorrect Format Exception!\n"


    0 // return an integer exit code
