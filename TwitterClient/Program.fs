(*
    Distributed Operating Systems: Twitter Clone Project
    Bonus is added.
    Teamates: Kavya Gopal, Rema Veeranna Gowda
*)
//All the required libraries 
open System
open System.Security.Cryptography
open System.Globalization
open System.Text
open System.Collections.Generic
open Akka.Actor
open Akka.FSharp
open System.Diagnostics
open UserAPIs
open Authentication

//For Json libraries
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Json

open Simulator
open Message
open UserInterface

//Websockert Interface
open WebSocketSharp
//Actor call
let system = ActorSystem.Create("UserInterface")
let serverWebsockAddr = "ws://localhost:9001"
let globalTimer = Stopwatch()






//Child Node Actor 
let clientActorNode (isSimulation) (clientMailbox:Actor<string>) =
    //Nodename with the userID is called
    let mutable nodeName = "User" + clientMailbox.Self.Path.Name
    let mutable nodeID = 
        match (Int32.TryParse(clientMailbox.Self.Path.Name)) with
        | (true, value) -> value
        | (false, _) -> 0
    //For authorization DiffieHellman code is created
    let nodeECDH = ECDiffieHellman.Create()
    let nodePublicKey = nodeECDH.ExportSubjectPublicKeyInfo() |> Convert.ToBase64String
    //Websockets are created and added for every service
    let wssDB = createWebsocketDB (serverWebsockAddr)
    (wssDB.["Register"]).OnMessage.Add(regCallback (nodeName, wssDB, isSimulation))
    (wssDB.["PostTweet"]).OnMessage.Add(replyCallback (nodeName))
    (wssDB.["Retweet"]).OnMessage.Add(replyCallback (nodeName))
    (wssDB.["Subscribe"]).OnMessage.Add(replyCallback (nodeName))
    (wssDB.["AllTweets"]).OnMessage.Add(queryCallback (nodeName))
    (wssDB.["SearchMention"]).OnMessage.Add(queryCallback (nodeName))
    (wssDB.["SearchHashTag"]).OnMessage.Add(queryCallback (nodeName))
    (wssDB.["SearchSubscribers"]).OnMessage.Add(queryCallback (nodeName))
    (wssDB.["Logout"]).OnMessage.Add(disconnectCallback (nodeName, wssDB))
    (wssDB.["Login"]).OnMessage.Add(connectCallback (nodeID, wssDB, nodeECDH))

    let rec loop() = actor {
        let! (message: string) = clientMailbox.Receive()
        let  jsonMsg = JsonValue.Parse(message)
        let  reqType = jsonMsg?ReqType.AsString()
        //Each message on match is sent to server for record purpose and will be displayed on server side
        match reqType with
            //user registration
            | "Register" ->                
                sendRegMsgToServer (message,isSimulation, wssDB.[reqType], nodeID, nodePublicKey)
            // tweet posting
            | "PostTweet" ->
                PostTweetToServer (message, wssDB.["PostTweet"], nodeName, nodeECDH)
            // all other functionalities 
            | "Retweet" | "Subscribe"
            | "AllTweets" | "SearchMention" | "SearchHashTag" | "SearchSubscribers" 
            //Logout from the session
            | "Logout" ->
                sendRequestMsgToServer (message, reqType, wssDB, nodeName)       
            //Login into the session 
            | "Login" ->
                let wssCon = wssDB.["Login"]
                wssCon.Connect()
                // TODO: send private key to this call back
                // start authentication with server in this call back
                // Need New Form of JSON that could encapsule origin JSON
                wssCon.Send(message)

            | "UserModeOn" ->
                let curUserID = jsonMsg?CurUserID.AsInteger()
                nodeID <- curUserID
                nodeName <- "User" + curUserID.ToString()

            | _ ->
                printfn "Client node \"%s\" received unknown message \"%s\"" nodeName reqType
                Environment.Exit 1
         
        return! loop()
    }
    loop()




[<EntryPoint>]
let main argv =
    try
        globalTimer.Start()
        (* dotnet run [simulate | user | debug] *)
        let programMode = argv.[0]

        if programMode = "user" then
            (* Create a terminal actor node for user mode *)
            
            let terminalRef = spawn system "-Terminal" (clientActorNode false)
            startUserInterface terminalRef

        // else if programMode = "simulate" then
        //     getSimualtionParamFromUser()
        //     startSimulation system globalTimer (clientActorNode true)

        else if programMode = "debug" then
            printfn "\n\n[Debug Mode]Show authentication messages\n"
            isAuthDebug <- true
            let terminalRef = spawn system "-Terminal" (clientActorNode false)
            startUserInterface terminalRef
            // use default simulation parameters
            // startSimulation system globalTimer (clientActorNode true)
        else
            printfn "\n\n[Error] Wrong argument!!\n Plese use: \n\t1. dotnet run simulate\n\t2. dotnet run user\n\t3. dotnet run debug\n"
            Environment.Exit 1

          
    with | :? IndexOutOfRangeException ->
            printfn "\n\n[Error] Wrong argument!!\n Please use: \n1. dotnet run user\n2. dotnet run debug\n\n"

         | :? FormatException ->
            printfn "\n[Main] FormatException!\n"


    0 