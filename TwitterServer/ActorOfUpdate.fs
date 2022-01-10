(*
    Distributed Operating Systems: Twitter Clone Project
    Bonus is added.
    Teamates: Kavya Gopal, Rema Veeranna Gowda
*)

module UpdateDBActors

open System
open System.Security.Cryptography
open Akka.Actor
open Akka.FSharp
open WebSocketSharp.Server
open TwitterServerCollections
(* For Json Libraries *)
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Json

open Authentication

type ActorMsg = 
| WebSocketToActor of string * WebSocketSessionManager * string

type ConActorMsg =
| WebSocketToLoginActor of string * WebSocketSessionManager * string
| AutoConnect of int

type RegActorMsg =
| WebSocketToRegisterActor of string * IActorRef * WebSocketSessionManager * string

type LoginActorMsg =
| WsockToLoginActor of string * IActorRef * WebSocketSessionManager * string

//
//   Actor for Register Request
//

let registerActor (serverMailbox:Actor<RegActorMsg>) =
    let nodeName = serverMailbox.Self.Path.Name
    let rec loop() = actor {
        let! (message: RegActorMsg) = serverMailbox.Receive()

        match message with
        | WebSocketToRegisterActor (msg, loginActorRef, sessionManager, sid) ->
            (* Save the register information into data strucute *)
            (* Check if the userID has already registered before *)
            let regMsg = (Json.deserialize<RegInfo> msg)
            let status = updateRegisterDB regMsg
            let serverECDH = ECDiffieHellman.Create()
            let serverPublicKey = 
                serverECDH.ExportSubjectPublicKeyInfo() |> Convert.ToBase64String            
                
            let reply:RegReply = { 
                ReqType = "Reply" ;
                Type = "Register" ;
                Status =  status ;
                ServerPublicKey = serverPublicKey;
                Desc = Some (regMsg.UserID.ToString());
            }
            let data = (Json.serialize reply)
            (* Reply for the register satus *)
            sessionManager.SendTo(data,sid)

            if status = "Success" then
                if isAuthDebug then
                    printfn "SERVER PUBLIC KEY GENERATED FOR User%i: %A" regMsg.UserID serverPublicKey
                    printfn "\n[%s] USER's AND SERVER's PUBLIC KEY IS SAVED IN DB" nodeName
                updateKeyDB regMsg serverECDH 
                loginActorRef <! AutoConnect (regMsg.UserID)

        return! loop()
    }
    loop()     



//
//   Actor for Subscribe Request
//
let subscribeActor (serverMailbox:Actor<ActorMsg>) =
    let nodeName = serverMailbox.Self.Path.Name
    let rec loop() = actor {
        let! (message: ActorMsg) = serverMailbox.Receive()

        match message with
        | WebSocketToActor (msg, sessionManager, sid) ->
            let SubscribeInfo = (Json.deserialize<SubscribeInfo> msg)
            let status = updatePubSubDB (SubscribeInfo.PublisherID) (SubscribeInfo.UserID)
            let mutable descStr = ""
            if status = "Success" then
                descStr <- "Successfully subscribe to User " + (SubscribeInfo.PublisherID.ToString())
            else
                descStr <- "Failed to subscribe to User " + (SubscribeInfo.PublisherID.ToString())


            let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = "Subscribe" ;
                    Status =  status ;
                    Desc =  Some descStr ;
            }
            let data = (Json.serialize reply)
            sessionManager.SendTo(data,sid)

        return! loop()
    }
    loop() 


//
//   Actor for PostTweet Request
//
let tweetActor (serverMailbox:Actor<ActorMsg>) =
    let nodeName = serverMailbox.Self.Path.Name
    let rec loop() = actor {
        let! (message: ActorMsg) = serverMailbox.Receive()

        match message with
        | WebSocketToActor (msg, sessionManager, sid) ->
            let m = (Json.deserialize<SignedTweet> msg)
            let unsignedJson = m.UnsignedJson
            let tInfo = Json.deserialize<TweetInfo> unsignedJson
            let sharedSecretKey = 
                keyMap.[tInfo.UserID].SharedSecretKey |> Convert.FromBase64String
            let tweetInfo = tInfo |> assignTweetID
            (* Store the informations for this tweet *)
            (* Check if the userID has already registered? if not, don't accept this Tweet *)
            if not (isValidUser tweetInfo.UserID) then
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = "PostTweet" ;
                    Status =  "Failed" ;
                    Desc =  Some "The user should be registered before sending a Tweet" ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)
            elif not (verifyHMAC unsignedJson m.HMACSignature sharedSecretKey) then
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = "PostTweet" ;
                    Status =  "Failed" ;
                    Desc =  Some "Cannot pass HMAC authentication" ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)
            else                
                updateTweetDB tweetInfo
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = "PostTweet" ;
                    Status =  "Success" ;
                    Desc =  Some "Successfully send a Tweet to Server" ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)            
                

        return! loop()
    }
    loop()

//
//   Actor for Retweet Request
//
let retweetActor (serverMailbox:Actor<ActorMsg>) =
    let nodeName = serverMailbox.Self.Path.Name
    let rec loop() = actor {
        let! (message: ActorMsg) = serverMailbox.Receive()

        match message with
        | WebSocketToActor (msg, sessionManager, sid) ->
            let retweetInfo = (Json.deserialize<RetweetInfo> msg)
            let retweetID = retweetInfo.RetweetID
            let userID = retweetInfo.UserID
            let tUserID = retweetInfo.TargetUserID
            let mutable isFail = false

            (* user might assign a specific retweetID or empty string *)
            if retweetID = "" then
                (* make sure the target user has at least one tweet in his history *)
                if (isValidUser tUserID) && allTweetsMap.ContainsKey(tUserID) && allTweetsMap.[tUserID].Count > 0 then
                    (* random pick one tweet from the target user's history *)
                    let rnd = Random()
                    let numTweet = allTweetsMap.[tUserID].Count
                    let rndIdx = rnd.Next(numTweet)
                    let targetReTweetID = allTweetsMap.[tUserID].[rndIdx]
                    let retweetInfo = tweetMap.[targetReTweetID]
                    (* check if the author is the one who send retweet request *)
                    if (retweetInfo.UserID <> userID) then
                        updateRetweet userID retweetInfo
                    else
                        isFail <- true
                else
                    isFail <- true
            else
                (* Check if it is a valid retweet ID in tweetDB *)
                if tweetMap.ContainsKey(retweetID) then
                    (* check if the author is the one who send retweet request *)
                    if (tweetMap.[retweetID].UserID) <> userID then
                        updateRetweet userID (tweetMap.[retweetID])
                    else
                        isFail <- true
                else
                    isFail <- true

            (* Deal with reply message *)
            if isFail then
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = "PostTweet" ;
                    Status =  "Failed" ;
                    Desc =  Some "Retweet failed, can't find the specified Tweet ID or due to author rule" ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)
            else
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = "PostTweet" ;
                    Status =  "Success" ;
                    Desc =  Some "Successfully retweet the Tweet!" ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)

        return! loop()
    }
    loop()