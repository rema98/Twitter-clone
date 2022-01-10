(*
    Distributed Operating Systems: Twitter Clone Project
    Bonus is added.
    Teamates: Kavya Gopal, Rema Veeranna Gowda
*)

module ActorOFDB

open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Json
open System
open System.Text
open System.Collections.Generic
open Akka.Actor
open Akka.FSharp
open System.Diagnostics
open TwitterServerCollections
open WebSocketSharp.Server

//
// Query Actor
//

type QueryActorMsg = 
| WebSocketToSearchActor of string * IActorRef * WebSocketSessionManager * string


type QueryWorkerMsg =
| AllTweets of string * WebSocketSessionManager * string * string[]
| SearchHashTag of string * WebSocketSessionManager * string * string[]
| SearchMention of string * WebSocketSessionManager * string * string[]

let AllTweetsActor (serverMailbox:Actor<QueryActorMsg>) =
    let nodeName = serverMailbox.Self.Path.Name
    let rec loop() = actor {
        let! (message: QueryActorMsg) = serverMailbox.Receive()
        match message with
        | WebSocketToSearchActor (msg, workerRef ,sessionManager, sid) ->
            let SearchInfo = (Json.deserialize<SearchInfo> msg)
            let userID = SearchInfo.UserID
            (* No any Tweet in history *)
            if not (allTweetsMap.ContainsKey(userID)) then
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = SearchInfo.ReqType ;
                    Status =  "NoTweet" ;
                    Desc =  Some "Searched Tweets. No Tweet to show yet" ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)
            else
                workerRef <! AllTweets (msg, sessionManager, sid, allTweetsMap.[userID].ToArray())
        return! loop()
    }
    loop() 

let SearchMentionActor (serverMailbox:Actor<QueryActorMsg>) =
    let nodeName = serverMailbox.Self.Path.Name
    let rec loop() = actor {
        let! (message: QueryActorMsg) = serverMailbox.Receive()
        match message with
        | WebSocketToSearchActor (msg, workerRef ,sessionManager, sid) ->
            let SearchInfo = (Json.deserialize<SearchInfo> msg)
            let userID = SearchInfo.UserID
            (* No any Tweet in history *)
            if not (mentionMap.ContainsKey(userID)) then
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = SearchInfo.ReqType ;
                    Status =  "NoTweet" ;
                    Desc =  Some "Searched Tweets. No Tweet to show yet" ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)
            else
                workerRef <! AllTweets (msg, sessionManager, sid, mentionMap.[userID].ToArray())
        return! loop()
    }
    loop() 

let SearchHashTagActor (serverMailbox:Actor<QueryActorMsg>) =
    let nodeName = serverMailbox.Self.Path.Name
    let rec loop() = actor {
        let! (message: QueryActorMsg) = serverMailbox.Receive()
        match message with
        | WebSocketToSearchActor (msg, workerRef ,sessionManager, sid) ->
            let SearchInfo = (Json.deserialize<SearchInfo> msg)
            let tag = SearchInfo.Tag
            (* No any Tweet in history *)
            if not (hashTagMap.ContainsKey(tag)) then
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = SearchInfo.ReqType ;
                    Status =  "NoTweet" ;
                    Desc =  Some "Searched Tweets. No Tweet to show yet" ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)
            else
                workerRef <! AllTweets (msg, sessionManager, sid, hashTagMap.[tag].ToArray())
        return! loop()
    }
    loop() 

let querySubActor (serverMailbox:Actor<QueryActorMsg>) =
    let nodeName = serverMailbox.Self.Path.Name
    let rec loop() = actor {
        let! (message: QueryActorMsg) = serverMailbox.Receive()
        match message with
        | WebSocketToSearchActor (msg, _ ,sessionManager, sid) ->
            let SearchInfo = (Json.deserialize<SearchInfo> msg)
            let userID = SearchInfo.UserID
            (* the user doesn't have any publisher subscripver information *)
            if not (subscriberMap.ContainsKey(userID)) && not (publisherMap.ContainsKey(userID))then
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = "AllTweets" ;
                    Status =  "NoTweet" ;
                    Desc =  Some ("The user doesn't have subscribers")  ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)
            else if (subscriberMap.ContainsKey(userID)) && not (publisherMap.ContainsKey(userID))then
                let SubscribeReply:SubscribeReply = {
                    ReqType = "Reply" ;
                    Type = "ShowSub" ;
                    TargetUserID = userID ;
                    Subscriber = subscriberMap.[userID].ToArray() ;
                    Publisher = [||] ;
                }
                let data = (Json.serialize SubscribeReply)
                sessionManager.SendTo(data,sid)
            else if not (subscriberMap.ContainsKey(userID)) && (publisherMap.ContainsKey(userID))then
                let SubscribeReply:SubscribeReply = {
                    ReqType = "Reply" ;
                    Type = "ShowSub" ;
                    TargetUserID = userID ;
                    Subscriber = [||] ;
                    Publisher = publisherMap.[userID].ToArray() ;
                }
                let data = (Json.serialize SubscribeReply)
                sessionManager.SendTo(data,sid)
            else 
                let SubscribeReply:SubscribeReply = {
                    ReqType = "Reply" ;
                    Type = "ShowSub" ;
                    TargetUserID = userID ;
                    Subscriber = subscriberMap.[userID].ToArray() ;
                    Publisher = publisherMap.[userID].ToArray() ;
                }
                let data = (Json.serialize SubscribeReply)
                sessionManager.SendTo(data,sid)     
        return! loop()
    }
    loop() 




//
//
// Query worker Actor (might of a bunch of workers)
//
//



let searchActorNode (mailbox:Actor<QueryWorkerMsg>) =
    let nodeName = "QueryActor " + mailbox.Self.Path.Name
    let rec loop() = actor {
        let! (message: QueryWorkerMsg) = mailbox.Receive()
       
        match message with
            | AllTweets (json, sessionManager, sid, tweetIDarray) ->
                let  jsonMsg = JsonValue.Parse(json)
                //printfn "[%s] %A" nodeName json
                let  userID = jsonMsg?UserID.AsInteger()
                
                (* send back all the tweets *)
                let mutable tweetCount = 0
                for tweetID in (tweetIDarray) do
                    if tweetMap.ContainsKey(tweetID) then
                        tweetCount <- tweetCount + 1
                        let tweetReply:TweetReply = {
                            ReqType = "Reply" ;
                            Type = "ShowTweet" ;
                            Status = tweetCount ;
                            TweetInfo = tweetMap.[tweetID] ;
                        }
                        let data = (Json.serialize tweetReply)
                        sessionManager.SendTo(data,sid)

                (* After sending ball all the history tweet, reply to sender *)
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = "AllTweets" ;
                    Status =  "Success" ;
                    Desc =  Some "Search all Tweets complete" ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)

            | SearchHashTag (json, sessionManager, sid, tweetIDarray) ->
                let  jsonMsg = JsonValue.Parse(json)
                //printfn "[%s] %A" nodeName json
                let tag = jsonMsg?Tag.AsString()
                (* send back all mentioned tweets *)
                let mutable tweetCount = 0
                for tweetID in tweetIDarray do
                    if tweetMap.ContainsKey(tweetID) then
                        tweetCount <- tweetCount + 1
                        
                        let tweetReply:TweetReply = {
                            ReqType = "Reply" ;
                            Type = "ShowTweet" ;
                            Status = tweetCount ;
                            TweetInfo = tweetMap.[tweetID] ;
                        }
                        let data = (Json.serialize tweetReply)
                        sessionManager.SendTo(data,sid)

                (* After sending back all the history tweet, reply to sender *)
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = "AllTweets" ;
                    Status =  "Success" ;
                    Desc =  Some ("Search Tweets with "+tag+ " complete") ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)

            | SearchMention (json, sessionManager, sid,tweetIDarray) ->
                let  jsonMsg = JsonValue.Parse(json)
                //printfn "[%s] %A" nodeName json
                let  userID = jsonMsg?UserID.AsInteger()
                let  reqType = jsonMsg?ReqType.AsString()
                (* send back all mentioned tweets *)
                let mutable tweetCount = 0
                for tweetID in (tweetIDarray) do
                    if tweetMap.ContainsKey(tweetID) then
                        tweetCount <- tweetCount + 1
                        let tweetReply:TweetReply = {
                            ReqType = "Reply" ;
                            Type = "ShowTweet" ;
                            Status = tweetCount ;
                            TweetInfo = tweetMap.[tweetID] ;
                        }
                        let data = (Json.serialize tweetReply)
                        sessionManager.SendTo(data,sid)

                (* After sending ball all the history tweet, reply to sender *)
                let (reply:ReplyInfo) = { 
                    ReqType = "Reply" ;
                    Type = "AllTweets" ;
                    Status =  "Success" ;
                    Desc =  Some "Search tweets with mention complete" ;
                }
                let data = (Json.serialize reply)
                sessionManager.SendTo(data,sid)
        return! loop()
    }
    loop()