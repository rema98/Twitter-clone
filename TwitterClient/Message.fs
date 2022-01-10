(*
    Distributed Operating Systems: Twitter Clone Project
    Bonus is added.
    Teamates: Kavya Gopal, Rema Veeranna Gowda
*)

module Message


open System
open System.Security.Cryptography
// For Json Libraries
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Json


// JSON message structures for different API request

type ReplyInfo = {
    ReqType : string
    Type : string
    Status : string
    Desc : string option
}

type RegReply = {
    ReqType : string
    Type : string
    Status : string
    ServerPublicKey : string
    Desc : string option
}

type RegJson = {
    ReqType : string
    UserID : int
    UserName : string
    PublicKey : string
}

type SignedTweet = {
    UnsignedJson: string
    HMACSignature: string
}

type TweetInfo = {
    ReqType : string
    UserID : int
    TweetID : string
    Time : DateTime
    Content : string
    Tag : string
    Mention : int
    RetweetTimes : int
}

type TweetReply = {
    ReqType : string
    Type : string
    Status : int
    TweetInfo : TweetInfo
}

type SubscribeInfo = {
    ReqType : string
    UserID : int 
    PublisherID : int
}

type SubscribeReply = {
    ReqType : string
    Type : string
    TargetUserID : int
    Subscriber : int[]
    Publisher : int[]
}

type LoginInfo = {
    ReqType : string
    UserID : int
    Signature: string
}

type LoginReply = {
    ReqType: string
    Type: string
    Status: string
    Authentication: string
    Desc: string option
}

type SearchInfo = {
    ReqType : string
    UserID : int
    Tag : string
}

type RetweetInfo = {
    ReqType: string
    UserID : int
    TargetUserID : int
    RetweetID : string
}



// User Mode for JSON request 
// Generate different JSON string for requests

let getUserInput (option:string) = 
    let mutable keepPrompt = true
    let mutable userInputStr = ""
    //gets user input 
    match option with
    | "int" ->
    //Used to ask for userID,to take in tweet ID and other integer inpuyts from user
        while keepPrompt do
            printf "Enter a number: "
            userInputStr <- Console.ReadLine()
            match (Int32.TryParse(userInputStr)) with
            | (true, _) -> (keepPrompt <- false)
            //Throws error 
            | (false, _) ->  printfn "[ERROR] Invalid number"
        userInputStr
    | "string" ->
        while keepPrompt do
        //Asks Name for registration and inputs in the form of string
            printf "Enter a string: "
            userInputStr <- Console.ReadLine()
            match userInputStr with
            //Invalid characters are outputted as error
            | "" | "\n" | "\r" | "\r\n" | "\0" -> printfn "[ERROR] Invalid string"
            | _ -> (keepPrompt <- false)
        userInputStr
    | "YesNo" ->
        while keepPrompt do
        //prompts for furthur actions 
            printf "Enter yes/no: "
            userInputStr <- Console.ReadLine()
            match userInputStr.ToLower() with
            | "yes" | "y" -> 
                (keepPrompt <- false) 
                userInputStr<-"yes"
            | "no" | "n" ->
                (keepPrompt <- false) 
                userInputStr<-"no"
                //error on invalid input
            | _ -> printfn "[ERROR] Invalid input"
        userInputStr
    | _ ->
        userInputStr                                
    


let genRegisterJSON (publicKey:string) =
    //Register the user if not already logged in
    //Asks for ID and Name 
    printfn "Enter \"ID to register\": "
    let userid = (int) (getUserInput "int")
    printfn "Enter \"Name to resiter with\": "
    let username = (getUserInput "string")
    //Also passes the public key
    let regJSON:RegJson = { 
        ReqType = "Register" ; 
        UserID =  userid ;
        UserName = username ; 
        PublicKey = publicKey;
    }
    Json.serialize regJSON

let genConnectDisconnectJSON (option:string, curUserID:int) = 
//Connection or disconnection information
    if option = "Login" then
    //Takes input ID 
        printfn "Enter \"ID of the user\": "
        let userid = (int) (getUserInput "int")
        let connectJSON:LoginInfo = {
            ReqType = "Login" ;
            UserID = userid ;
            Signature = "";
        }
        Json.serialize connectJSON
    else
    //Logs out of the session
        let connectJSON:LoginInfo = {
            ReqType = "Logout" ;
            UserID = curUserID ;
            Signature = "";
        }
        Json.serialize connectJSON
//Tweet Input information
let genTweetJSON curUserID = 
    let mutable tag = ""
    let mutable mention = -1
    printfn "Tweet?: "
    let content = (getUserInput "string")
    //Asks for to add an hashtag
    printfn "Do want to append hashtag to tweet?"
    if (getUserInput "YesNo") = "yes" then
        printfn "ENTER HASH-TAG Ex: #tag : "
        tag <- (getUserInput "string")
    //Asks for mention in the tag
    printfn "Do you want to mention another user in your tweet?"
    if (getUserInput "YesNo") = "yes" then
        printfn "ENTER Id of the user you want to mention: "
        mention <- (int) (getUserInput "int")
    //sends tweet to the server
    let (tweetJSON:TweetInfo) = {
        ReqType = "PostTweet" ;
        UserID  = curUserID ;
        TweetID = "" ;
        //Time of tweet is also sent
        Time = (DateTime.Now) ;
        Content = content ;
        //keeps a count of tag,mention and retweet times
        Tag = tag ;
        Mention = mention ;
        RetweetTimes = 0 ;
    }
    Json.serialize tweetJSON

//subscribe
let genSubscribeJSON curUserID = 
// For one user to follow another user 
    printfn "Enter ID of the user you want to subscribe to or follow: "
    let subToUserID = (int) (getUserInput "int")
    //Sends it to be subscribed on server
    let (subJSON:SubscribeInfo) = {
        ReqType = "Subscribe" ;
        UserID = curUserID ;
        PublisherID = subToUserID;
    }
    Json.serialize subJSON
//retweet
let genRetweetJSON curUserID = 
// ID is taken to retweet the tweet needed
    printfn "Enter TWEET-ID you want to Retweet: "
    let retweetID = (getUserInput "string")
    //Capture retweet and send to server
    let (retweetJSON:RetweetInfo) = {
        ReqType = "Retweet" ;
        UserID  = curUserID ;
        TargetUserID =  -1 ;
        RetweetID = retweetID ;
    }
    Json.serialize retweetJSON
//AllTweets
//SearchHashTag
//SearchMention
//querysubscirbe
let genQueryJSON (option:string) =
    match option with
    //search hashtags if mentioned for the input given 
    | "SearchHashTag" ->
        printfn "Enter the HASH-TAG to search. Ex: #hi "
        let tag = getUserInput "string"
        //sending back to server
        let (SearchHashTagJSON:SearchInfo) = {
            ReqType = "SearchHashTag" ;
            UserID = -1 ;
            Tag = tag ;
        }
        Json.serialize SearchHashTagJSON
        //all tweets and mentions and subscribers for the user is taken
    | "AllTweets" | "SearchMention" | "SearchSubscribers" ->
        printfn "Enter USERID to \"%s\":" option
        let userid = (int) (getUserInput "int")
        //sending back to server
        let (queryJSON:SearchInfo) = {
            ReqType = option ;
            UserID = userid ;
            Tag = "" ;
        }
        Json.serialize queryJSON
    | _ -> 
    //Exits on invalid input
        printfn "[ERROR] Invalid Input to search query"
        Environment.Exit 1
        ""

let getUserID (jsonStr:string) = 
    let jsonMsg = JsonValue.Parse(jsonStr)
    (jsonMsg?UserID.AsInteger())