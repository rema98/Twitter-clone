(*
    Distributed Operating Systems: Twitter Clone Project
    Bonus is added.
    Teamates: Kavya Gopal, Rema Veeranna Gowda
*)

module UserInterface
open Message
open System
open Akka.FSharp

(* User Mode Connect/Register check *)
type UserModeStatusCheck =
| Success
| Fail
| Waiting
| Timeout
| SessionTimeout

let mutable (isUserModeLoginSuccess:UserModeStatusCheck) = Waiting
let mutable (serverPublicKey:string) = ""

(* User Mode Prompt  *)
let printBanner (printStr:string) =
    printfn "%s" printStr
//On prompt displays all the functionalities
let showPrompt option curUserID= 
    match option with
    | "loginFirst" ->
        printfn "Existing Users can login.\nNew users should register."
        printfn "1. REGISTER"
        printfn "2. LOGIN"
        printfn "3. EXIT"
        printf "Enter input: "
    | "afterLogin" ->
        printfn "\nUSER NAME: \"User%i\" \n" curUserID
        printfn "--------Choose operation-----------------------------"
        printfn "1. POST TWEET"
        printfn "2. RETWEET"
        printfn "3. SUBSCRIBE"
        printfn "4. LOGOUT"
        printfn "5. ALL TWEETS"
        printfn "6. SEARCH TAGS"
        printfn "7. SEARCh MENTIONS"
        printfn "8. SEARCH FOLLOWERS"
        printfn "9. EXIT"
        printfn "-----------------------------------------------------"
        printf "Enter Input: "
    | _ ->
        ()

//Setting timeout
let setTimeout _ =
    isUserModeLoginSuccess <- Timeout


let waitForServerResponse (timeout:float) =
    (* timeout: seconds *)
    let timer = new Timers.Timer(timeout*1000.0)
    isUserModeLoginSuccess <- Waiting
    timer.Elapsed.Add(setTimeout)
    timer.Start()
    printBanner "-----Sent User details to server. Waiting for reply.-----"
    while isUserModeLoginSuccess = Waiting do ()
    timer.Close()
//Error message on disonnection
let waitServerResPlusAutoLogoutCheck (timeout:float, command:string) =
    waitForServerResponse timeout
    if isUserModeLoginSuccess = SessionTimeout then
        printBanner (sprintf "------Error in performing \"%s\", Please reconnect to server -----" command)



let startUserInterface terminalRef =    

    let mutable curUserID = -1
    let mutable curState= 0
    (* Prompt User for Simulator Usage *)
    while true do
        (* First State, User have to register or connect(login) first *)
        (* If successfully registered, *)
        while curState = 0 do
            (showPrompt "loginFirst" curUserID)
            let inputStr = Console.ReadLine()
            match inputStr with
                | "1" ->
                    let requestJSON = genRegisterJSON "key"
                    let tmpuserID = getUserID requestJSON
                    terminalRef <! requestJSON
  
                    waitForServerResponse (5.0)
                    if isUserModeLoginSuccess = Success then
                        printBanner ("-----Registration Successful----\nUSER ID: "+ tmpuserID.ToString())
                        terminalRef <! """{"ReqType":"UserModeOn", "CurUserID":"""+"\""+ tmpuserID.ToString() + "\"}"
                        curUserID <- tmpuserID
                        curState <- 1
                        (showPrompt "afterLogin" curUserID)
                    else if isUserModeLoginSuccess = Fail then
                        printBanner (sprintf "Error registering UserID: %i\n -----USERID exists!----" tmpuserID)

                    else
                        printBanner ("Error registering UserID: " + tmpuserID.ToString() + "\n -----Session timed out----")


                | "2" ->
                    let requestJSON = genConnectDisconnectJSON ("Login", -1)
                    let tmpuserID = getUserID requestJSON
                    terminalRef <! requestJSON

                    waitForServerResponse (5.0)
                    if isUserModeLoginSuccess = Success then
                        printBanner ("Login Successful: USER: "+ tmpuserID.ToString())
                        terminalRef <! """{"ReqType":"UserModeOn", "CurUserID":"""+"\""+ tmpuserID.ToString() + "\"}"
                        curUserID <- tmpuserID
                        curState <- 1
                        (showPrompt "afterLogin" curUserID)
                    else if isUserModeLoginSuccess = Fail then
                        ()
                        // TO CHECK
                        //printBanner (sprintf "Faild to connect and login for UserID: %i\nIncorrect userID or already connected before..." tmpuserID)

                    else
                        printBanner ("Login Failed. USER: " + tmpuserID.ToString() + "\n --------No Response from server--------")


                | "3" ->
                    printBanner "-------------EXITING!------------"
                    Environment.Exit 1
                | _ ->
                    ()


        while curState = 1 do
            let inputStr = Console.ReadLine()
            match inputStr with
                | "1" ->
                    terminalRef <! genTweetJSON curUserID
                    waitServerResPlusAutoLogoutCheck (5.0, "PostTweet")

                | "2" -> 
                    terminalRef <! genRetweetJSON curUserID
                    waitServerResPlusAutoLogoutCheck (5.0, "retweet")

                | "3" -> 
                    terminalRef <! genSubscribeJSON curUserID
                    waitServerResPlusAutoLogoutCheck (5.0, "subscribe")

                | "4"  ->
                    terminalRef <! genConnectDisconnectJSON ("Logout", curUserID)
                    waitForServerResponse (5.0)
                    if isUserModeLoginSuccess = Success then
                        printBanner ("Logout Successful. USERID: "+ curUserID.ToString())
                        curUserID <- -1
                        curState <- 0
                    else if isUserModeLoginSuccess = SessionTimeout then
                        curUserID <- -1
                        curState <- 0
                        // (showPrompt "loginFirst" curUserID)

                | "5" -> 
                    terminalRef <! genQueryJSON "AllTweets"
                    waitServerResPlusAutoLogoutCheck (10.0, "AllTweets")

                | "6" -> 
                    terminalRef <! genQueryJSON "SearchHashTag"
                    waitServerResPlusAutoLogoutCheck (5.0, "SearchHashTag")

                | "7" -> 
                    terminalRef <! genQueryJSON "SearchMention"
                    waitServerResPlusAutoLogoutCheck (5.0, "SearchMention")

                | "8" -> 
                    terminalRef <! genQueryJSON "SearchSubscribers"
                    waitServerResPlusAutoLogoutCheck (5.0, "SearchSubscribers")

                | "9" ->
                    terminalRef <! genConnectDisconnectJSON ("Logout", curUserID)
                    waitForServerResponse (5.0)
                    printBanner "-------------EXITING!--------------"
                    Environment.Exit 1
                | _ ->
                    (showPrompt "afterLogin" curUserID)
                    ()
