(*
    Distributed Operating Systems: Twitter Clone Project
    Bonus is added.
    Teamates: Kavya Gopal, Rema Veeranna Gowda
*)

module ServerWebSocketInterface
open WebSocketSharp.Server
open System
open UpdateDBActors

// Disconnect from server
type Disconnect () =
    inherit WebSocketBehavior()
    override x.OnMessage message = 
        // Disconnecting from server. Prints all information
        printfn "SERVER ID: %A" x.ID
        printfn "SESSION ID: %A" x.Sessions.IDs
        printfn "SERVER DISCONNECTED %s" message.Data 
        // Sends disconnected message to client
        x.Send (message.Data + " [from server]")