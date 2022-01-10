(*
    Distributed Operating Systems: Twitter Clone Project
    Bonus is added.
    Teamates: Kavya Gopal, Rema Veeranna Gowda
*)


module Authentication

open System
open System.Security.Cryptography

// a flag for authentication debugging
let mutable isAuthDebug = false

// Conversion of string to Bytes
let stringToBytes (str: string) = 
    Text.Encoding.UTF8.GetBytes str
//Addition of Padding
let addTimePadding (message: byte[]) =
    let curTime = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
    let padding = curTime |> BitConverter.GetBytes
    if isAuthDebug then
         printfn "\nCURRENT TIME IN s:%i (unix seconds)\n" curTime 
    Array.concat [|message; padding|]
//Hashing the message 
let getHashed (message: byte[]) = 
    message |> SHA256.HashData 
//Public Key Generation
let getECPublicKey (pub:byte[]) =
    let ecdh = ECDiffieHellman.Create()
    let size = ecdh.KeySize
    ecdh.ImportSubjectPublicKeyInfo((System.ReadOnlySpan pub), (ref size))
    ecdh.ExportParameters(false)
//Signature for challenges
let getSignature (message: byte[]) (ecdh: ECDiffieHellman) =
    let ecdsa = ecdh.ExportParameters(true) |> ECDsa.Create
    let hasedMessage = message |> addTimePadding |> getHashed
    if isAuthDebug then
        printfn "HASH(TIME + CHALLENGE): %A" (hasedMessage|>Convert.ToBase64String)
    ecdsa.SignData(hasedMessage, HashAlgorithmName.SHA256) |> Convert.ToBase64String


//DH AND HMAC FUNCTIONS
//Diffie Helman algorithm added
let getSharedSecretKey (clientECDH: ECDiffieHellman) (publicKey: String) = 
    let pub = publicKey |> Convert.FromBase64String
    let size = clientECDH.KeySize
    let temp = ECDiffieHellman.Create()
    temp.ImportSubjectPublicKeyInfo((System.ReadOnlySpan pub), (ref size))
    clientECDH.DeriveKeyMaterial(temp.PublicKey)
//Hmac signature
let getHMACSignature (jsonMessage: string) (sharedSecretKey: byte[]) =    
    use hmac = new HMACSHA1(sharedSecretKey)
    jsonMessage |> stringToBytes |> hmac.ComputeHash |> Convert.ToBase64String

