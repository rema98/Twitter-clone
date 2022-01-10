------------------------------------------------------------------------
 Goal 
------------------------------------------------------------------------
We had implemented all the functionalities for a twitter like engine in Part 1 which will be paired up with a WebSocket API in this part (Part 2). Some of the functionalities are mentioned below :

1. a JSON based API that represents all messages and their replies (including errors)
2. re-wrote parts of our engine using WebSharper to implement the API interface
3. Implemented a client to use REST API and take inputs from the command line.

+ BONUS IMPLEMENTED

-------------------------------------------------------------------------
Instructions To Run code
-------------------------------------------------------------------------
1. Download our code from canvas submission
2. In visual studio Code import the twitter API project.
3. Change the folder to TwitterClient on one terminal which is the client side.
4. Change the folder to TwitterServer on second terminal which is the server side.
4. Run the project using command “dotnet run user” on client side and "dotnet run" on server side. The API will start.
5. Both the client and server are up and many functionalities are given simulating the twitter engine.
---------------------------------------------------------------------------
Implementation 
---------------------------------------------------------------------------
1. We are implementing the REST API using websharper.
2. It contains GET requests which include : “/hashtag”, “/searchmention, “/searchhashtag" and
“/alltweets”.
3. The POST requests include : “/register”, “/login", “/retweet” and “/subscribe”
4. All these are of the format " http://localhost:9001/api/register/ ".
5. Once the server gets the requests the work is delegated to other functions.
6. On the client side we get the responses as jsons.
7. For GET requests we can parse the json in a straightforward manner.
8. For the POST request the response is present in the body of the json. It is then converted to a string which is then displayed to the user.

* Client Program: CLI based terminal (with message prompts, just follow the messages in the terminal)
* Server Program: Press any key to terminate the program while runni
------------------------------------------------------------------------
  Libraries/Programming labguage
------------------------------------------------------------------------
* Libraries Used:
NET 5.0
F#
Akka.FSharp
FSharp.JSON
Websocket-sharp

* Special Libraries Used:

FSharp.Json
Websocket-sharp
Akka.FSharp
FSharp.Data

------------------------------------------------------------------------
  Brief Demo Vedio Link
------------------------------------------------------------------------
Brief Demo Video on YouTube: 
https://www.youtube.com/watch?v=m350Y6yXQOQ


------------------------------------------------------------------------
  Author
------------------------------------------------------------------------
Kavya Gopal : 6581-2209
Rema Veeranna Gowda : 9462-8005

------------------------------------------------------------------------
  Usage
------------------------------------------------------------------------

* Directories:
    Server Program : "TwitterServer"
    Client Program : "TwitterClient"


------------------------------------------------------------------------
BONUS PART
------------------------------------------------------------------------
A user, upon registration, provides a public key (can be RSA-2048 or a 256-bit ElipticCurve)
When the user re-connects via WebSockets, it first authenticates using a challenge-based algorithm
The engine sends a 256-bit challenge
The client forms a message containing the challenge, the current time (UNIX time in seconds) and digitally signs it.
The engine sends a confirmation or an error
The engine is not allowed to reuse the challenge and it must only cache it for 1 second. If the protocol does not finish within 1s, it must be tried again
The user establishes a secret key with the engine (using Diffie-Helman protocol) and HMAC signs every message sent
The HMAC is computed over the serialized JSON content.
