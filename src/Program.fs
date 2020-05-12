//Чатик тестовое задание. Простой чатик a la диван или кроватка.

open System
open Person
open Message
open Server
[<EntryPoint>]
let main argv =
    printfn "Hello, my friend!"
    printfn "Wellcome to Sufaa chat."
    printfn "Run as Server? (yes,no - default)"
    match Console.ReadLine() with 
    | "yes" -> 
        Server.SetRole Common.Role.Server
    | _ -> 
        Server.SetRole Common.Role.Client
    let cn = LocalSock()
    let lEP = GetLocalEndpoint ()
    
    AsyncReadSock cn
    
    Server.FindServer cn

    let msg = 
        {Author = Some(user)
         Text = " Hello every One!!!! this is my first message"
         TimeStamp = DateTime.Now
         PreviousMessage = None
        } 
        |> Message.GetBytes

    AsyncWriteByteSock cn msg


    Console.ReadLine()

    0 // return an integer exit code
