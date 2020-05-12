
module Server
open System.Net.Sockets 
open System
open System.Net
open System.Net.NetworkInformation
open System.Threading
open Common
open Person


let mutable private  localEndPoint: IPEndPoint = null
let mutable private server: IPEndPoint = null
let mutable private userList: Person list = []
let mutable private role = Role.Server

let SetServer serverEndPoint = 
    server <- serverEndPoint

let GetLocalEndpoint = 
     fun () -> localEndPoint

let SetRole newRole = 
    role <- newRole

let GetRole =
    fun () -> role

let GetStartPort = 
    fun () -> 
        match role with 
        | Role.Server -> 12310
        | _ -> 12311

let AddNewClient (person: Person) =
    userList <- person :: userList

let DropClient (person: Person) = 
    userList <-
        userList 
        |> List.filter( fun psn -> psn <> person)

//Trying to bind to available port 
let rec TryBind port maxPort = 
    try 
        let sc = new Socket(SocketType.Dgram, ProtocolType.Udp)
        sc.EnableBroadcast <- true
        //let  host = 
        //    Dns.GetHostAddresses("localhost") 
        //    |> Array.filter(fun addr -> addr.AddressFamily = AddressFamily.InterNetwork)
        let addrs=
            NetworkInterface.GetAllNetworkInterfaces()
            |> Array.filter (fun nif -> 
                nif.OperationalStatus = OperationalStatus.Up &&
                nif.NetworkInterfaceType <> NetworkInterfaceType.Loopback
            )
            |> Array.map (fun nif -> nif.GetIPProperties().UnicastAddresses)
            |> Seq.concat
            |> Seq.filter (fun (ua: UnicastIPAddressInformation) -> ua.IPv4Mask <> IPAddress(0L))
            |> Seq.map( fun ua -> ua.Address)
            |> Seq.toArray
        let address = 
            match addrs.Length with
            | 1 -> addrs.[0]
            | _ -> 
                printfn "Выбери Интерфейс"
                for idx in [0 .. addrs.Length-1] do
                    let ua = addrs.[idx].MapToIPv4().ToString()
                    printfn "%d %s" idx ua
                let input = Console.ReadKey().KeyChar.ToString()
                let userkey = Int32.Parse(input)
                if userkey > addrs.Length then addrs.[0] else addrs.[userkey]
        localEndPoint <- new IPEndPoint(address,port)
        sc.Bind(localEndPoint)
        printfn "Bound to port %d" port
        Some(sc)
    with
    | :? SocketException -> 
         match port with
         | prt when prt = maxPort -> 
             printfn "Error - Can't Open Any socket!"
             None
         | _ -> TryBind (port + 1) maxPort
    | _ as ex -> 
        printfn "Error - %s" ex.Message
        None
        
let LocalSock = 
    fun () ->
        let startPort = GetStartPort() 
        match (TryBind startPort 12320) with
        | Some res -> 
         printfn "Server Created" 
         res
        | _ -> 
         printfn "Enough! It is no 1t a Sofa-shop!"
         System.Console.Read()
         exit 1


let rec AsyncWriteSock (sock: Socket) (message: byte []) attempt (endPoint: Option<IPEndPoint>) =
    async {
        let ep = if endPoint = None then IPEndPoint(IPAddress.Broadcast, 12310) else endPoint.Value
        let result = sock.SendToAsync(System.ArraySegment(message),SocketFlags.None, ep).Result
        match result, attempt with
        | res, 0 when res < 1 ->  AsyncWriteSock sock message 1
        | res, 1 when res < 1 ->  false
        | _, _ -> true
    } |> Async.Start

let AsyncWriteByteSock (sock: Socket) (message: byte []) =
    AsyncWriteSock sock message 0

let AsyncWriteStringSock (sock: Socket) (message: string) =
    let msgBytes = System.Text.Encoding.ASCII.GetBytes(message)
    AsyncWriteByteSock sock msgBytes

let AsyncHandlePing localSocket bytes = 
    async {
        let epBytes = IPEndpointToBytes localEndPoint
        let pongMsg = 
                [|[|byte(MessageType.Pong)|]
                  epBytes|]
                |> Array.concat
        let rmEndpoint = fst (IPEndpointFromBytes bytes 1)
        AsyncWriteByteSock localSocket pongMsg (Some rmEndpoint)
    } |> Async.Start

let AsyncHandlePong localSocket bytes = 
    async {
        let serverEP = fst (Common.IPEndpointFromBytes bytes 0)
        SetServer serverEP
    } |> Async.Start

let AsyncHandleUserStatus socket msgType bytes = 
    let person = fst (Person.FromBytes 1 bytes)
    match msgType, GetRole() with 
    | MessageType.UserLogin, Role.Client -> 
        printfn "User - %s - logged in!" person.Nickname
        AddNewClient person
    | MessageType.UserLogoff, Role.Client -> 
        printfn "User - %s - logged off!" person.Nickname
        DropClient person
    | MessageType.UserLogin, Role.Server -> 
        AddNewClient person
        userList
        |> List.map ( fun (usr: Person) ->  AsyncWriteByteSock socket bytes (Some usr.Connection))
        printfn "User - %s - logged in!" person.Nickname
    | MessageType.UserLogoff, Role.Server -> 
        DropClient person
        userList
        |> List.map ( fun (usr: Person) ->  AsyncWriteByteSock socket bytes (Some usr.Connection))
        printfn "User - %s - logged off!" person.Nickname

let AsyncMessageHandler socket  bytes = 
    async {
        let msg = Message.FromBytes bytes 1
        match role with
        | Role.Server -> 
             userList
             |> List.map ( fun (usr: Person) ->  AsyncWriteByteSock socket bytes (Some usr.Connection))
             ()
        | _ -> ()
        
    } |> Async.Start

let rec AsyncReadSock (sc: Socket) = 
    async {
            let localReader = AsyncReadSock sc
            let buf = Array.zeroCreate<byte>(500)
            let read = sc.ReceiveAsync(System.ArraySegment(buf),SocketFlags.None).Result
            match read with
            | 0 -> AsyncReadSock sc
            | _ ->
                let msgType = AsMessageType buf.[0]
                match msgType with 
                | MessageType.Ping -> 
                    match role with
                    | Role.Server -> buf.[1..] |> AsyncHandlePing sc 
                    | _ -> ()
                    AsyncReadSock sc
                | MessageType.Pong ->
                    AsyncHandlePong sc buf.[1..] 
                    AsyncReadSock sc
                | MessageType.UserMessage ->
                    AsyncMessageHandler sc buf
                    AsyncReadSock sc
                | MessageType.UserLogin -> 
                    AsyncHandleUserStatus sc MessageType.UserLogin buf
                    AsyncReadSock sc
                | MessageType.UserLogoff -> 
                    AsyncHandleUserStatus sc MessageType.UserLogoff buf
                    AsyncReadSock sc
                | MessageType.UserList -> 

                    AsyncReadSock sc
                | _ ->
                    printfn "UNKNONW RECIEVED \n --- %s --- \n" 
                        (System.Text.Encoding.ASCII.GetString(buf)) //should be only for Server 
                    AsyncReadSock sc
    } |> Async.Start
    
let FindServer (sock: Socket) = 
    AsyncWriteByteSock sock [|byte(MessageType.Ping)|]

// Should check server (client) status, but I won't.
let StatusCheck = 
    0