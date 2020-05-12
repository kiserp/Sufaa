
module Message
open Common

    type Message = {Author : Person.Person option
                    Text: string
                    TimeStamp: System.DateTime
                    PreviousMessage: Message option}
    
    let mutable lastMessage = 
        {Author = None
         Text = ""
         TimeStamp = System.DateTime.MinValue
         PreviousMessage = None }

    let GetBytes message : byte [] =
        let byteDilimeterArr = [|delimeter|]
        let author = Person.GetBytes message.Author.Value  
        let text = System.Text.Encoding.ASCII.GetBytes(message.Text)
        let ts = System.BitConverter.GetBytes(message.TimeStamp.ToBinary())
        [|[|byte(Common.MessageType.UserMessage)|]
          author
          byteDilimeterArr   
          text
          byteDilimeterArr
          ts
          byteDilimeterArr|] 
        |> Array.concat

    // Возввращает Message и индекс последнего байта, предполагается что после message ничего в массиве нет
    let FromBytes bytes inIdx : Message = 
        let person, index = Person.FromBytesStartFromBegining bytes 
        let partiotions = SplitByDelimeter bytes [||] index  2
        let text = (fst partiotions).[0] |> System.Text.Encoding.ASCII.GetString
        let ts = System.BitConverter.ToInt64((fst partiotions).[1],0) |> System.DateTime.FromBinary 
        let prevMsg = if lastMessage.TimeStamp = System.DateTime.MinValue then None else Some(lastMessage)
        let lastMessage =
            {Author = Some(person)
             Text = text
             TimeStamp = ts
             PreviousMessage = prevMsg}
        lastMessage

   
    


