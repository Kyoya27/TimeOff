namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | CancelRequest of UserId * Guid
    | ValidateRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | CancelRequest (userId, _) -> userId
        | ValidateRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestCancelled of TimeOffRequest
    | RequestValidated of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestCancelled request -> request
        | RequestValidated request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =


    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Cancelled of TimeOffRequest
        | Validated of TimeOffRequest 
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Cancelled request -> request
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | Cancelled _ -> false
            | PendingValidation _
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let getCurrentDate = DateTime.Today
        // match unit with
        // | second ->
        // | minute ->
        // | hour ->
        // | day ->
        // | month ->
        // | year ->
      
    let Holidays = ["01/01"; "02/04"; "01/05"; "08/05"; "10/05"; "21/05"; "14/07"; "15/08"; "01/11"; "11/01"; "25/12"]

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith request1 request2 =
        if request1.Start.Date <= request2.End.Date && request1.End.Date >= request2.End.Date then
            true
        else 
            request2.Start.Date <= request1.End.Date && request2.End.Date >= request1.End.Date

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        let mutable ok = false
        for otherRequest in otherRequests do
            if overlapsWith otherRequest request then
                ok <- true
        ok
    
    let isHoliday (date: DateTime) =
        let mutable ok = false
        let year = date.Year
        for days in Holidays do
            let result = days.Split '/'
            if date.Equals(DateTime(year, result.[1] |> int, result.[0] |> int)) then
                ok <- true
        ok


    let calculateDaysOff request =
        let mutable days = float((request.End.Date - request.Start.Date).Days)
        let mutable date = request.Start.Date
        while (date.Equals(request.End.Date)).Equals(false) do
            if date.DayOfWeek.Equals(DayOfWeek.Saturday) || date.DayOfWeek.Equals(DayOfWeek.Sunday) || isHoliday date
                then days <- days - 1.0
            date <- date.AddDays(1.0)

        days <- days + 1.0
        if request.Start.HalfDay.Equals(PM) then days <- days - 0.5
        if request.End.HalfDay.Equals(AM) then days <- days - 0.5
        days

    let daysOffChanged request usersDaysOff =
        usersDaysOff - calculateDaysOff request

    let createRequest activeUserRequests  request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        // This DateTime.Today must go away!
        elif request.Start.Date <= getCurrentDate then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
    
    let cancelRequestByEmployee requestState =
        if requestState.Start.Date <= getCurrentDate then
                Ok [PendingValidation requestState]
            else
                Ok [Cancelled requestState]

    let cancelRequestByManager requestState = 
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancelled request]
        | _ ->
            Error "Request cannot be cancelled"


    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
            | CancelRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                    //let request = defaultArg (userRequests.TryFind requestId) NotCreated
                    //cancelRequestByEmployee request
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    cancelRequestByManager requestState
