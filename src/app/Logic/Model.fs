﻿namespace TimeOff

open System
open Storage.Events

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | RefuseRequest of UserId *  Guid
    | CancelRequest of UserId * Guid
    | RefuseRequestCancelation of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | RefuseRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | RefuseRequestCancelation (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestPendingCancelation of TimeOffRequest
    | RequestCancelledByManager of TimeOffRequest
    | RequestCancelledByEmployee of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestRefused request -> request
        | RequestPendingCancelation request -> request
        | RequestCancelledByManager request -> request
        | RequestCancelledByEmployee request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =
    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest
        | Refused of TimeOffRequest
        | PendingCancelation of TimeOffRequest
        | CancelledByManager of TimeOffRequest
        | CancelledByEmployee of TimeOffRequest
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Refused request -> request
            | PendingCancelation request
            | CancelledByManager request -> request
            | CancelledByEmployee request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _ -> true
            | Validated _ -> true
            | Refused _ -> false
            | PendingCancelation _ -> true
            | CancelledByManager _ -> false
            | CancelledByEmployee _ -> false

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
        | RequestRefused request -> Refused request
        | RequestPendingCancelation request -> PendingCancelation request
        | RequestCancelledByManager request -> CancelledByManager request
        | RequestCancelledByEmployee request -> CancelledByEmployee request

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

    let calculateDaysOff (request: RequestEvent) =
        let mutable days = float((request.Request.End.Date - request.Request.Start.Date).Days) + 1.0
        let mutable date = request.Request.Start.Date
        while (date <= request.Request.End.Date).Equals(true) do
            if date.DayOfWeek.Equals(DayOfWeek.Saturday) || date.DayOfWeek.Equals(DayOfWeek.Sunday) || isHoliday date then
                days <- days - 1.0
            date <- date.AddDays(1.0)

        //days <- days + 1.0
        if not (request.Request.Start.Date.DayOfWeek.Equals(DayOfWeek.Saturday))
            && not (request.Request.Start.Date.DayOfWeek.Equals(DayOfWeek.Sunday))
            && request.Request.Start.HalfDay.Equals(PM)
            then days <- days - 0.5
        
        if not (request.Request.End.Date.DayOfWeek.Equals(DayOfWeek.Saturday))
            && not (request.Request.End.Date.DayOfWeek.Equals(DayOfWeek.Sunday))
            && request.Request.End.HalfDay.Equals(AM)
            then days <- days - 0.5
        days

    // let daysOffChanged request usersDaysOff =
    //     usersDaysOff - calculateDaysOff request

    let getDaysOffGranted = 20.0

    let getDaysOffTaken (requests: RequestEvent list) (today: DateTime) =
        let mutable count = 0.0
        let firstDay = DateTime(today.Year, 1, 1)

        for req in requests do
            if req.Request.RequestStatus = Status.Validated && req.Request.Start.Date >= firstDay && req.Request.End.Date <= today then
                count <- count + calculateDaysOff req
        count

    let getDaysOffIncoming (requests: RequestEvent list) (today: DateTime) =
        let mutable count = 0.0
        let lastDay = DateTime(today.Year, 12, 31)

        for req in requests do
            if req.Request.RequestStatus = Status.Validated && req.Request.Start.Date > today && req.Request.End.Date <= lastDay then
                count <- count + calculateDaysOff req
        count
    
    let getDaysLeftFromThePast (requests: RequestEvent list) (today: DateTime) =
        let  mutable count = 0.0
        let firstDay = DateTime(today.Year - 1, 1, 1)
        let lastDay = DateTime(today.Year - 1, 12, 31)

        for req in requests do
            if req.Request.RequestStatus = Status.Validated && req.Request.Start.Date >= firstDay && req.Request.End.Date <= lastDay then
                count <- count + calculateDaysOff req

        getDaysOffGranted - count
    
    let getDaysOffLeft (requests: RequestEvent list) (today: DateTime) =
        getDaysOffGranted + (getDaysLeftFromThePast requests today) - (getDaysOffTaken requests today + getDaysOffIncoming requests today)

    let createRequest activeUserRequests request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        // This DateTime.Today must go away!
        elif request.Start.Date <= getCurrentDate then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | _ -> Error "Request can't be refused"

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            request.RequestStatus <- Status.Validated
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let cancelRequestByManager requestState = 
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancelledByManager request]
        | PendingCancelation request ->
            Ok [RequestCancelledByManager request]
        | Validated request  ->
            Ok [RequestCancelledByManager request]
        | _ ->
            Error "Request cannot be cancelled"

    let cancelRequestByEmployee (requestState: RequestState) =
        if getCurrentDate >= requestState.Request.Start.Date then
            Ok [RequestPendingCancelation requestState.Request]
        else
            Ok [RequestCancelledByEmployee requestState.Request]

    let refuseRequestCancelation requestState =
        match requestState with
        | PendingCancelation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request's pending cancelation canno't be refused"

    let getUserRequests (eventStore: IStore<UserId, RequestEvent>) (user: User) =
        match user with
        | Employee userId ->
            let eventStream = eventStore.GetStream(userId)
            eventStream.ReadAll() |> Seq.fold evolveUserRequests Map.empty 
        | _ -> Map.empty 

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
            
            | RefuseRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    refuseRequest requestState

            | CancelRequest (_, requestId) ->
                if user <> Manager then
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    cancelRequestByEmployee requestState
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    cancelRequestByManager requestState
            | RefuseRequestCancelation (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    refuseRequestCancelation requestState
