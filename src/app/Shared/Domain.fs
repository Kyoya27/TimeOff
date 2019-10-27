namespace TimeOff

open System

// First, we define our domain
type UserId = string

type User =
    | Employee of UserId
    | Manager

type HalfDay = | AM | PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

type Status =  OnHold | UpperValidation | Refused | CancelRequest | CancelledByEmployee | NotApprovedCancel | CancelledByManager 

[<CLIMutable>]
type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
    RequestStatus: Status
}

type DaysOff = {
    UserId: UserId
    Available: int
}