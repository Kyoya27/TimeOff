module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide userRequestsState user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
        RequestStatus = OnHold
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
        RequestStatus = OnHold
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
        RequestStatus = OnHold
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } 
        RequestStatus = OnHold
        }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }

    test "Request Cancelled" {
      let request = {
        UserId = "Ahmed"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 31); HalfDay = PM }
        RequestStatus = OnHold
      }

      Given [ ]
      |> ConnectedAs (Employee "Ahmed")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "Request On Hold"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } 
        RequestStatus = OnHold
      }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }

    test "Request Anwsered From Manager" {
      let request = {
        UserId = "Ahmed2"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 01, 31); HalfDay = AM } 
        RequestStatus = OnHold
      }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("Ahmed2", request.RequestId))
      |> Then (Ok [RequestValidated request]) "Request Validated By Manager"
    }
  ]

[<Tests>]
let daysOffCalculsTests =
  testList "Calculs tests" [
    test "A day is taken" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } 
        RequestStatus = OnHold
      }

      let result = Logic.calculateDaysOff request
      Expect.isTrue ((1.0).Equals(result)) "Calcul should be right"
    }

    test "A day is taken on half and half" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 28); HalfDay = PM }
        End = { Date = DateTime(2019, 10, 29); HalfDay = AM } 
        RequestStatus = OnHold
      }

      let result = Logic.calculateDaysOff request
      Expect.isTrue ((1.0).Equals(result)) "Calcul should be right"
    }

    test "Two days were took on the week-end" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 25); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 28); HalfDay = PM } 
        RequestStatus = OnHold
      }

      let result = Logic.calculateDaysOff request
      Expect.isTrue ((2.0).Equals(result)) "Calcul should be right"
    }

    test "An Holiday" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 24); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 26); HalfDay = PM } 
        RequestStatus = OnHold
      }

      let result = Logic.calculateDaysOff request
      Expect.isTrue ((2.0).Equals(result)) "Calcul should be right"
    }
  ]
