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

    test "Should not overlap with any requests" {
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

      let request3 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
        RequestStatus = OnHold
      }

       let reqs = [ request1; request2 ]

      Expect.isTrue(Logic.overlapsWithAnyRequest reqs request3) "Requests overlap"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2010, 12, 27); HalfDay = PM } 
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
        Start = { Date = DateTime(2020, 12, 27); HalfDay = AM }
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
let RefuseRequest =
  testList "Cancel request" [
    test "Cancel" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 25); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 28); HalfDay = PM } 
        RequestStatus = OnHold
      }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (RefuseRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestRefused request]) "The request should have been refused"
    }
  ]

[<Tests>]
let cancelEmplTests =
  testList "Cancel requests by employee" [
    test "A request is canceled from created by employee" {
      let request = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 02, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 02, 27); HalfDay = PM } 
        RequestStatus = OnHold
      }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "employee1")
      |> When (CancelRequest ("employee1", request.RequestId))
      |> Then (Ok [RequestCancelledByEmployee request]) "The request should have been canceled"
    }

    test "A request is canceled from validated by employee" {
      let request = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 02, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 02, 27); HalfDay = PM } 
        RequestStatus = Validated
      }

      Given [ RequestValidated request ]
      |> ConnectedAs (Employee "employee1")
      |> When (CancelRequest ("employee1", request.RequestId))
      |> Then (Ok [RequestCancelledByEmployee request]) "The request should have been canceled"
    }

    test "A request is pending cancelation from validated by employee" {
      let request = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 22); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 25); HalfDay = PM } 
        RequestStatus = Validated
      }

      Given [ RequestValidated request ]
      |> ConnectedAs (Employee "employee1")
      |> When (CancelRequest ("employee1", request.RequestId))
      |> Then (Ok [RequestPendingCancelation request]) "The request should have been in pending cancelation"
    }
  ]

[<Tests>]
let cancelTManagerests =
  testList "Cancel requests by manager" [
    test "A request is canceled from created by manager" {
      let request = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 02, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 02, 27); HalfDay = PM } 
        RequestStatus = OnHold
      }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("employee1", request.RequestId))
      |> Then (Ok [RequestCancelledByManager request]) "The request should have been canceled"
    }

    test "A request is canceled from validated by manager" {
      let request = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 02, 27); HalfDay = AM }
        End = { Date = DateTime(2020, 02, 27); HalfDay = PM } 
        RequestStatus = Validated
      }

      Given [ RequestValidated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("employee1", request.RequestId))
      |> Then (Ok [RequestCancelledByManager request]) "The request should have been canceled"
    }

    test "A request is canceled from pending cancelation by manager" {
      let request = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 22); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 25); HalfDay = PM } 
        RequestStatus = PendingCancelation
      }

      Given [ RequestPendingCancelation request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("employee1", request.RequestId))
      |> Then (Ok [ RequestCancelledByManager request]) "The request should have been canceled"
    }
  ]

[<Tests>]
let refuseCancelation =
  testList "Cancelation refused by manager" [
    test "A request is refused from pending cancelation by manager" {
      let request = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 02, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 02, 27); HalfDay = PM } 
        RequestStatus = OnHold
      }

      Given [ RequestPendingCancelation request ]
      |> ConnectedAs Manager
      |> When (RefuseRequestCancelation ("employee1", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
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
      let (reqEvent:RequestEvent) = RequestValidated request
      let result = Logic.calculateDaysOff reqEvent
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
      let (reqEvent:RequestEvent) = RequestValidated request
      let result = Logic.calculateDaysOff reqEvent
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

      let (reqEvent:RequestEvent) = RequestValidated request
      let result = Logic.calculateDaysOff reqEvent
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

      let (reqEvent:RequestEvent) = RequestValidated request
      let result = Logic.calculateDaysOff reqEvent
      Expect.isTrue ((2.0).Equals(result)) "Calcul should be right"
    }
  ]

[<Tests>]
let holidaysTaken =
  testList "Taken tests" [
    test "Check days off taken, one request" {
      let request = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 03); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 06); HalfDay = PM } 
        RequestStatus = Validated
      }
      let (reqEvent:RequestEvent) = RequestValidated request
      let (result: float) = Logic.getDaysOffTaken [reqEvent] (DateTime(2020, 01, 20))
      Expect.isTrue ((2.0).Equals(result)) "Result should be 2"
    }

    test "Check days off taken, two requests" {
      let request1 = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 03); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 06); HalfDay = PM } 
        RequestStatus = Validated
      }

      let request2 = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 08); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 09); HalfDay = AM } 
        RequestStatus = Validated
      }
      let (reqEvent1:RequestEvent) = RequestValidated request1
      let (reqEvent2:RequestEvent) = RequestValidated request2
      let (result: float) = Logic.getDaysOffTaken [reqEvent1; reqEvent2] (DateTime(2020, 01, 20))
      Expect.isTrue ((3.5).Equals(result)) "Result should be 3.5"
    }
  ]

[<Tests>]
let holidaysIncoming =
  testList "Incoming tests" [
    test "Check days off incoming, one request" {
      let request = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 13); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 20); HalfDay = PM } 
        RequestStatus = Validated
      }
      let (reqEvent:RequestEvent) = RequestValidated request
      let (result: float) = Logic.getDaysOffIncoming [reqEvent] (DateTime(2020, 01, 10))
      Expect.isTrue ((6.0).Equals(result)) "Result should be 6"
    }

    test "Check days off incoming, two requests" {
      let request1 = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 13); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 20); HalfDay = PM } 
        RequestStatus = Validated
      }

      let request2 = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 21); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 22); HalfDay = AM } 
        RequestStatus = Validated
      }
      let (reqEvent1:RequestEvent) = RequestValidated request1
      let (reqEvent2:RequestEvent) = RequestValidated request2
      let (result: float) = Logic.getDaysOffIncoming [reqEvent1; reqEvent2] (DateTime(2020, 01, 10))
      Expect.isTrue ((7.5).Equals(result)) "Result should be 7.5"
    }
  ]

[<Tests>]
let holidaysFromLastYear =
  testList "Past days tests" [
    test "Check days off days from last year, one request" {
      let request = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 13); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 20); HalfDay = PM } 
        RequestStatus = Validated
      }

      let (reqEvent:RequestEvent) = RequestValidated request
      let (result: float) = Logic.getDaysLeftFromThePast [reqEvent] (DateTime(2020, 01, 10))
      Expect.isTrue ((14.0).Equals(result)) "Result should be 20 - 6 = 14"
    }

    test "Check days off from last year, two requests" {
      let request1 = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 13); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 20); HalfDay = PM } 
        RequestStatus = Validated
      }

      let request2 = {
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 24); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 26); HalfDay = AM } // Noël
        RequestStatus = Validated
      }

      let (reqEvent1:RequestEvent) = RequestValidated request1
      let (reqEvent2:RequestEvent) = RequestValidated request2
      let (result: float) = Logic.getDaysLeftFromThePast [reqEvent1; reqEvent2] (DateTime(2020, 01, 10))
      Expect.isTrue((12.5).Equals(result)) "Result should be 20 - 7.5 = 12.5"
    }
  ]

[<Tests>]
let holidaysLeft =
  testList "Holidays left" [
    test "Check days off days from last year, one request" {
      let requestTaken = { // 5 used
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 03); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 09); HalfDay = PM } 
        RequestStatus = Validated
      }

      let requestIncoming = { // 5 used
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 13); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 17); HalfDay = PM } 
        RequestStatus = Validated
      }

      let requestPast = { // 2 given
        UserId = "employee1"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 02); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 26); HalfDay = PM } 
        RequestStatus = Validated
      }

      let (reqTaken: RequestEvent) = RequestValidated requestTaken
      let (reqIncoming: RequestEvent) = RequestValidated requestIncoming
      let (reqPast: RequestEvent) = RequestValidated requestPast
      let (result: float) = Logic.getDaysOffLeft [reqTaken; reqIncoming; reqPast] (DateTime(2020, 01, 10))
      Expect.isTrue ((12.0).Equals(result)) "Result should be 20 - 5 - 5 + 2 = 12"
    }
  ]