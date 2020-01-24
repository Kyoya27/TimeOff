module ServerCode.App

open TimeOff
open Storage.Events

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open FSharp.Control.Tasks
open Newtonsoft.Json.Linq
open System.Collections.Generic


// ---------------------------------
// Handlers
// ---------------------------------

module HttpHandlers =

    open Microsoft.AspNetCore.Http

    type DaysOff(granted_: float, taken_: float, incoming_: float, past_: float, left_: float) =
        let mutable granted = granted_
        let mutable taken = taken_
        let mutable incoming = incoming_
        let mutable past = past_
        let mutable left = left_

    [<CLIMutable>]
    type UserAndRequestId = {
        UserId: UserId
        RequestId: Guid
    }

    [<CLIMutable>]
    type UserQuery = {
        UserId: UserId
    }

    let requestTimeOff (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! timeOffRequest = ctx.BindJsonAsync<TimeOffRequest>()
                let command = RequestTimeOff timeOffRequest
                let result = handleCommand command
                match result with
                | Ok _ -> return! json timeOffRequest next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let validateRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                printfn "AAA = %s" userAndRequestId.UserId
                let command = ValidateRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestValidated timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let refuseRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = RefuseRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestRefused timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let cancelRequestByEmployee (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = CancelRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestCancelledByEmployee timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok [RequestPendingCancelation timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
    let cancelRequestByManager (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = CancelRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestCancelledByManager timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
    let refuseRequestCancelation (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = RefuseRequestCancelation (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestValidated timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let  getDaysOffGranted (eventStore: IStore<UserId, RequestEvent>) (user: User) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let res = Logic.getDaysOffGranted
                return! Successful.OK res next ctx
            }

    let getDaysOffTaken (eventStore: IStore<UserId, RequestEvent>) (user: User) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userId = ctx.BindQueryString<UserQuery>().UserId
                let stream = eventStore.GetStream(userId)
                let requests = Seq.toList<RequestEvent> (stream.ReadAll())

                let res = Logic.getDaysOffTaken requests DateTime.Today
                return! Successful.OK res next ctx
            }

    let getDaysOffIncoming (eventStore: IStore<UserId, RequestEvent>) (user: User) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userId = ctx.BindQueryString<UserQuery>().UserId
                let stream = eventStore.GetStream(userId)
                let requests = Seq.toList<RequestEvent> (stream.ReadAll())
                
                let res = Logic.getDaysOffIncoming requests DateTime.Today
                return! Successful.OK res next ctx
            }

    let getDaysOffLeft (eventStore: IStore<UserId, RequestEvent>) (user: User) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userId = ctx.BindQueryString<UserQuery>().UserId
                let stream = eventStore.GetStream(userId)
                let requests = Seq.toList<RequestEvent> (stream.ReadAll())
                
                let res = Logic.getDaysOffLeft requests DateTime.Today
                return! Successful.OK res next ctx
            }

    let getDaysOffFromThePast (eventStore: IStore<UserId, RequestEvent>) (user: User) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userId = ctx.BindQueryString<UserQuery>().UserId
                let stream = eventStore.GetStream(userId)
                let requests = Seq.toList<RequestEvent> (stream.ReadAll())
                
                let res = Logic.getDaysLeftFromThePast requests DateTime.Today
                return! Successful.OK res next ctx
            }

    let getDaysOffSummary (eventStore: IStore<UserId, RequestEvent>) (user: User) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userId = ctx.BindQueryString<UserQuery>().UserId
                let stream = eventStore.GetStream(userId)
                let requests = Seq.toList<RequestEvent> (stream.ReadAll())
                
                // let res = DaysOff (Logic.getDaysOffGranted, Logic.getDaysOffTaken requests, Logic.getDaysOffIncoming requests, Logic.getDaysFromThePast requests, Logic.getDaysOffLeft requests)
                let granted  = Logic.getDaysOffGranted
                let taken = Logic.getDaysOffTaken requests DateTime.Today
                let incoming = Logic.getDaysOffIncoming requests DateTime.Today
                let past = Logic.getDaysLeftFromThePast requests DateTime.Today
                let left = Logic.getDaysOffLeft requests DateTime.Today

                let res = JObject [
                    JProperty("Granted", granted)
                    JProperty("Taken", taken)
                    JProperty("Incoming", incoming)
                    JProperty("Past", past)
                    JProperty("Left", left)
                ]

                return! Successful.OK res next ctx
            }

    let getHistoryByUser (eventStore: IStore<UserId, RequestEvent>) (user: User) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let res = Logic.getUserRequests eventStore user
                return! Successful.OK res next ctx
            }

// ---------------------------------
// Web app
// ---------------------------------

let webApp (eventStore: IStore<UserId, RequestEvent>) =
    let handleCommand (user: User) (command: Command) =
        let userId = command.UserId

        let eventStream = eventStore.GetStream(userId)
        let state = eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty

        // Decide how to handle the command
        let result = Logic.decide state user command

        // Save events in case of success
        match result with
        | Ok events -> eventStream.Append(events)
        | _ -> ()

        // Finally, return the result
        result
        
    choose [
        subRoute "/api"
            (choose [
                route "/users/login" >=> POST >=> Auth.Handlers.login
                subRoute "/timeoff"
                    (Auth.Handlers.requiresJwtTokenForAPI (fun user ->
                        choose [
                            POST >=> route "/request" >=> HttpHandlers.requestTimeOff (handleCommand user)
                            POST >=> route "/validate-request" >=> HttpHandlers.validateRequest (handleCommand user)
                            POST >=> route "/refuse-request" >=> HttpHandlers.refuseRequest (handleCommand user)
                            POST >=> route "/cancel-request-employee"  >=> HttpHandlers.cancelRequestByEmployee (handleCommand user)
                            POST >=> route "/cancel-request-manager"  >=> HttpHandlers.cancelRequestByManager (handleCommand user)
                            POST >=> route "/refuse-request-cancelation"  >=> HttpHandlers.refuseRequestCancelation (handleCommand user)
                            
                            GET >=> route "/history" >=> HttpHandlers.getHistoryByUser eventStore user
                            GET >=> route "/days-off-granted" >=> HttpHandlers.getDaysOffGranted eventStore user
                            GET >=> route "/days-off-taken" >=> HttpHandlers.getDaysOffTaken eventStore user
                            GET >=> route "/days-off-incoming" >=> HttpHandlers.getDaysOffIncoming eventStore user
                            GET >=> route "/days-off-past" >=> HttpHandlers.getDaysOffFromThePast eventStore user
                            GET >=> route "/days-off-left" >=> HttpHandlers.getDaysOffLeft eventStore user
                            GET >=> route "/days-off-summary" >=> HttpHandlers.getDaysOffSummary eventStore user
                        ]
                    ))
            ])
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (eventStore: IStore<UserId, RequestEvent>) (app: IApplicationBuilder) =
    let webApp = webApp eventStore
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services.AddCors() |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder: ILoggingBuilder) =
    let filter (l: LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()

    //let eventStore = InMemoryStore.Create<UserId, RequestEvent>()
    let storagePath = System.IO.Path.Combine(contentRoot, "../../../.storage", "userRequests")
    let eventStore = FileSystemStore.Create<UserId, RequestEvent>(storagePath, sprintf "%s")

    let webRoot = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder>(configureApp eventStore))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0