namespace ServerCode.Auth

open TimeOff
open Giraffe
open RequestErrors
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2

/// Represents the authenticated user for a request
type Identity =
    {
        UserName : string
        User: User
    }

/// Login handlers and functions for API handlers request authorisation with JWT.
module Handlers =

    let private tryGetValidIdentityFromLogin (login: TimeOff.AuthTypes.Login) : Identity option =
        if login.Password = login.UserName then
            match login.UserName with
            | "manager"   -> Some { UserName = login.UserName; User = Manager }
            | "employee1" -> Some { UserName = login.UserName; User = Employee login.UserName }
            | "employee2" -> Some { UserName = login.UserName; User = Employee login.UserName }
            | "employee3" -> Some { UserName = login.UserName; User = Employee login.UserName }
            | "employee4" -> Some { UserName = login.UserName; User = Employee login.UserName }
            | "employee5" -> Some { UserName = login.UserName; User = Employee login.UserName }
            | _ -> None
        else
            None

    let private createUserData (identity: Identity) =
        {
            UserName = identity.UserName
            User     = identity.User
            Token    = JsonWebToken.encode identity
        } : TimeOff.AuthTypes.UserData

    /// Authenticates a user and returns a token in the HTTP body.
    let login : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! login = ctx.BindJsonAsync<TimeOff.AuthTypes.Login>()
                return!
                    match tryGetValidIdentityFromLogin login with
                    | Some identity ->
                        let data = createUserData identity
                        ctx.WriteJsonAsync data
                    | None -> UNAUTHORIZED "Bearer" "" (sprintf "User '%s' can't be logged in." login.UserName) next ctx
            }

    let private missingToken = RequestErrors.BAD_REQUEST "Request doesn't contain a JSON Web Token"
    let private invalidToken = RequestErrors.FORBIDDEN "Accessing this API is not allowed"

    /// Returns true if the JSON Web Token is successfully decoded and the signature is verified.
    let private isValid (jwt: string) : Identity option =
        try
            let token = JsonWebToken.decode jwt
            Some token
        with
        | _ -> None

    /// Checks if the HTTP request has a valid JWT token for API.
    /// On success it will invoke the given `f` function by passing in the valid token.
    let requiresJwtTokenForAPI f : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            (match ctx.TryGetRequestHeader "Authorization" with
            | Some authHeader ->
                let jwt = authHeader.Replace("Bearer ", "")
                match isValid jwt with
                | Some identity -> f identity.User
                | None -> invalidToken
            | None -> missingToken) next ctx
