namespace TimeOff.AuthTypes

open TimeOff
open System

// Json web token type.
type JWT = string

// Login credentials.
type Login =
    { UserName   : string
      Password   : string
      PasswordId : Guid }

type UserData =
  { UserName : string
    User     : User
    Token    : JWT }
