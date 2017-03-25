module Person exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as JD

type alias Person =
  { email : String
  , name : String
  , employer : String
  , age : Int
  , balance : Int
  , address : String
  , gender : String
  , account_number : Int
  }

personDecoder : JD.Decoder Person
personDecoder =
  let
    nameDecoder : JD.Decoder String
    nameDecoder = 
      JD.map3 (\a b c -> a ++ b ++ c)
        (JD.field "firstname" JD.string)
        (JD.succeed " ")
        (JD.field "lastname" JD.string)

    addressDecoder : JD.Decoder String
    addressDecoder =
      JD.map5 (\a b c d e -> a ++ b ++ c ++ d ++ e)
        (JD.field "address" JD.string)
        (JD.succeed ", ")
        (JD.field "city" JD.string)
        (JD.succeed ", ")
        (JD.field "state" JD.string)
      
  in
    JD.map8 Person
      (JD.field "email" JD.string)
      nameDecoder
      (JD.field "employer" JD.string)
      (JD.field "age" JD.int)
      (JD.field "balance" JD.int)
      addressDecoder
      (JD.field "gender" JD.string)
      (JD.field "account_number" JD.int)


personView : Person -> Html a
personView person =
  div [class "panel panel-default"]
    [ div [class "panel-body"]
      [ h2 [] [text person.name]
      , p [] [b [] [text "Email: "], text person.email]
      , p [] [b [] [text "Age: "], text <| toString person.age]
      , p [] [b [] [text "Balance: "], text <| toString person.balance]
      , p [] [b [] [text "Address: "], text person.address]
      ]
    ]

