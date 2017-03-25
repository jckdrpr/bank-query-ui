module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD

import Person exposing (..)
import Utils exposing (..)


-- CONSTANTS (HAHAHAHAH) --

baseURL : String
baseURL = "http://haskell-es-example.sherubthakur.com/search"



-- MODEL --

type alias EsResult =
  Either String (List Person)


type State
  = InitialMsg
  | Typing
  | Searching
  | NetworkError String
  | ResponseError (String)
  | SuccessfulLoad (List Person)


type alias Model =
  { query : String
  , content : State
  }


initModel : Model
initModel = 
  { query = ""
  , content = InitialMsg
  }


init : ( Model, Cmd Msg )
init =
  ( initModel
  , Cmd.none
  )



-- UPDATE --

type Msg
  = ChangeQuery String
  | Search
  | GetResults (Result Http.Error (EsResult))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ChangeQuery newQuery ->
      ( {model | query = newQuery, content = Typing}, Cmd.none )

    Search ->
      ( {model | content = Searching}, query (model.query) )

    GetResults (Ok result) ->
      case result of 
        Right persons ->
          ( {model | content = SuccessfulLoad persons}, Cmd.none )

        Left error ->
          ( {model | content = ResponseError error}, Cmd.none )

    GetResults (Err _) ->
      ( {model | content = NetworkError "Network or Server Error"}, Cmd.none )


query : String -> Cmd Msg
query q =
  let
    url = baseURL ++ "?query=" ++ q
  in
    Http.send GetResults (Http.get url decodeSearchResponse)


decodeSearchResponse : JD.Decoder EsResult
decodeSearchResponse =
  JD.oneOf
    [ JD.map Left (JD.at ["error"] JD.string)
    , JD.map Right (JD.at ["results"] (JD.list personDecoder))
    ]



-- VIEW --

contentView : State -> Html Msg
contentView content =
  case content of
    InitialMsg ->
      let
        content : String
        content =
          String.join " "
            [ "This is the front-end for an example application which shows how one can"
            , "create a slack like query language for their own application."
            , "You can type in something like the following queries."
            ]

      in
        div []
          [ div [] [text content]
          , pre []
              [ ul [class "text-left"]
                [ li [] [text "gender:F"]
                , li [] [text "gender:F and age>21 and age<25"]
                , li [] [text "gender:F and (age>21 and age<25 or balance>45000)"]
                , li [] [text "..."]
                ]
              ]
          ]

    Typing ->
      div []
        [text "Type. Type. Type"]

    Searching ->
      div []
        [text "Loading"]
  
    NetworkError error ->
      div []
        [text error]

    ResponseError error ->
      div []
        [text error]

    SuccessfulLoad persons ->
      div [] (List.map personView persons)


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text "Bank Query ES"]
    , br [] []
    , input [type_ "text", name "search", placeholder "Search..", onInput ChangeQuery] []
    , button [type_ "button", onClick Search] [text "Search"]
    , br [] []
    , br [] []
    , contentView (model.content)
    ]



-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

