module TdsTfmMonitor exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Http
import Task
import Json.Decode as Decode exposing (Decoder, (:=))
import Array exposing (..)
import Time exposing (..)
import Date exposing (..)
import Dict exposing (..)

type alias JobTiming = 
  { id: String
  , durationSecs: Float
  }
 
main =
    App.program
        { init = init 0
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



type alias Model =
    { count : Int
    , jobHistory : List JobTiming
    , lastError : String
    }


init : Int -> ( Model, Cmd Msg )
init count =
    ( Model count [{id = "1235", durationSecs = 5.2}, {id="1234", durationSecs=8.1}] "NONE"
    , Cmd.none
    )

type Msg
    = Increment
    | Decrement
    | GetLatestJobs
    | FetchLatestJobsFail Http.Error
    | FetchLatestJobsSuccess Jobs

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )
        GetLatestJobs ->
            ( { model | lastError = "NONE"}, getLatestJobs )
        FetchLatestJobsFail error ->
            ( {model | lastError = (toString error)}, Cmd.none )
        FetchLatestJobsSuccess jobs ->
            ( {model | jobHistory = updateHistory jobs []}, Cmd.none ) 

updateHistory jobs previousTimings = 
  case jobs of 
    [] -> previousTimings
    first :: rest -> updateHistory (List.drop 1 jobs) ({ id=first.id
                                                       , durationSecs = (computeDuration first)}
 ::previousTimings)

computeDuration job =
  let
    startTime = (toTimeSecs job.startTimeString)
    endTime = (toTimeSecs job.finishTimeString)
    dt = endTime - startTime
  in
    Time.inSeconds dt
 
toTimeSecs string =
  let
    aDate = Date.fromString string
  in
    case aDate of
      Ok value ->
        Date.toTime value
      Err err ->
        0

view : Model -> Html Msg
view model =
    div []
        ((h3 [] [text model.lastError]) :: ((button [onClick GetLatestJobs][text "Poll for Job Updates"]) :: (List.intersperse (p [style[("line-height","80px")]][br[][]]) (renderJobHistory model))))


renderJobHistory model = 
  let 
    sortedHistory = List.sortBy .durationSecs model.jobHistory
    maxDuration = getMax (List.reverse sortedHistory)
    scaledHistory = List.map (\h -> scale h maxDuration) sortedHistory
    inReverseOrderScaledHistory = (List.reverse (List.sortBy .id scaledHistory))
  in
    List.map (\h -> renderJobTiming h) inReverseOrderScaledHistory

scale jobTiming maxDuration = 
  { 
    id = jobTiming.id
  , widthPercent = (Basics.round (jobTiming.durationSecs/maxDuration*100.0))
  , durationSecs = jobTiming.durationSecs
  }
  
renderJobTiming h = 
  let
    widthString = (toString h.widthPercent) ++ "%"
  in
    div [][ 
             div [style [
                 ("float","left")
               , ("border-style","solid")
               , ("border-color","white") 
               , ("height", "80px")
               , ("color","white")
               , ("font-family", "sans-serif")
               , ("background-color","blue")
               , ("width",widthString)
               , ("vertical-align", "middle")]] [text (h.id)]
           , div [style [("float","left")
                        ,("height","80px")
                        ,("line-height","80px")
                        ,("font-family", "sans-serif")
                        ,("vertical-align", "middle")]] [text (toString h.durationSecs)]
           ] 

getMax sorted = 
  case sorted of
    [] -> 0.0
    first :: rest -> (first.durationSecs*1.1)
    
    
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- basic idea
-- query for latest jobs (select * from tdstfm.forecast_job order by issue_time desc)
-- present a summary for the job : ID, start, end,  duration as text
-- show a "bar" showing the total duration
-- show a "bar" showing the steps within the job, scaled appropriately
-- see "name" of each step
-- ability to "sort" steps based on duration vs. order 

getLatestJobs =
  let 
    url = "http://localhost:3008/api/job/all"
  in 
    Task.perform FetchLatestJobsFail FetchLatestJobsSuccess (Http.get decodeJobs url)

decodeJobs = 
  Decode.object1 identity
  ("jobs" := Decode.list decodeJob)

decodeJob = 
  Decode.object3 Job
    ("id" := Decode.string)
    ("start_processing_timestamp" := Decode.string)
    ("finish_processing_timestamp" := Decode.string)

type alias Jobs = List Job

type alias Job = 
  { id : String
  , startTimeString : String
  , finishTimeString : String
  }
