import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)


main = 
    -- App Entry point
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }

-- MODEL

type alias Model =
    -- keep track of all of our game states
    { frames: List Frame
    , frame_score: List FrameScore
    , game_score: Int
    , game_state: GameState
    , error: String
    , currentRoll: String
    , currentFrame: Frame
    }

init : Model
init = Model [] [] 0 InProgress "" "" New

type GameState = InProgress | Finished

-- All of permutations a bowling frame can take
type Frame = New
           | Half Int 
           | OpenFrame (Int, Int) 
           | Spare Int 
           | Strike 
           | FinalFrame (List Int)

{- 
Since some frame types rely on later frames to be able to calculate their
score, we need a type model that some frames haven't finished scoring.
-}
type FrameScore = Scored Int | Incomplete

-- UPDATE

type Msg = Roll | TypeIn String

-- update our game state when the user interacts with the app
update : Msg -> Model -> Model
update msg model =
    case msg of
        TypeIn str -> { model | currentRoll = str }
        Roll -> 
            case toInt model.currentRoll of
                Err err -> 
                    { model | error = "Invalid input", currentRoll = "" }
                Ok rollScore ->
                    updateFrames rollScore model |> detectFinalFrame |> updateScores

{- 
Update our list of frames and current frame when the form is submitted
-}
updateFrames : Int -> Model -> Model
updateFrames rollScore model =
    let 
        newFrame = updateFrame rollScore model.currentFrame
    in
        case newFrame of
            Err txt -> { model | error = txt, currentRoll = "" }
            Ok (Half fst) ->
                { model |
                    currentFrame = Half fst,
                    currentRoll = "",
                    error = "" }
            Ok (FinalFrame rolls) ->
                if isFinalFrameDone rolls then
                    resetModel { model | 
                        frames = model.frames ++ [ FinalFrame rolls ],
                        game_state = Finished }
                else
                    { model |
                        currentFrame = FinalFrame rolls,
                        currentRoll = "",
                        error = "" }
            Ok frame ->
                resetModel { model | frames = model.frames ++ [ frame ] }

{- Sanitizes the input and if the input is valid, returns a valid result of the roll -}
updateFrame : Int -> Frame -> Result String Frame
updateFrame roll frame =
    if (roll < 0) || (roll > 10) then
        Err "Invalid number of pins"
    else
        case frame of
            New -> 
                if roll == 10 then
                    Ok Strike
                else
                    Ok (Half roll)
            Half fst ->
                if fst + roll > 10 then
                    Err "Too many pins!"
                else if fst + roll == 10 then
                    Ok (Spare fst)
                else
                    Ok (OpenFrame (fst, roll))
            FinalFrame rolls ->
                handleFinalFrame roll rolls
            _ -> Ok New

resetModel : Model -> Model
resetModel model =
    { model |
        currentFrame = New,
        currentRoll = "",
        error = "" }

isFinalFrameDone : List Int -> Bool
isFinalFrameDone rolls =
    case rolls of
        [] -> False
        [x] -> False
        [x, y] ->
            if x + y >= 10 then
                False
            else 
                True
        _ -> True

-- Since the final frame in bowling has special rules, we handle that here
handleFinalFrame : Int -> List Int -> Result String Frame
handleFinalFrame roll rolls =
    case rolls of
        [] -> Ok <| FinalFrame [roll]
        [10] -> Ok <| FinalFrame [10, roll]
        x::[] -> 
            if x + roll > 10 then
                Err "Too many pins in the second roll"
            else
                Ok <| FinalFrame [x, roll]
        10::10::[] -> Ok <| FinalFrame [10, 10, roll]
        10::y::[] -> 
            if y + roll > 10 then
                Err "Too many pins in the third roll"
            else
                Ok <| FinalFrame [10, y, roll]
        _ -> Err "Invalid action"

-- Make sure if we're in the 10th frame, we're operating on a FinalFrame type
detectFinalFrame : Model -> Model
detectFinalFrame model =
    if (model.currentFrame == New) && (List.length model.frames >= 9) then
        { model | currentFrame = FinalFrame [] }
    else
        model

-- Update our score fields based on our frames
updateScores : Model -> Model
updateScores model =
    let 
        newFrameScores = scoreFrames (model.frames ++ [ model.currentFrame ])
    in
        { model |
            frame_score = newFrameScores,
            game_score = scoreGame newFrameScores }

-- go through our list of frames, and tabulate the scores based on the frame's type
scoreFrames : List Frame -> List FrameScore
scoreFrames frames =
    case frames of
        [] -> []
        Half x::xs -> Incomplete :: scoreFrames xs
        OpenFrame (fst, sec)::xs -> Scored (fst + sec) :: (scoreFrames xs)
        Spare _::xs -> scoreSpare (List.take 1 xs) :: scoreFrames xs
        Strike::xs -> scoreStrike (List.take 2 xs) :: scoreFrames xs
        (FinalFrame scores)::xs -> scoreFinal scores :: scoreFrames xs
        _::xs -> Incomplete :: scoreFrames xs

-- Apply the special rules of spares
scoreSpare : List Frame -> FrameScore
scoreSpare next =
    case next of
        [] -> Incomplete
        FinalFrame []::_ -> Incomplete
        Half x::_ -> Scored (10 + x)
        OpenFrame (x, _)::_ -> Scored (10 + x)
        Strike::_ -> Scored 20
        (FinalFrame (x::_))::_ -> Scored (10 + x)
        Spare x::_ -> Scored (10 + x)
        New::_ -> Incomplete

-- Apply the special rules of strikes
scoreStrike : List Frame -> FrameScore
scoreStrike nextTwo =
    case nextTwo of
        [] -> Incomplete
        New::_ -> Incomplete
        [Strike] -> Incomplete
        Strike::New::_ -> Incomplete
        Strike::(FinalFrame [])::_ -> Incomplete
        Half _::_ -> Incomplete
        (FinalFrame [])::_ -> Incomplete
        (FinalFrame [_])::_ -> Incomplete
        (FinalFrame (x::y::_))::_ -> Scored (10 + x + y)
        Spare x::_ -> Scored 20
        OpenFrame (x, y)::_ -> Scored (10 + x + y)
        Strike::Half x::_ -> Scored (20 + x)
        Strike::OpenFrame (x, _)::_ -> Scored (20 + x)
        Strike::Spare x::_ -> Scored (20 + x)
        Strike::FinalFrame (x::_)::_ -> Scored (20 + x)
        Strike::Strike::_ -> Scored 30

-- Apply the special rules of the final frame
scoreFinal : List Int -> FrameScore
scoreFinal scores =
    case scores of
        [] -> Incomplete
        [x] -> Incomplete
        [x, y] ->
            if (x + y) < 10 then
                Scored (x + y)
            else
                Incomplete
        x::y::z::_ -> Scored(x + y + z)

translateScore : FrameScore -> Int
translateScore score =
    case score of
        Scored num -> num
        Incomplete -> 0

scoreGame : List FrameScore -> Int
scoreGame scores =
    List.sum (List.map translateScore scores)

-- VIEW

-- base html template
view : Model -> Html Msg
view model = 
    div []
        [ div [ class "frame-score" ] 
            <| [ h4 [] [ text "Score Card" ] ] ++
            ( List.map render_frame (List.map2 (,) model.frames model.frame_score) )
        , div [ class "current-frame" ] 
            <| [ h4 [] [text "Current Frame" ] ] ++ (currentFrame model.currentFrame)
        , div [class "game-score"] [ text <| "Total Score: " ++ (toString model.game_score) ]
        , div [ class "add-roll" ] ( viewForm model )
        ]

render_frame : (Frame, FrameScore) -> Html Msg
render_frame (frame, score) =
    let 
        total = 
            case score of
                Incomplete -> "??"
                Scored num -> toString num

        breakdown = 
            case frame of 
                New -> ["?", "?"]
                Half rollOne -> [ toString rollOne, "?" ]
                OpenFrame (rollOne, rollTwo) -> [ toString rollOne, toString rollTwo ]
                Spare rollOne -> [ toString rollOne, "/" ]
                Strike -> [ "X", "/" ]
                FinalFrame rolls -> 
                    List.map toString rolls
    in 
        div [ class "frame" ]
            <| (List.map (\score -> div [class "frame-segment"] [ text score ]) breakdown) ++ 
            [ div [class "frame-score" ] [ text total ] ]

currentFrame frame =
    case frame of
        Half x -> [ div [] [ text <| toString x ] ]
        FinalFrame scores -> List.map (\x -> div [] [ text (toString x) ]) scores
        _ -> [ div [] [ text "0" ] ]

-- show the input field or Game Over if we've finished
viewForm : Model -> List (Html Msg)
viewForm model =
    case model.game_state of
        InProgress ->
            [ p [ class "error" ] [ text model.error ]
            , input [ type' "text", placeholder "How many pins?", value model.currentRoll, onInput TypeIn ] []
            , button [onClick Roll] [text "Go!" ] ]
        Finished ->
            [ p [ class "game-over" ] [ text "Game Over!" ] ]

