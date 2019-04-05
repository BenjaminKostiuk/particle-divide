import Browser
import Browser.Navigation exposing (Key(..))
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Url
import List
import Random

type Msg = Tick Float GetKeyState
         | MakeRequest Browser.UrlRequest
         | UrlChange Url.Url
         | GenerateParticles (List ((Float, Float), (Float, Float)))  -- Generate a list of random particles
         | OnBtn Int        -- Hover over button
         | OffBtn Int       -- Stop hovering a button
         | LoadLevel Int    -- Go to the next level
         | GoToMainMenu     -- Go to Main Menu
         | ChangeColor      -- Change the particle color
         | GetMousePos (Float, Float)   -- Get the current mouse position
         | AddClick (Float, Float)      -- Add a click to the list of clicks during the level
-- Model
type alias Model = { particles : List Particle, particleColor : Color, 
                    pos : (Float, Float), clicks : List (Float, Float), 
                    -- Information for levels
                    level : Int, speed : Float, quantity : Int, margin : Int,
                    -- Transparency levels for buttons (for onHover)
                    transBtn1 : Float, transBtn2 : Float,
                    -- Keep track of user position
                    mainMenu : Bool, levelEnd : Bool, activeLevel : Bool }

-- Particle (PosX, PosY) (dX, dY)
type Particle = Particle (Float, Float) (Float, Float)

init : () -> Url.Url -> Key -> ( Model, Cmd Msg )
init flags url key = ( { particles = [], particleColor = lightBlue, quantity = 200, speed = 1.3, 
                        pos = (0, 0), clicks = [], level = 0, margin = 21,
                        transBtn1 = 0.7, transBtn2 = 0.7,
                        mainMenu = True, levelEnd = False, activeLevel = False } , Random.generate GenerateParticles (genParticles 15 2.5 "centered"))

-- Generate n number of particles moving at a speed
genParticles : Int -> Float -> String -> Random.Generator (List ((Float, Float), (Float, Float)))
genParticles n speed option = Random.list n 
    ((Random.map3 
        (\dx sy x ->
            let 
                -- Compute dy direction
                dy = sqrt(speed^2 - dx^2)
                posX = if option == "centered" then 0 else x
                posY = if option == "centered" then 0 else sqrt(15^2 - x^2)
                --return the particle with given coordinates and directions
            in case sy of
                    0 -> ((posX,posY),(dx,dy))
                    _ -> ((posX,posY),(dx,-dy)))
          -- Generate a dx and position sign for dx and dy
         (Random.float -speed speed) (Random.int 0 1) (Random.float -15 15)))
        
-- Update the position of all particles in model.particules
updateParticles : Particle -> Particle
updateParticles particle = 
    case particle of
        Particle (x, y) (dx, dy) ->
            if x < -375 then Particle (375, y + dy) (dx, dy) 
            else if x > 375 then Particle (-375, y + dy) (dx, dy)
            else if y < -250 then Particle (x + dx, 250) (dx, dy) 
            else if y > 250 then Particle (x + dx, -250) (dx, dy) 
            else Particle (x + dx, y + dy) (dx, dy)

-- Change the color of the particles
changeModelColor : Model -> Model
changeModelColor model =
    let
        -- List of all possible colors
        colors = [blue, green, red, purple, orange, pink, lightBlue, 
                    lightGreen, lightOrange, lightPurple, lightRed]
        -- Function to swap to the next color in the list
        swap : List Color -> Color
        swap allColors = case List.head allColors of 
            Just color -> if color == model.particleColor then 
                case List.head (List.drop 1 allColors) of  
                    Just nextColor -> nextColor
                    Nothing -> blue
                else swap (List.drop 1 allColors)
            Nothing -> blue
    in { model | particleColor = (swap colors)}

-- Get the slope (m) and base (b) of the line as a y = mx + b function
getLineEquation : Model -> (Float, Float)
getLineEquation model = 
    case List.head (model.clicks) of
        Just (x,y) -> case List.head (List.drop 1 model.clicks) of 
                            Just (w,z) -> ((z-y) / (w-x), y - ((z-y) / (w-x)) * x)
                            Nothing -> (0,0)
        Nothing -> (0,0)

-- Calculate the score after the user has drawn a line,
--  returns (Percentage Error, (#Particles above line, #Particles below line))
calculateScore : Model -> (Int, (Int, Int))
calculateScore model = 
    let
        -- Get the equation of the line drawn
        (slope, b) = getLineEquation model
        -- Get the number of particles above and below the line
        particlesAbove = List.filter (\(Particle (x,y) (dx, dy)) -> y > x * slope + b) model.particles |> List.length
        particlesBelow = List.filter (\(Particle (x,y) (dx, dy)) -> y < x * slope + b) model.particles |> List.length
        -- Calculate the error between the particles above and below the line
        error = toFloat (abs (particlesAbove - particlesBelow)) / toFloat (List.length (model.particles)) * 100 |> round
    in (error, (particlesAbove, particlesBelow))

-- Update function
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
   case msg of
        Tick time getKeyState ->
           ( { model | particles = List.map updateParticles model.particles }, Cmd.none )

        MakeRequest req ->
           ( model, Cmd.none )

        UrlChange url ->
           ( model, Cmd.none )

        GenerateParticles coordinates ->
            ( { model | particles = List.map (\((x,y),(dx, dy)) -> Particle (x, y) (dx, dy)) coordinates }, Cmd.none )
        -- On Hover
        OnBtn id ->
            ( { model | transBtn1 = if id == 1 then 1.0 else model.transBtn1, 
                        transBtn2 = if id == 2 then 1.0 else model.transBtn2 }, Cmd.none)
        -- Off Hover
        OffBtn id ->
            ( { model | transBtn1 = if id == 1 then 0.7 else model.transBtn1,
                        transBtn2 = if id == 2 then 0.7 else model.transBtn2 }, Cmd.none)
        -- Load the next level given an increment -> 1 for next level, 0 to try again
        LoadLevel increment -> 
            ( { model | mainMenu = False, levelEnd = False, activeLevel = True, 
            transBtn1 = 0.7, transBtn2 = 0.7, level = model.level + increment,
            quantity = let
                            n = round (400 / toFloat (model.level + 1) - 4)
                        in (if modBy 2 n == 0 then n else n + 1),
            speed = if increment == 1 then 1.5 + 0.2 * toFloat model.level else model.speed, 
            margin = if increment == 1 then (if model.margin <= 0 then 0 else model.margin - 1) else model.margin,
            clicks = [] },
            Random.generate GenerateParticles (genParticles model.quantity model.speed "random"))
        
        GoToMainMenu ->
            ( { model | mainMenu = True, levelEnd = False, activeLevel = False, level = 0,
            speed = 1.3, margin = 21, quantity = 200,
            transBtn1 = 0.7, transBtn2 = 0.7, clicks = [] } , Random.generate GenerateParticles (genParticles 15 2.5 "centered"))

        ChangeColor ->
            ( changeModelColor model , Cmd.none )

        GetMousePos position ->
            ( { model | pos = position }, Cmd.none)
        
        AddClick click ->
            ( { model | clicks = model.clicks ++ [click],
            activeLevel = if List.length (model.clicks) >= 1 then False else model.activeLevel,
            levelEnd = if List.length (model.clicks) >= 1 then True else model.levelEnd,
            particles = if List.length (model.clicks) >= 1 then
                -- Stop all particle mouvement once line is draw
                List.map (\(Particle (x,y) (dx,dy)) -> Particle (x,y) (0,0)) model.particles else model.particles }, Cmd.none)

-- Update the collage to draw the updated positions of the particles
viewParticles : List Particle -> Color -> Float -> List (Shape Msg)
viewParticles particles color size = List.map (\(Particle (x,y) (dx, dy)) -> circle size |> filled color |> move (x, y)) particles

-- Create a standard button Shape with a custom template
standardBtn : String -> Float -> Shape Msg
standardBtn content transparency = union (roundedRect 100 30 2.5
        |> filled lightBlue |> makeTransparent transparency)
        (text content
        |> customFont "Trebuchet MS" |> size 12
        |> bold |> centered
        |> filled white |> move (0,-4)
        |> makeTransparent transparency)

-- Create a stantard text Shape with a custom template
standardText : String -> Float -> Color -> Float -> Shape Msg
standardText message fontSize textColor yPos = text message
        |> customFont "Trebuchet MS" 
        |> size fontSize
        |> centered
        |> filled textColor
        |> move(0,yPos)

-- Draw all parts of the line draw
drawSeperator : Model -> List (Shape Msg)
drawSeperator model = if List.length(model.clicks) < 2 then
    -- If there are less than two clicks to draw a completed line
    [ circle 2
        |> filled black
        |> move model.pos ] ++ case List.head (model.clicks) of
            Just click -> [line click model.pos |> outlined (solid 2) black ]
            Nothing -> []
    else
        -- Draw a extended line that spans the screen from two seperate mouse clicks
        let
            (slope, b) = getLineEquation model
        in case List.head (model.clicks) of
            Just (x,y) -> case List.head (List.drop 1 model.clicks) of 
                            Just (w,z) -> [line (-375, -375 * slope + b) (375, 375 * slope + b) |> outlined (solid 2) black]
                            Nothing -> []
            Nothing -> []

-- Create the button and text Shape after completing a level
levelEndShapes : Model -> Shape Msg
levelEndShapes model = 
    case calculateScore model of
        (percent, (above, below)) -> union
            -- Create the text displaying results according to error percentage
            (standardText 
                ("You were " ++ (String.fromInt percent) ++ "% off"
                    ++ (if percent > model.margin then ", you need to be at most " ++ (String.fromInt model.margin) ++ "% off." else "")) 25 black 100)
            (standardText ("Ratio : " ++ (String.fromInt above) ++ " - " ++ (String.fromInt below)) 20 blue 75)
            |> union (standardText ("Level " ++ String.fromInt (model.level) ++ (if percent <= model.margin then " Complete!" else " Failed!")) 30
                (if percent <= model.margin then green else red ) 130)
            |> union
                -- Create a button for "try again" or "next level" 
                ((if percent <= model.margin then
                (standardBtn "Next Level" model.transBtn1)
                |> notifyTap (LoadLevel 1)
                else
                (standardBtn "Try Again" model.transBtn1)
                |> notifyTap (LoadLevel 0))
                |> move (-60,30)
                |> notifyEnter (OnBtn 1)
                |> notifyLeave (OffBtn 1))

-- View function
view : Model -> { title : String, body : Collage Msg }
view model = 
  let 
    title = "ParticleDivide"
    -- Message for main menu
    mainMenuMessage = union (standardText "Welcome to Particle Divide!" 25 black 100)
        (standardText "Created by Benjamin Kostiuk" 13 black 75)
    -- Buttons for main menu
    mainMenuButtons = union ((standardBtn "Start Game" model.transBtn1)
        |> move (-60,30)
        |> notifyEnter (OnBtn 1)
        |> notifyLeave (OffBtn 1)
        |> notifyTap (LoadLevel 1))
        ((standardBtn "Change Color" model.transBtn2)
        |> move (60,30)
        |> notifyEnter (OnBtn 2)
        |> notifyLeave (OffBtn 2)
        |> notifyTap ChangeColor)
    -- Buttons and message for end of level
    levelEndBtnAndMsg = levelEndShapes model
    -- Button for back to main menu
    goToMainMenuButton = (standardBtn "Main Menu" model.transBtn2)
        |> move (60,30)
        |> notifyEnter (OnBtn 2)
        |> notifyLeave (OffBtn 2)
        |> notifyTap GoToMainMenu
    -- Transparent backdrop
    backdrop = rect 750 500
        |> filled white
        |> makeTransparent 0
        |> notifyMouseMoveAt (\(x,y) -> GetMousePos (x,y))
        |> notifyMouseDownAt (\(x,y) -> AddClick (x,y))

    body  = collage 750 500
        ((viewParticles model.particles model.particleColor 4) ++
        (if model.mainMenu then [mainMenuMessage, mainMenuButtons] else []) ++
        (if model.levelEnd then (drawSeperator model) ++ [levelEndBtnAndMsg, goToMainMenuButton] else []) ++
        (if model.activeLevel then (drawSeperator model) ++ [backdrop] else []))
        
  in { title = title, body = body }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

main : AppWithTick () Model Msg
main = appWithTick Tick
       { init = init
       , update = update
       , view = view
       , subscriptions = subscriptions
       , onUrlRequest = MakeRequest
       , onUrlChange = UrlChange
       } 