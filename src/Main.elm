module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (GetKeyState, gameApp)

type Event = Tick Float GetKeyState
         --Transitions between states
         | Caf2UpEng
         | Caf2Med
         | Med2UpEng
         | UpEng2LowEng
         | UpEng2Reac
         | UpEng2Sec
         | Reac2Sec
         | Sec2Reac
         | StartReacTask
         | EndReacTask
         | FailReacTask
         | Reac2LowEng
         | SecToLowEng
         | LowEng2Stor
         | LowEng2Elec
         | Elec2Stor
         | Stor2Adm
         | Stor2Caf
         | StartAdmTask
         | EndAdmTask
         | Adm2Caf

         --Card Swipe Minigame
         | CardTap
         | ChangeCardDragState
         | DragCard (Float, Float) --Just uses x-pos
         | DummyEvent

         --Leaves Minigame
         | DragLeaf1 (Float, Float)
         | SwitchStateForLeaf1 (Float, Float)
         | DragLeaf2 (Float, Float)
         | SwitchStateForLeaf2 (Float, Float)
         | DragLeaf3 (Float, Float)
         | SwitchStateForLeaf3 (Float, Float)
         | DragLeaf4 (Float, Float)
         | SwitchStateForLeaf4 (Float, Float)
         | DragLeaf5 (Float, Float)
         | SwitchStateForLeaf5 (Float, Float)
         | DragLeaf6 (Float, Float)
         | SwitchStateForLeaf6 (Float, Float)

--Different areas on the map
type State = Cafeteria 
           | MedBay 
           | UpperEngine 
           | Reactor 
           | LowerEngine 
           | Electrical 
           | Storage 
           | Admin 
           | Security 
           | Weapons 
           | Navigation 
           | Shields 
           | Communications 
           | O2 
           | ReactorTask 
           | AdminTask 

--Positions buttons can be placed on the screen
type ButtonPosition = Left
                    | Right
                    | Down
                    | Up
                    | LeftDown
                    | LeftUp
                    | RightDown
                    | RightUp
                    | UpLeft
                    | UpRight
                    | DownLeft
                    | DownRight
                    | Middle

--States for dragging objects
--Two dragging states are present as both minigames were implemented by separate group members
type DragState = Released --Nothing is being dragged around the screen
               | Dragging --The card in the Card Swipe minigame is being dragged
               | DraggingLeaf (Float, Float) --A leaf in the Vacuum Minigame is being dragged

--Constant definitions
cardWalletXPos = -35
cardSwipeStartXPos = -80
cardSwipeStartYPos = 0
cardSwipeFinalXPos = 85
cardSwipeFinalYPos = 78
cardSwipeStartMsg = "SWIPE CARD"
leavesVacuumX = -75

--Actual Code--

--Definitions for each state
myShapes model =
    case model.state of
        Cafeteria  ->
            [
               areaName "Cafeteria"
             , transitionButton Left Caf2UpEng "Upper Engine"
             , transitionButton LeftDown Caf2Med "Med Bay"
            ]
        MedBay  ->
            [
               areaName "Med Bay"
             , transitionButton LeftUp Med2UpEng "Upper Engine"
            ]
        UpperEngine  ->
            [
               areaName "Upper Engine"
             , transitionButton Down UpEng2LowEng "Lower Engine"
             , transitionButton LeftDown UpEng2Reac "Reactor"
             , transitionButton RightDown UpEng2Sec "Security"
            ]
        Reactor  ->
            [
               areaName "Reactor"
             , transitionButton Down Reac2LowEng "Lower Engine"
             , transitionButton Right Reac2Sec "Security"
             , transitionButton Middle StartReacTask "Clean Leaves"
            ]
        ReactorTask  ->
            [ square 200 --Background
                |> filled lightBlue
              ,
              vacuum
              ,

              --Leaf 1
                let (xpos,ypos) = model.leaf1Pos in
                  if xpos >= leavesVacuumX
                    then leaf |> move (xpos, 10 * sin(model.time) + ypos)  --Move leaf up and down
                                |> notifyMouseDownAt SwitchStateForLeaf1 
                  else group []
              ,
                case model.leaf1DragState of --Movement handler        
                    DraggingLeaf _ -> rect 185 120
                                        |> ghost 
                                        |> notifyMouseUpAt SwitchStateForLeaf1
                                        |> notifyMouseMoveAt DragLeaf1
                                        |> notifyLeaveAt SwitchStateForLeaf1 
                    _  -> group []
              ,
 
              --Leaf 2
                let (xpos,ypos) = model.leaf2Pos in
                  if xpos >= leavesVacuumX
                    then leaf |> move (xpos, 10 * sin(model.time) + ypos) --Move leaf up and down
                |> notifyMouseDownAt SwitchStateForLeaf2 
                  else group []
              ,
                case model.leaf2DragState of --Movement handler         
                    DraggingLeaf _ -> rect 185 120 
                                        |> ghost 
                                        |> notifyMouseUpAt SwitchStateForLeaf2
                                        |> notifyMouseMoveAt DragLeaf2
                                        |> notifyLeaveAt SwitchStateForLeaf2
                    _  -> group []
              ,
 
              --Leaf 3
                let (xpos,ypos) = model.leaf3Pos in
                  if xpos >= leavesVacuumX
                    then leaf |> move (xpos, 10 * sin(model.time) + ypos) --Move leaf up and down
                                |> notifyMouseDownAt SwitchStateForLeaf3
                  else group []
              ,
                case model.leaf3DragState of --Movement handler        
                    DraggingLeaf _ -> rect 185 120
                                        |> ghost 
                                        |> notifyMouseUpAt SwitchStateForLeaf3
                                        |> notifyMouseMoveAt DragLeaf3
                                        |> notifyLeaveAt SwitchStateForLeaf3
                    _  -> group []
              ,
 
              --Leaf 4
                let (xpos,ypos) = model.leaf4Pos in
                  if xpos >= leavesVacuumX
                then leaf |> move (xpos, 10 * sin(model.time) + ypos) --Move leaf up and down
                            |> notifyMouseDownAt SwitchStateForLeaf4
                  else group []
              ,
                case model.leaf4DragState of --Movement handler        
                    DraggingLeaf _ -> rect 185 120
                                        |> ghost 
                                        |> notifyMouseUpAt SwitchStateForLeaf4
                                        |> notifyMouseMoveAt DragLeaf4
                                        |> notifyLeaveAt SwitchStateForLeaf4
                    _  -> group []
              ,
 
                --Leaf 5
                let (xpos,ypos) = model.leaf5Pos in
                  if xpos >= leavesVacuumX
                    then leaf |> move (xpos, 10 * sin(model.time) + ypos) --Move leaf up and down
                                |> notifyMouseDownAt SwitchStateForLeaf5
                else group []
              ,
                case model.leaf5DragState of --Movement handler    
                    DraggingLeaf _ -> rect 185 120
                                        |> ghost 
                                        |> notifyMouseUpAt SwitchStateForLeaf5
                                        |> notifyMouseMoveAt DragLeaf5
                                        |> notifyLeaveAt SwitchStateForLeaf5
                    _  -> group []
              ,
 
                --Leaf 6
                let (xpos,ypos) = model.leaf6Pos in
                  if xpos >= leavesVacuumX
                    then leaf |> move (xpos,10*sin(model.time)+ypos) --Move leaf up and down
                                |> notifyMouseDownAt SwitchStateForLeaf6
                else group []
              ,
                case model.leaf6DragState of --Movement handler         
                    DraggingLeaf _ -> rect 185 120
                                        |> ghost 
                                        |> notifyMouseUpAt SwitchStateForLeaf6
                                        |> notifyMouseMoveAt DragLeaf6
                                        |> notifyLeaveAt SwitchStateForLeaf6
                    _  -> group []
              ,
 
                --Check if user exceeded time limit
                let
                    (leaf1x, leaf1y) = model.leaf1Pos
                    (leaf2x, leaf2y) = model.leaf2Pos
                    (leaf3x, leaf3y) = model.leaf3Pos
                    (leaf4x, leaf4y) = model.leaf4Pos
                    (leaf5x, leaf5y) = model.leaf5Pos
                    (leaf6x, leaf6y) = model.leaf6Pos
                in
                    --Check if time is exceeded and task has not been completed
                    if (model.time > 30 + model.leavesStartTime --Time exceeded
                    && (leaf1x >= leavesVacuumX --At least one leaf isn't in
                    || leaf2x >= leavesVacuumX
                    || leaf3x >= leavesVacuumX
                    || leaf4x >= leavesVacuumX
                    || leaf5x >= leavesVacuumX
                    || leaf6x >= leavesVacuumX)) then
                        text "Task Failed!" --Print failure text on the screen
                          |> centered
                          |> filled red
                    else
                        group []
              ,

                --Let the user redo the task
                let
                    (leaf1x, leaf1y) = model.leaf1Pos
                    (leaf2x, leaf2y) = model.leaf2Pos
                    (leaf3x, leaf3y) = model.leaf3Pos
                    (leaf4x, leaf4y) = model.leaf4Pos
                    (leaf5x, leaf5y) = model.leaf5Pos
                    (leaf6x, leaf6y) = model.leaf6Pos
                in
                    if (model.time > 31 + model.leavesStartTime --Time exceeded
                    && (leaf1x >= leavesVacuumX --At least one leaf isn't in
                    || leaf2x >= leavesVacuumX
                    || leaf3x >= leavesVacuumX
                    || leaf4x >= leavesVacuumX
                    || leaf5x >= leavesVacuumX
                    || leaf6x >= leavesVacuumX)) then
                      square 500 --Invisible box for clicking to restart the task
                        |> ghost
                        |> notifyTap FailReacTask
                    else
                        group []
              ,

                --Check if task successfully completed
                let
                    (leaf1x,leaf1y) = model.leaf1Pos
                    (leaf2x,leaf2y) = model.leaf2Pos
                    (leaf3x,leaf3y) = model.leaf3Pos
                    (leaf4x,leaf4y) = model.leaf4Pos
                    (leaf5x,leaf5y) = model.leaf5Pos
                    (leaf6x,leaf6y) = model.leaf6Pos
                in
                    -- Check if all the leaves are sucked into the vent
                    if (leaf1x <= leavesVacuumX
                    && leaf2x <= leavesVacuumX
                    && leaf3x <= leavesVacuumX
                    && leaf4x <= leavesVacuumX
                    && leaf5x <= leavesVacuumX
                    && leaf6x <= leavesVacuumX) then
                        group
                        [
                           button "Reactor" EndReacTask
                             |> move (0, -40)
                         , text "Task Complete!"
                            |> centered
                            |> filled black
                        ]
                     else
                         group []
              ]
        Security  ->
            [
               areaName "Security"
             , transitionButton Down SecToLowEng "Lower Engine"
             , transitionButton Left Sec2Reac "Reactor"
            ]
        LowerEngine  ->
            [
               areaName "Lower Engine"
             , transitionButton RightUp LowEng2Elec "Electrical"
             , transitionButton Right LowEng2Stor "Storage"
            ]
        Electrical  ->
            [
               areaName "Electrical"
             , transitionButton RightDown Elec2Stor "Storage"
            ]
        Storage  ->
            [ 
               areaName "Storage"
             , transitionButton Up Stor2Caf "Cafeteria"
             , transitionButton UpRight Stor2Adm "Admin"
            ]
        Admin  ->
            [ 
               areaName "Admin"
             , transitionButton UpLeft Adm2Caf "Cafeteria"
             , transitionButton Middle StartAdmTask "Swipe Card"
            ]
        AdminTask  ->
            [
               machine model.cardMessage --Display the machine with a specific message
             , wallet model.cardXPos model.cardYPos model.cardMessage --Wallet and card go on top of machine
             --, text (Debug.toString model.cardXPos) --For debugging
             , case model.dragState of
                   Dragging ->
                       rect 185 125 --Hidden box to handle the mouse movement
                         |> ghost --Invisible
                         |> notifyMouseMoveAt DragCard
                         |> notifyLeave ChangeCardDragState
                         |> notifyMouseUp ChangeCardDragState
                   _ ->
                       group []
            ]

        --Unused Locations
        Weapons  -> [areaName "Weapons"]
        Navigation  -> [areaName "Navigation"]
        Shields  -> [areaName "Shields"]
        Communications -> [areaName "Communications"]
        O2  -> [areaName "O2"]

--A macro for the name of an area printed in the centre of the screen
areaName label =
  group
  [
    text label
      |> centered
      |> filled black
      |> scale 0.8
  ]

--Defines the event functions for each button press
--"otherwise" is the "default" case
update event model =
    case event of
        Tick t _ -> --Runs every frame
            if (model.state == AdminTask
            && model.cardMessage == "APPROVED"
            && model.time - model.cardApprovedTimeout > 1) then --One second since the Admin Task was cleared
                update EndAdmTask { model | time = t} --Leave the admin task
            else
                { model | time = t}
        Caf2UpEng ->
            case model.state of
                Cafeteria  ->
                    { model | state = UpperEngine  }
                otherwise ->
                    model
        Caf2Med ->
            case model.state of
                Cafeteria  ->
                    { model | state = MedBay  }
                otherwise ->
                    model
        Med2UpEng ->
            case model.state of
                MedBay  ->
                    { model | state = UpperEngine  }
                otherwise ->
                    model
        UpEng2LowEng ->
            case model.state of
                UpperEngine  ->
                    { model | state = LowerEngine  }
                otherwise ->
                    model
        UpEng2Reac ->
            case model.state of
                UpperEngine  ->
                    { model | state = Reactor  }
                otherwise ->
                    model
        UpEng2Sec ->
            case model.state of
                UpperEngine  ->
                    { model | state = Security  }
                otherwise ->
                    model
        Reac2Sec ->
            case model.state of
                Reactor  ->
                    { model | state = Security  }
                otherwise ->
                    model
        Sec2Reac ->
            case model.state of
                Security  ->
                    { model | state = Reactor  }
                otherwise ->
                    model
        StartReacTask ->
            case model.state of
                Reactor  ->
                    { model |   state = ReactorTask
                              , leavesStartTime = model.time
                                
                                --Initialize positions and drag states of each leaf
                              , leaf1Pos = (-10,-25)
                              , leaf1DragState = Released
                              , leaf2Pos = (45,25)
                              , leaf2DragState = Released
                              , leaf3Pos = (55,-25)
                              , leaf3DragState = Released
                              , leaf4Pos = (-35,35)
                              , leaf4DragState = Released
                              , leaf5Pos = (0,10)
                              , leaf5DragState = Released
                              , leaf6Pos = (-55,0)
                              , leaf6DragState = Released
                    }
                otherwise ->
                    model
        EndReacTask ->
            case model.state of
                ReactorTask  ->
                    { model | state = Reactor  }
                otherwise ->
                    model
        Reac2LowEng ->
            case model.state of
                Reactor  ->
                    { model | state = LowerEngine  }
                otherwise ->
                    model
        SecToLowEng ->
            case model.state of
                Security  ->
                    { model | state = LowerEngine  }
                otherwise ->
                    model
        LowEng2Stor ->
            case model.state of
                LowerEngine  ->
                    { model | state = Storage  }
                otherwise ->
                    model
        LowEng2Elec ->
            case model.state of
                LowerEngine  ->
                    { model | state = Electrical  }
                otherwise ->
                    model
        Elec2Stor ->
            case model.state of
                Electrical  ->
                    { model | state = Storage  }
                otherwise ->
                    model
        Stor2Adm ->
            case model.state of
                Storage  ->
                    { model | state = Admin  }
                otherwise ->
                    model
        Stor2Caf ->
            case model.state of
                Storage  ->
                    { model | state = Cafeteria  }
                otherwise ->
                    model
        StartAdmTask ->
            case model.state of
                Admin  ->
                    { model |
                     state = AdminTask
                     
                     --Initialize variables for Card Swipe Minigame
                    , dragState = Released
                    , cardXPos = cardWalletXPos
                    , cardYPos = cardSwipeStartYPos
                    , cardMessage = cardSwipeStartMsg
                    , cardSwipeStartTime = 0
                    , changeCardStateTracker = 0
                    , cardApprovedTimeout = 0
                   }
                otherwise ->
                    model
        EndAdmTask ->
            case model.state of
                AdminTask  ->
                    { model | state = Admin }
                otherwise ->
                    model
        Adm2Caf ->
            case model.state of
                Admin  ->
                    { model | state = Cafeteria  }
                otherwise ->
                    model
        
        --Events for Minigames
        --Card Swipe
        CardTap -> --Tapping on the card moves it to swiping position
            { model |
              cardYPos = cardSwipeFinalYPos
            , cardXPos = cardSwipeStartXPos
            }

        ChangeCardDragState ->
            case model.changeCardStateTracker of --Handle function like a state machine
                0 ->
                    update event --Recursively call to process again after tracker change
                    { model |

                      --Change the current handler for dragging
                      dragState =
                          case model.dragState of
                              Released -> Dragging --Started dragging
                              Dragging -> Released --Finished dragging
                              _ -> model.dragState
                    
                      --Move to next state
                    , changeCardStateTracker = 1
                    }

                1 ->
                    case model.dragState of
                        Dragging -> --Started dragging
                            update event --Recursively call to process again after tracker change
                            { model |
                              cardSwipeStartTime = model.time --Just started to drag so update with current time
                            , cardMessage = cardSwipeStartMsg --Reload message on display
                            , changeCardStateTracker = 2 --Move to next state
                            }
                        Released -> --Finished dragging
                            update event
                            { model |

                              --Update the message on the machine based on result
                              cardMessage =
                                   if (model.cardXPos < cardSwipeFinalXPos) then
                                       "BAD READ"
                                   else if (model.time - model.cardSwipeStartTime < 1) then --Took less than a second to swipe
                                       "TOO FAST. TRY AGAIN."
                                   else if (model.time - model.cardSwipeStartTime > 2) then --Took more than two seconds to swipe
                                       "TOO SLOW. TRY AGAIN."
                                   else
                                       "APPROVED"

                              --Move to next state
                            , changeCardStateTracker = 2
                            }
                        _ -> model
 
                2 ->
                    case model.dragState of
                        Dragging -> --Started dragging
                            { model | changeCardStateTracker = 0 } --Reset tracker for future calls
                        Released -> --Finished dragging
                            update event
                            { model |
                              --Reset the card's base position if the minigame was a failure
                              cardXPos =
                                  if (model.cardMessage == "APPROVED") then --Swipe failure
                                      model.cardXPos --Leave in success position
                                  else
                                      cardSwipeStartXPos --Return to original pos

                             --Return to Admin if the minigame is cleared
                            , changeCardStateTracker =
                                  if (model.cardMessage == "APPROVED") then
                                      4 --Return to Admin
                                  else
                                      3 --Prepare to reset tracker for future calls
                            }
                        _ -> model
                3 ->
                    { model | changeCardStateTracker = 0 } --Reset tracker for future calls

                4 ->
                    { model |
                      cardApprovedTimeout = model.time --Start time of final message being shown
                    , changeCardStateTracker = 0
                    }

                _ -> model

        DragCard (x, y) -> { model |
                             cardXPos =
                                 case model.dragState of
                                     Dragging -> --Only works while dragging
                                         x
                                     Released -> --Not dragging
                                         model.cardXPos --Don't change anything
                                     _ ->
                                         model.cardXPos
                           }

        DummyEvent -> model --An event that does absolutely nothing

    --Leaves Minigame
        --Drag leaf to appropriate position
        DragLeaf1 (x, y) ->  
          case model.leaf1DragState of 
            DraggingLeaf (xDelta, yDelta) 
              -> { model | leaf1Pos = (x + xDelta, y + yDelta) }
            _
              -> model
        --Switch the state of each leaf from released to dragging
        SwitchStateForLeaf1 (x, y) -> 
           { model | leaf1DragState = 
             case model.leaf1DragState of
               Released ->
                 let (xPos, yPos) = model.leaf1Pos
                   in DraggingLeaf (xPos - x, yPos - y)
               DraggingLeaf _ -> Released
               _ -> Released
           }

        --Drag leaf to appropriate position
        DragLeaf2 (x, y) ->  
          case model.leaf2DragState of 
            DraggingLeaf (xDelta, yDelta) 
              -> { model | leaf2Pos = (x + xDelta, y + yDelta) }
            _
              -> model
        --Switch the state of each leaf from released to dragging
        SwitchStateForLeaf2 (x, y) ->  
           { model | leaf2DragState = 
             case model.leaf2DragState of
               Released ->
                 let (xPos, yPos) = model.leaf2Pos
                   in DraggingLeaf (xPos - x, yPos - y)
               DraggingLeaf _ -> Released
               _ -> Released
           }

        --Drag leaf to appropriate position
        DragLeaf3 (x, y) -> 
          case model.leaf3DragState of 
            DraggingLeaf (xDelta, yDelta) 
              -> { model | leaf3Pos = (x + xDelta, y + yDelta) }
            _
              -> model
        --Switch the state of each leaf from released to dragging
        SwitchStateForLeaf3 (x, y) ->  
           { model | leaf3DragState = 
             case model.leaf3DragState of
               Released         ->
                 let (xPos, yPos) = model.leaf3Pos
                   in DraggingLeaf (xPos - x, yPos - y)
               DraggingLeaf _      -> Released
               _ -> Released
           }

        --Drag leaf to appropriate position
        DragLeaf4 (x, y) -> 
          case model.leaf4DragState of 
            DraggingLeaf (xDelta, yDelta) 
              -> { model | leaf4Pos = (x + xDelta, y + yDelta) }
            _
              -> model
        --Switch the state of each leaf from released to dragging
        SwitchStateForLeaf4 (x, y) ->   
           { model | leaf4DragState = 
             case model.leaf4DragState of
               Released ->
                 let (xPos, yPos) = model.leaf4Pos
                   in DraggingLeaf (xPos - x, yPos - y)
               DraggingLeaf _ -> Released
               _ -> Released
           }
 
        --Drag leaf to appropriate position
        DragLeaf5 (x, y) -> 
          case model.leaf5DragState of 
            DraggingLeaf (xDelta, yDelta) 
              -> { model | leaf5Pos = (x + xDelta, y + yDelta) }
            _
              -> model
        --Switch the state of each leaf from released to dragging
        SwitchStateForLeaf5 (x, y) -> 
           { model | leaf5DragState = 
             case model.leaf5DragState of
               Released ->
                 let (xPos, yPos) = model.leaf5Pos
                   in DraggingLeaf (xPos - x, yPos - y)
               DraggingLeaf _ -> Released
               _ -> Released
           }
 
        --Drag leaf to appropriate position
        DragLeaf6 (x, y) -> 
          case model.leaf6DragState of 
            DraggingLeaf (xDelta, yDelta) 
              -> { model | leaf6Pos = (x + xDelta, y + yDelta) }
            _
              -> model
        --Switch the state of each leaf from released to dragging
        SwitchStateForLeaf6 (x, y) ->  
           { model | leaf6DragState = 
             case model.leaf6DragState of
               Released ->
                 let (xPos, yPos) = model.leaf6Pos
                   in DraggingLeaf (xPos - x, yPos - y)
               DraggingLeaf _ -> Released
               _ -> Released
           }

        --In case of task failure, reset state and timer to redo the task
        FailReacTask ->
            case model.state of
                ReactorTask ->
                    { init |   time = 0 
                             , state = ReactorTask
                             , leavesStartTime = model.time
                    }
                otherwise ->
                    model

--Macro for individual button shapes
button label event =
  group
  [
     roundedRect 50 15 2 
       |> filled green                       
   , text label
       |> filled black
       |> scale 0.6
       |> move (-20, -5)
  ]
    |> notifyTap event

--Assign each kind of button a position on the screen
transitionButton : ButtonPosition -> Event -> String -> Shape Event
transitionButton pos event label =
    case pos of 
        Left ->      button label event
                       |> move (-60, 0)

        Right ->     button label event
                       |> move (60, 0)

        Up ->        button label event
                       |> move (0, 50)

        Down ->      button label event
                       |> move (0, -50)

        LeftDown ->  button label event
                       |> move (-60, -30)

        LeftUp ->    button label event
                       |> move (-60, 30)

        RightDown -> button label event
                       |> move (60, -30)

        RightUp ->   button label event
                       |> move (60, 30)

        UpLeft ->    button label event
                       |> move (-60, 50)

        UpRight ->   button label event
                       |> move (60, 50)

        DownLeft ->  button label event
                       |> move (-60, -50)

        DownRight -> button label event
                       |> move (60, -50)

        Middle ->    button label event
                       |> move (0, 20)

--Code for the Card Minigame--

--The card itself
card colour xPos yPos message =
  group
  [
    roundedRect 30 55 5
      |> filled lightBlue
      |> rotate (degrees 90)
      |> move (0, -45)
      
  , roundedRect 25 20 5
      |> filled white
      |> rotate (degrees 90)
      |> move (-15, -45)
      
  , oval 15 20 
      |> filled orange
      |> move (-15, -45)
      
  , roundedRect 15 15 3
      |> filled orange
      |> move (-15, -50)
      
  , roundedRect 5 10 1
      |> filled blue
      |> makeTransparent 0.7
      |> rotate (degrees 90)
      |> move (-16, -45)
      
  , roundedRect 15 1 1
      |> filled black
      |> move (4, -35)
      
  , roundedRect 5 1 1
      |> filled black
      |> move (19, -35)
      
  , roundedRect 5 1 1
      |> filled black
      |> move (-1, -40)
  
  , roundedRect 8 1 1
      |> filled black
      |> move (7, -40)
      
  , roundedRect 8 1 1
      |> filled black
      |> move (17, -40)

  , roundedRect 20 1 1
      |> filled black
      |> move (11, -45)
  ]
    |> move (xPos, yPos)
    |> notifyMouseDown
    (
      if (yPos /= cardSwipeFinalYPos) then --Code can't be dragged while it's in the wallet
          DummyEvent
      else if (message /= "APPROVED") then
          ChangeCardDragState --Start dragging the card
      else
          DummyEvent --Nothing happens after the game is completed
    )

--The wallet
wallet cardXPos cardYPos message = 
  group 
  [ roundedRect 50 80 5
      |> filled darkBrown
      |> rotate (degrees -80)
      |> move (-33.5, -40)
      
  , roundedRect 50 80 5
      |> filled darkBrown
      |> rotate (degrees 80)
      |> move (30, -40)
      
  , roundedRect 15 55 5
      |> filled darkRed
      |> rotate (degrees -75)
      |> move (-38, -30)
      
  , rect 10 58 
      |> filled darkBrown
      |> rotate (degrees -80)
      |> move (-37, -35)
      
  , roundedRect 1 60 1
      |> filled black
      |> rotate (degrees -80)
      |> move (-38, -30)
 
  , card black cardXPos cardYPos message
      |> notifyTap  
      (
        if (cardYPos /= cardSwipeFinalYPos) then --The card is still in the wallet
            CardTap --The card can be moved when clicked on
        else
            DummyEvent --Nothing happens when the card is in the right spot
      )

  , rect 20 60 
      |> filled darkBrown
      |> rotate (degrees -80)
      |> move (-34, -55)

  , roundedRect 1 60 1
      |> filled black
      |> rotate (degrees -80)
      |> move (-36, -45)
      
  , roundedRect 40 60 5
      |> filled lightBrown
      |> rotate (degrees 80)
      |> move (35, -40)
      
  , roundedRect 16 60.4 3
      |> filled brown
      |> rotate (degrees 80)
      |> move (37.2, -28)
      
  , circle 2
      |> filled black
      |> move (35, -32)
  ]

--The machine interface
machine message =
  group
  [
     swipeInterface
   , upperModule message
   , redCircle message
   , greenCircle message
  ]

swipeInterface =
  group
  [
     roundedRect 135 130 5
       |> filled black
   , roundedRect 130 35 5 --outer black border
       |> filled (rgb 157 157 157)
       |> move (0,45)
  ]
    |> scale 0.9
 
upperModule message =
  group
  [ 
     roundedRect 130 80 5 --Top grey part
       |> filled (rgb 84 84 84)
       |> move (0,-20)
   , roundedRect 130 15 5
       |> filled (rgb 157 157 157)
       |> move (0,12)
   , rect 120 12 --Green bar
       |> filled (rgb 29 69 30)
       |> move (0,53)
   , text message
       |> filled white
       |> scale 0.8
       |> move (-55,50)
   , arrow
       |> move (-25, 35)
  ]
    |> scale 0.9
 
arrow =
  group
  [
     triangle 7
       |> filled black
       |> move (5,0)
   , rect 40 5
       |> filled black
       |> move (-15,0)
  ]    
 
redCircle message =
  group
  [ circle 5.5
       |> filled black
       |> move (35,32)
   , circle 5
       |> filled
        (
           if (message /= "SWIPE CARD" && message /= "APPROVED") then --Failed attempt
               rgb 255 0 0 --Bright red
           else
               darkRed
        )
       |> move (35,32)
  ]
 
greenCircle message =
  group
  [
     circle 5.5
       |> filled black
       |> move (48,32)
   , circle 5
       |> filled
         (
           if (message == "APPROVED") then --Minigame complete
               rgb 0 255 0 --Bright green
           else
               darkGreen
         )
       |> move (48,32)
  ]

--Code for the Leaves Minigame
vacuum = --The hole where the leaves go into
  group
  [
     rect 30 150
       |> outlined (solid 1) black
       |> move (-80,0)
   , rect 30 150
       |> filled darkGray
       |> move (-80,0)
   , rect 10 50
       |> filled black
       |> move (-80,0)
  ]

leaf = 
  group
  [
     ngon 5 10
       |> filled brown
   , triangle 15
       |> filled brown
       |> move (-5,-10) 
   , rect 3 15
       |> filled brown
       |> rotate (degrees 150)
       |> move (5,10)
   , rect 20 1
       |> filled yellow
       |> rotate (degrees -115)
       |> move (-3,-5)
  ]

--Generic gunk for the game to run
type alias Model =
  {
     time : Float
   , state : State

   --Card Minigame
   , dragState : DragState
   , cardXPos : Float
   , cardYPos : Float
   , cardMessage : String
   , cardSwipeStartTime : Float
   , changeCardStateTracker : Int
   , cardApprovedTimeout : Float

   --Leaves Minigame
   , leaf1Pos : (Float, Float)
   , leaf1DragState : DragState
   , leaf2Pos : (Float, Float)
   , leaf2DragState : DragState
   , leaf3Pos : (Float, Float)
   , leaf3DragState : DragState
   , leaf4Pos : (Float, Float)
   , leaf4DragState : DragState
   , leaf5Pos : (Float, Float)
   , leaf5DragState : DragState
   , leaf6Pos : (Float, Float)
   , leaf6DragState : DragState
   , leavesStartTime : Float
  }

init : Model
init =
  {
     time = 0 
   , state = Cafeteria

   --Card Minigame
   , dragState = Released
   , cardXPos = cardWalletXPos
   , cardYPos = cardSwipeStartYPos
   , cardMessage = cardSwipeStartMsg
   , cardSwipeStartTime = 0
   , changeCardStateTracker = 0
   , cardApprovedTimeout = 0

   --Leaves Minigame
   , leaf1Pos = (-10,-25)
   , leaf1DragState = Released
   , leaf2Pos = (45,25)
   , leaf2DragState = Released
   , leaf3Pos = (55,-25)
   , leaf3DragState = Released
   , leaf4Pos = (-35,35)
   , leaf4DragState = Released
   , leaf5Pos = (0,10)
   , leaf5DragState = Released
   , leaf6Pos = (-55,0)
   , leaf6DragState = Released
   , leavesStartTime = 0
  }


main = gameApp Tick {model = init, view = view, update = update, title = "Game Slot"}

view model = collage 192 128 (myShapes model)
