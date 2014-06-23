import Touch

data SwipeDirection = Left|Right|Up|Down|None

data TouchData =  { positionX      : Int,
                    positionY      : Int,
                    startPositionX : Int,
                    startPositionY : Int,
                    touchTime      : Int,
                    swipeDirection : SwipeDirection }

data TouchState = { touches : [TouchData] }

checkIfTouchEqual touch touchData = let c1 = (touchData.startPositionX,touchData.startPositionY,touchData.touchTime)
                                        c2 = (touch.x0, touch.y0, touch.t0)
                                    in  c1 == c2

checkIfTouchEnded touches touchData = let touch = head touches
                                      in if | touch == [] -> False
                                            | checkIfTouchEqual touch touchData == True -> True
                                            | otherwise checkIfTouchEnded (tail touches) touchData

checkIfNewTouch touch touchState =  let touchData = head touchState
                                    in if | touchData == [] -> False
                                          | checkIfTouchEqual touch touchData == True -> True
                                          | otherwise checkIfNewTouch touch (tail touchState)


updateTouches

analyseTouches touches touchState = let currentTouches = filter (checkIfTouchEnded touches) touchState
                                        newTouches = filter (checkIfNewTouch touches) touchState.touches

main = asText <~  Touch.touches
