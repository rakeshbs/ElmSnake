import Touch

data SwipeDirection = Left|Right|Up|Down|None

data TouchData =  { positionX      : Int,
                    positionY      : Int,
                    touchTime      : Int,
                    swipeDirection : SwipeDirection }

data TouchState = { touches : [TouchData] }

checkIfTouchEqual touch touchData = let c1 = (touchData.positionX,touchData.positionY,touchData.touchTime)
                                        c2 = (touch.x0, touch.y0, touch.t0)
                                    in  c1 == c2

checkIfTouchEnded touches touchData = let touch = head touches
                                       in if | touch == [] -> False
                                             | checkIfTouchEqual touch touchData == True -> True
                                             | otherwise checkIfTouchEnded (tail touches) touchData

analyseTouches = let currentTouches = filter (checkIfTouchEnded touches) touchData


main = asText <~  Touch.touches

y = ((1,2,3) == (1,2,3))
-- > True : Bool
