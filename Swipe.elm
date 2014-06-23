import Touch

data SwipeDirection = Left|Right|Up|Down|None

data TouchData =  { positionX      : Int,
                    positionY      : Int,
                    touchTime      : Int,
                    swipeDirection : SwipeDirection }

data TouchState = { touches : [TouchData] }



analyseTouches =

main = asText <~  Touch.touches
