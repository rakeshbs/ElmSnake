import Keyboard
import Window
import Touch

widthSquare = 10
widthCanvas = 320
heightCanvas = 480
gridColor = rgba 0 0 0 0.7
canvasBackgroundColor = rgba 0 0 255 0.1
snakeColor = rgba 100 0 0 0.5
verticalNumberOfCells = 24
horizontalNumberOfCells = 16
halfVerticalCells = verticalNumberOfCells `div` 2
halfHorizontalCells = horizontalNumberOfCells `div` 2

data SnakeDirection = Up|Down|Right|Left|None

type Vec = (Int,Int)

type Snake = {segments : [Vec],direction : SnakeDirection}

type GameState = {snake : Snake}

defaultGame = { snake = defaultSnake }

defaultSnake = { segments = [(4,8),(4,7),(4,6)], direction = Down }

data Event = GameTick (Float) | KeyboardInput {x : Int, y : Int}


boundsX : Int -> Int
boundsX x =  if | x < -halfHorizontalCells -> halfHorizontalCells - 1
                | x >= halfHorizontalCells -> -halfHorizontalCells
                | otherwise         -> x

boundsY : Int -> Int
boundsY y =  if | y < -halfVerticalCells -> halfVerticalCells - 1
                | y >= halfVerticalCells -> -halfVerticalCells
                | otherwise          -> y


moveSnakeHead : Vec -> SnakeDirection -> [Vec]
moveSnakeHead (x,y) direction = let (newX,newY) =
                                 case direction of
                                     Up    -> (x,y-1)
                                     Down  -> (x,y+1)
                                     Right -> (x+1,y)
                                     Left  -> (x-1,y)
                                in [(boundsX newX,boundsY newY)]

moveSnakeRest : [Vec] -> Vec -> [Vec]
moveSnakeRest segments prevSeg = let seg  = head segments
                                     segs = tail segments
                                 in if | segs == [] -> [prevSeg]
                                       | otherwise  -> [prevSeg] ++ moveSnakeRest segs seg

moveSnake : Snake -> Snake
moveSnake snake = let h = head snake.segments
                  in {snake | segments <- ( moveSnakeHead h snake.direction ++ moveSnakeRest (tail snake.segments) h) }


getDirection input = if  | input.x == -1 -> Left
                         | input.x ==  1 -> Right
                         | input.y ==  1  -> Down
                         | input.y == -1 -> Up
                         | otherwise     -> None

changeDirection snake input = { snake | direction <- let newdirection = getDirection input
                                                    in if | newdirection == None -> snake.direction
                                                          | otherwise            -> newdirection }

stepGame : Event -> GameState -> GameState
stepGame event g = case event of
                       GameTick _          -> {g | snake <- moveSnake g.snake }
                       KeyboardInput input -> {g | snake <- changeDirection g.snake input}
                       otherwise           -> g

{----------------------------------------------------------
Draw the grid
-----------------------------------------------------------}

drawVerticalLineAtIndex squareSide index =  traced (solid gridColor) <|
                                            segment (index * squareSide,-1000) (index * squareSide,1000)

drawHorizontalLineAtIndex squareSide index =  traced (solid gridColor) <|
                                              segment (-1000,index * squareSide) (1000,index * squareSide)

drawGrid (canvasWidth,canvasHeight) squareSide  = let widthRatio = toFloat (canvasWidth)/(2 * squareSide)
                                                      heightRatio = toFloat(canvasHeight)/(2 * squareSide)
                                                      verticalLines = map (drawVerticalLineAtIndex squareSide) [(-widthRatio)..(widthRatio)]
                                                      horizontalLines =  map (drawHorizontalLineAtIndex squareSide) [(-heightRatio)..(heightRatio)]
                                                   in verticalLines ++ horizontalLines

{----------------------------------------------------------
Draw the Snake
-----------------------------------------------------------}
drawSnake : Snake -> Float -> [Form]
drawSnake snake squareSide = map (drawSnakeSegment squareSide) <| snake.segments

drawSnakeSegment : Float -> Vec -> Form
drawSnakeSegment squareSide (x,y) = square  squareSide |> filled snakeColor |>
                                    move ((toFloat x) * squareSide + (squareSide/2),
                                    (toFloat y) * squareSide + (squareSide/2))

drawGame : (Int,Int) -> GameState -> Element
drawGame (windowWidth,windowHeight) gameState = let squareSide = getSquareSide (windowWidth,windowHeight)
                                                    (canvasWidth,canvasHeight) = getCanvasSize (windowWidth,windowHeight)
                                                in color canvasBackgroundColor <| collage canvasWidth canvasHeight
                                                   <| (drawGrid (canvasWidth,canvasHeight) squareSide) ++
                                                                  drawSnake gameState.snake squareSide


getSquareSide : (Int,Int) -> Float
getSquareSide (width,height) = toFloat(height `div` verticalNumberOfCells)

getCanvasSize (width,height) = let side = height `div` verticalNumberOfCells
                               in (side * horizontalNumberOfCells,side * verticalNumberOfCells)

delta = fps 6
inputdelta = Keyboard.arrows

getTouches = Touch.touches

gameSignal = merges [lift GameTick delta,lift KeyboardInput inputdelta]

currentGameState = foldp (stepGame) defaultGame <| gameSignal

getSingleTouch touches = if | length touches > 0 -> Just (head touches)
                            | otherwise -> Nothing

main = drawGame <~ Window.dimensions ~ currentGameState


