import Keyboard
import Window

widthSquare = 10
widthCanvas = 320
heightCanvas = 480
gridColor = rgba 0 0 0 0.7
canvasBackgroundColor = rgba 0 0 255 0.1
snakeColor = rgba 100 0 0 0.5
halfMaxWidth = widthCanvas `div` (2 * widthSquare)
halfMaxHeight = heightCanvas `div` (2 * widthSquare)
verticalNumberOfRows = 48
horizontalNumberOfRows = 32

data SnakeDirection = Up|Down|Right|Left|None

type Vec = (Int,Int)

type Snake = {segments : [Vec],direction : SnakeDirection}

type GameState = {snake : Snake}

defaultGame = { snake = defaultSnake }

defaultSnake = { segments = [(4,8),(4,7),(4,6)], direction = Down }

data Event = GameTick (Float) | KeyboardInput {x : Int, y : Int}


boundsX : Int -> Int
boundsX x =  if | x < -halfMaxWidth -> halfMaxWidth - 1
                | x >= halfMaxWidth -> -halfMaxWidth
                | otherwise         -> x

boundsY : Int -> Int
boundsY y =  if | y < -halfMaxHeight -> halfMaxHeight - 1
                | y >= halfMaxHeight -> -halfMaxHeight
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

drawGrid (canvasWidth,canvasHeight) squareSide  = let widthRatio = squareSide
                                                      heightRatio = squareSide
                                                      verticalLines = map (drawVerticalLineAtIndex squareSide) [(-widthRatio)..(widthRatio)]
                                                      horizontalLines =  map (drawHorizontalLineAtIndex squareSide) [(-heightRatio)..(heightRatio)]
                                                  in verticalLines ++ horizontalLines

{----------------------------------------------------------
Draw the Snake
-----------------------------------------------------------}
drawSnake : Snake -> [Form]
drawSnake snake = map (drawSnakeSegment) <| snake.segments

drawSnakeSegment : Vec -> Form
drawSnakeSegment (x,y) = square  widthSquare |> filled snakeColor |>
                            move ((toFloat x) * widthSquare + (widthSquare/2),
                                  (toFloat y) * widthSquare + (widthSquare/2))

drawGame : (Int,Int) -> GameState -> Element
drawGame (windowWidth,windowHeight) gameState = let squareSide = getSquareSide (windowWidth,windowHeight)
                                                    (canvasWidth,canvasHeight) = getCanvasSize (windowWidth,windowHeight)
                                                in color canvasBackgroundColor <| collage canvasWidth canvasHeight
                                                   <| (drawGrid (canvasWidth,canvasHeight) squareSide) ++ drawSnake gameState.snake

getSquareSide (width,height) = 10--height `div` verticalNumberOfRows

getCanvasSize (width,height) = let side = height `div` verticalNumberOfRows
                               in (side * horizontalNumberOfRows,height)

delta = fps 6
inputdelta = Keyboard.arrows

gameSignal = merges [lift GameTick delta,lift KeyboardInput inputdelta]

gameState = foldp (stepGame) defaultGame <| gameSignal

main = drawGame <~ Window.dimensions ~ gameState
