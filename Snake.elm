import Mouse

widthSquare = 20
widthCanvas = 320
heightCanvas = 480
gridColor = rgba 0 0 0 0.7
canvasBackgroundColor = rgba 0 0 255 0.1
snakeColor = rgba 100 0 0 0.5

data SnakeDirection = Up|Down|Right|Left

type Vec = (Int,Int)

type Snake = {segments : [Vec],direction : SnakeDirection}

type GameState = {snake : Snake}

defaultGame = { snake = defaultSnake }

defaultSnake = { segments = [(4,4),(4,5),(4,6)], direction = Up }

type UserInput = {}

userInput : Signal UserInput
userInput = constant {}

type Input = { timeDelta:Float, userInput:UserInput }

{----------------------------------------------------------
Draw the grid
-----------------------------------------------------------}

drawVerticalLineAtIndex index =  traced (solid gridColor) <|
                                 segment (index * widthSquare,-1000) (index * widthSquare,1000)

drawHorizontalLineAtIndex index =  traced (solid gridColor) <|
                                    segment (-1000,index * widthSquare) (1000,index * widthSquare)


drawGrid = let widthRatio = (widthCanvas / (2 * widthSquare))
               heightRatio = (heightCanvas / (2 * widthSquare))
               verticalLines = map (drawVerticalLineAtIndex) [(-widthRatio)..(widthRatio)]
               horizontalLines =  map (drawHorizontalLineAtIndex) [(-heightRatio)..(heightRatio)]
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

drawGame : GameState -> Element
drawGame gameState = color canvasBackgroundColor <| collage 320 480
                                <| drawGrid  ++ drawSnake gameState.snake

delta = fps 30
input = sampleOn delta (lift2 Input delta userInput)
interval = every second

gameState = foldp stepGame defaultGame input



main = drawGame <~ gameState
