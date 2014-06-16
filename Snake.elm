import Mouse

widthSquare = 20
widthCanvas = 320
heightCanvas = 480
gridColor = rgba 0 0 0 1
canvasBackgroundColor = rgba 0 0 255 0.1
snakeColor = rgba 100 0 0 0.5

type Vec = (Int,Int)

type SnakeSegment = { position : Vec }

type Snake = {segments : [SnakeSegment]}

defaultSnake = [(4,4),(4,5),(4,6)]

{----------------------------------------------------------
Draw the grid
-----------------------------------------------------------}

drawVerticalLineAtIndex index =  traced (solid gridColor) <|  segment (index * widthSquare,-1000) (index * widthSquare,1000)

drawHorizontalLineAtIndex index =  traced (solid gridColor) <|  segment (-1000,index * widthSquare) (1000,index * widthSquare)


drawGrid = let verticalLines = map (drawVerticalLineAtIndex) [(-widthCanvas/(2 * widthSquare))..(widthCanvas/(2 * widthSquare))]
               horizontalLines =  map (drawHorizontalLineAtIndex) [(-heightCanvas/(2 * widthSquare))..(heightCanvas/(2 * widthSquare))]
           in verticalLines ++ horizontalLines


drawSnake segments = map (drawSnakeSegment) segments

drawSnakeSegment (x,y) = square  widthSquare |> filled snakeColor |> move (x * widthSquare + (widthSquare/2),y * widthSquare + (widthSquare/2))

drawGame = color canvasBackgroundColor <| collage 320 480 <|  drawGrid  ++ drawSnake defaultSnake


main = drawGame
