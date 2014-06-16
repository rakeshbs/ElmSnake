import Mouse

widthSquare = 30
widthCanvas = 320
heightCanvas = 480
gridColor = rgba 0 0 0 0.3
canvasBackgroundColor = rgba 0 0 255 0.4

type Vec = (Int,Int)

type SnakeSegment = { position : Vec }

type Snake = {segments : [SnakeSegment]}

main = renderScreen

renderVerticalLineAtIndex index =  traced (solid gridColor) <|  segment (index * widthSquare,-1000) (index * widthSquare,1000)

renderHorizontalLineAtIndex index =  traced (solid gridColor) <|  segment (-1000,index * widthSquare) (1000,index * widthSquare)


renderGrid = let verticalLines = map (renderVerticalLineAtIndex) [(-widthCanvas/(2 * widthSquare))..(widthCanvas/(2 * widthSquare))]
                 horizontalLines =  map (renderHorizontalLineAtIndex) [(-heightCanvas/(2 * widthSquare))..(heightCanvas/(2 * widthSquare))]
             in verticalLines ++ horizontalLines

renderScreen = color canvasBackgroundColor <| collage 320 480 renderGrid
