module Player exposing (moveLeft, moveUp, moveRight, moveDown)

moveLeft ( x, y ) =
  ( if x == 1 then 1 else x - 1, y )

moveUp ( x, y ) =
  ( x , if y == 1 then 1 else y - 1 )

moveRight ( x, y ) =
  ( if x == 10 then 10 else x + 1, y )

moveDown ( x, y ) =
  ( x, if y == 10 then 10 else y + 1 )
