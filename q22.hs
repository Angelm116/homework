
type Point = (Float, Float)

                                        -- Points are: 
data Shape = Rectangle Point Point      -- Bottom left and top right corners 
           | Triangle Point Point Point -- a b and c corners where 
                                        -- a-b, b-c and c-a are sides

perimeter :: Shape -> Float
perimeter (Rectangle (x,y) (x',y')) = let
    s1 = distance (x,y) (x',y) 
    s2 = distance (x,y) (x,y')
  in 
    2*s1+2*s2

perimeter (Triangle p1 p2 p3) = let
    s1 = distance p1 p2
    s2 = distance p2 p3
    s3 = distance p3 p1
  in
    s1 + s2 + s3



distance :: Point -> Point -> Float
distance (x, y) (x', y') = sqrt $ (x - x')^2 + (y - y')^2
