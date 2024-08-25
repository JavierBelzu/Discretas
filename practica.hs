distanciaPuntos :: (Float, Float) -> (Float,Float) -> Float
distanciaPuntos (x1,y1) (x2,y2)  = sqrt (((x2-x1) * (x2-x1)) + ((y2-y1) * (y2-y1)))


hipotenusa :: Float -> Float -> Float
hipotenusa x y = sqrt ((x * x) + (y * y))

pendiente :: (Float, Float) -> (Float,Float) -> Float
pendiente (x1,y1) (x2,y2)  = ((y2-y1) / (x2-x1))

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = (((-b + sqrt(b*b -(4*a*c)))/2*a),((-b - sqrt(b*b -(4*a*c)))/2*a))                                                      

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (((a + b + c)/2)*(((a + b + c)/2) - a)*(((a + b + c)/2) - b)*(((a + b + c)/2) - c))

comparador :: Int -> Int -> Int
comparador x y = if x == y
 then 0
 else if x < y
  then -1
  else 1

esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente a b c d = if d < c && c < b && b < a
 then True
 else False

maximo :: Float -> Float -> Float -> Float
maximo x y z = if x > y && x > z
 then x
 else if x < y && z < y
      then y
      else z
