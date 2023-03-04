ifEven func x = if even x then func x else x

genIfEven f = (\x -> ifEven f x)

genIfXEven x = (\f -> ifEven f x)

inc x = x + 1

double x = x * 2

square x = x ^ 2

ifEvenInc = ifEven inc

ifEvenDouble = ifEven double

ifEvenSquare = ifEven square

getRequestUrl host apikey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apikey

genHostRequestBuilder host = (\apikey resource id -> getRequestUrl host apikey resource id)

genApiRequestBuilder hostBuilder apiKey resource = hostBuilder apiKey resource id

bookRequestBuilder = getRequestUrl "resource" "resource" "books"

subtract2 = flip (-) 2
-- subtract2 x = flip (-) 2 x
add4 a b c d = a + b + c + d
add3 a b c = add4 a b c 1
add3a = add4 1
-- add3a b c d = add4 1 b c d

binaryPartialApplication f x = (\y -> f x y)
