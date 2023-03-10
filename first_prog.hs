main :: IO ()
main = do
  print "Whois the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)

toPart :: [Char] -> [Char]
toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart :: [Char] -> [Char]
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ",\n"

fromPart :: [Char] -> [Char]
fromPart author = "Thanks,\n" ++ author

createEmail :: [Char] -> [Char] -> [Char] -> [Char]
createEmail recipient bookTitle author = toPart recipient ++ bodyPart bookTitle ++ fromPart author
