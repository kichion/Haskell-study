myRepeat n = cycle [n]

subseq s e l = take (e - s) (drop s l)

inFirstHalf i l = i elem halfList
  where
    half = div (length l) 2
    halfList = take half l
