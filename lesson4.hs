import Data.List

ifEven func x = if even x then func x else x

names = [("Ian", "Sumner"), ("Ian", "Curtis"), ("Bernard", "Sumner"), ("Peter", "Hook"), ("Stephen", "Morris")]

compareLastNames name1 name2
  | lastNameOrdering == EQ = firstNameOrdering
  | otherwise = lastNameOrdering
  where
    lastNameOrdering = compare (snd name1) (snd name2)
    firstNameOrdering = compare (fst name1) (fst name2)

addressLetter name location = locationFunc name
  where
    locationFunc = getLocationFunction location

getLocationFunction location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "wdc" -> wdcOffice
    _ -> (\name -> fst name ++ "" ++ snd name)

sfOffice name =
  if lastName < "L"
    then nameText ++ "POBox1234SanFrancisco,CA,94111"
    else nameText ++ "POBox1010SanFrancisco,CA,94109"
  where
    lastName = snd name
    nameText = fst name ++ "" ++ lastName

nyOffice name = nameText ++ ":POBox789NewYork,NY,10013"
  where
    nameText = fst name ++ "" ++ snd name

renoOffice name = nameText ++ "POBox456Reno,NV89523"
  where
    nameText = snd name

wdcOffice name = nameText ++ "Esq"
  where
    nameText = fst name ++ "" ++ snd name
