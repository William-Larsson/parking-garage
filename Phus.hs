module Phus where
    import Register (day0, day1, day2, day3,res_day0,res_day1,res_day2,res_day3)
    import Data.List (findIndices)

    -- Main function of the system. 
    phus :: [(String, Bool, (Integer, Integer))] -> (String, [(String, (Integer, Integer))])
    phus plist = let
        pt  = calcParkTime plist
        spe = sumParkEntries pt 
        in (findLongest spe, spe)


    -- Calculate the time between a cars arrival and departure 
    -- [(regNr, arriving?, (hour, min))] -> [(regNr, (hour, min))]
    calcParkTime :: [(String, Bool, (Integer, Integer))] -> [(String, (Integer, Integer))]
    calcParkTime [] = []
    calcParkTime ((s, b, arr@(_, _)):xs) 
        | not b     = calcParkTime xs
        | otherwise = (s, pd) : calcParkTime xs
        where
            dpt = getDepartureTime s xs  
            pd  = getParkDuration arr dpt 
            

    -- Matches input registration number with remaining cars and 
    -- fetches the departure time for that car. 
    getDepartureTime :: String -> [(String, Bool, (Integer, Integer))] -> (Integer, Integer)
    getDepartureTime _ [] = error "Empty list"
    getDepartureTime str ((str2, b, (h, m)):xs) 
        | str == str2 && not b = (h, m)
        | otherwise            = getDepartureTime str xs   
    

    -- Get the delta representing the duration that a car was parked. 
    -- (Hour, Min)::Arrival -> (Hour, Min)::Departure -> (dHour, dMin) 
    getParkDuration :: Integral a => (a, a) -> (a, a) -> (a, a)
    getParkDuration (h1, m1) (h2, m2) = (dh, dm)
        where 
            mTemp = m2 - m1
            hTemp = h2 - h1
            dm = if mTemp >= 0 then mTemp else mTemp + 60
            dh = if mTemp >= 0 then hTemp else hTemp - 1 


    -- Finds all park entries for the same car and sums 
    -- the times for all park instances in to a single entry. 
    sumParkEntries :: [(String, (Integer, Integer))] -> [(String, (Integer, Integer))]
    sumParkEntries []         = []
    sumParkEntries all@(x:_) = (fst x, sum) : sumParkEntries newList
        where
            i       = findIndices (\y -> fst x == fst y) all 
            times   = getTimes i all 
            sum     = foldr combineTimes (head times) (tail times) 
            newList = removeElements (reverse i) all


    -- Get times from from list based on list of indices. 
    getTimes :: [Int] -> [(String, (Integer, Integer))] -> [(Integer, Integer)]
    getTimes [] _ = []
    getTimes _ [] = []
    getTimes (i:is) xs = snd (xs !! i) : getTimes is xs


    -- Takes two times as (Hour, Minute) and combines them
    combineTimes :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
    combineTimes (h1, m1) (h2, m2) = (h, m)
        where
            mTemp = m1 + m2
            hTemp = h1 + h2
            m = if mTemp < 60 then mTemp else mTemp - 60
            h = if mTemp < 60 then hTemp else hTemp + 1


    -- Removes elements at indices from list xs. 
    -- [Int] indices needs to be in descending order
    removeElements :: [Int] -> [(String, (Integer, Integer))] -> [(String, (Integer, Integer))]
    removeElements is xs = foldl (flip removeAt) xs is


    -- Removes element from list at given index.
    removeAt :: Int -> [(String, (Integer, Integer))] -> [(String, (Integer, Integer))]
    removeAt n []  = []
    removeAt n (x:xs) 
        | n == 0    = removeAt (n-1) xs
        | otherwise = x : removeAt (n-1) xs


    -- Finds the longest parked car and returns its 
    -- registration number.
    findLongest :: [(String, (Integer, Integer))] -> String
    findLongest []  = error "Empty list"
    findLongest [x] = fst x 
    findLongest (x:y:xs) 
        | fst (snd x) > fst (snd y) = findLongest (x:xs)
        | fst (snd x) < fst (snd y) = findLongest (y:xs)
        | snd (snd x) > snd (snd y) = findLongest (x:xs)
        | otherwise                 = findLongest (y:xs)