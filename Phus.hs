module Phus where
    import Register 

    -- Main function of the system. 
    phus::[(String, Bool, (Integer, Integer))] -> (String, [(String, (Integer, Integer))])
    phus (x:xs) = ("example", [("example", (1, 55))])
        -- TODO: read data from Register.hs
        --       do calculations
        --       print result



    -- TODO: This works?!
    -- Calculate the time between a cars arrival and departure 
    -- [(regNr, arriving?, (hour, min))] -> [(regNr, (hour, min))]
    calcParkTime::[(String, Bool, (Integer, Integer))] -> [(String, (Integer, Integer))]
    calcParkTime [] = []
    calcParkTime ((s, b, arr@(_, _)):xs) 
        | not b     = calcParkTime xs
        | otherwise = (s, pd) : calcParkTime xs
        where
            dpt = getDepartureTime s xs  
            pd  = getParkDuration arr dpt 


    -- TODO: write doc. 
    getParkDuration :: Integral a => (a, a) -> (a, a) -> (a, a)
    getParkDuration (h1, m1) (h2, m2) = (h, m)
        where 
            mTemp = m2 - m1
            hTemp = h2 - h1
            m = if mTemp >= 0 then mTemp else mTemp + 60
            h = if mTemp >= 0 then hTemp else hTemp - 1 
            

    -- TODO: Done, combine with other functions! (i.e: combineTimes!!)
    -- Matches input registration number with remaining cars and 
    -- fetches the departure time for that car. 
    getDepartureTime :: String -> [(String, Bool, (Integer, Integer))] -> (Integer, Integer)
    getDepartureTime _ [] = error "Empty list"
    getDepartureTime str ((str2, b, (h, m)):xs) 
        | str == str2 && not b = (h, m)
        | otherwise            = getDepartureTime str xs   
    --testData = [("test", False, (1,2)), ("cool", True, (3,4)), ("hej", False, (5,6)), ("nej", False, (7,8))]


    -- TODO: Done, combine with other functions!
    -- Takes two times as (Hour, Minute) and combines them
    combineTimes :: Integral a => (a, a) -> (a, a) -> (a, a)
    combineTimes (h1, m1) (h2, m2) = (h, m)
        where
            mTemp = m1 + m2
            hTemp = h1 + h2
            m = if mTemp < 60 then mTemp else mTemp - 60
            h = if mTemp < 60 then hTemp else hTemp + 1


    -- TODO: Done, combine with other functions!
    -- Finds the longest parked car and returns its 
    -- registration number.
    findLongest::[(String, (Integer, Integer))] -> String
    findLongest []  = error "Empty list"
    findLongest [x] = fst x 
    findLongest (x:y:xs) 
        | fst (snd x) > fst (snd y) = findLongest (x:xs)
        | fst (snd x) < fst (snd y) = findLongest (y:xs)
        | snd (snd x) > snd (snd y) = findLongest (x:xs)
        | otherwise                 = findLongest (y:xs)
    -- testData = [("nope", (1,2)), ("win", (10,10)), ("lose", (10, 9)), ("sike", (11, 55))]