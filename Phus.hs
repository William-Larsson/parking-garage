module Phus where
    import OldRegister 

    -- Main function of the system. 
    phus::[(String, Bool, (Integer, Integer ))] -> (String, [(String, (Integer, Integer ))])
    phus (x:xs) = ("example", [("example", (1, 55))])
        -- read data from Register.hs
        -- do calculations
        -- print result


    -- Take the an arriving entry from the list and find its 
    -- corresponding departing value. Calc the time between entries. 
    -- TODO: separate into finding entries and calc.? 
    -- [(regNr, arriving?, (hour, min))] -> [(regNr, (hour, min))]
    calcTime::[(String, Bool, (Integer, Integer))] -> [(String, (Integer, Integer))]
    calcTime (x:xs) = [("example", (1, 10)), ("example", (0, 45))]
    --   Iterate over all entries.
    --   Take one entry's start time (True) and find the next end time for the entry
    --   Calculate the time spent in the parking in a new list


    combineTimes::[(String, (Integer, Integer))] -> [(String, (Integer, Integer))]
    combineTimes (x:xs) = [("example", (1, 55))]
    --2. Iterate over all entries in new list from 1. 
    --   Take of entry and look for duplicates. 
    --   If duplicate(s) exists, combine time. 
    --   Make new list based on this. 

    findLongest::[(String, (Integer, Integer))] -> (String, [(String, (Integer, Integer ))])
    findLongest (x:xs) = ("example", [("example", (1, 55))])
    --3. Iterate over all entries in new list from 2. 
    --   Find the entry with longest parking time that day, save id-number
    --   Make a tuple of id number and list from 2. 
    