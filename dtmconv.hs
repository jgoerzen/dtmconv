-- arch-tag: DTM conversion program

{-

TODO: categories
CHECK: can rid be eliminated? (palm uses it, so it doesn't seem to harm anything)

   Copyright (c) 2005 John Goerzen

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

-}

import Text.XML.HaXml
import System.Posix.Time(epochTime)
import System.Time
import Text.Regex
import Data.List
import MissingH.Time

-- Get an attribute value from an element.

attrofelem :: String -> Content -> AttValue
attrofelem attrname (CElem (Elem name al _)) =
    case lookup attrname al of
       Just x -> x
       Nothing -> error $ "attrofelem: no " ++ attrname ++ " in " ++ name
attrofelem _ _ =
    error "attrofelem: called on something other than a CElem"

--Render an attribute value as a string.

showattv :: AttValue -> String
showattv (AttValue v) = worker v
                        where worker [] = []
                              worker (Left x:xs) = x ++ worker xs
                              worker (Right x:xs) = worker xs

-- Parse stdin.

parse :: IO Content
parse =
    do c <- getContents
       return $ getContent $ xmlParse "(stdin)" c
    where getContent (Document _ _ e) = CElem e
       
-- Render a Cnotent.

xml2str :: [Content] -> String
xml2str =
    render . ppContent
    where 
        ppContent [CElem e] = element e
        ppContent []  = error "produced no output"
        ppContent _   = error "produced more than one output"

-- Split a date.  Returns Just (date, time) or Nothing if the input
-- was NULL or otherwise unparsable.
splitdate :: String -> Maybe (String, String)
splitdate x = 
    case break (== 'T') x of
      (_, "") -> Nothing
      (date, time) -> Just (date, tail time)

-- Convert a tag to calendar time.
tag2ct :: String -> Content -> Maybe CalendarTime
tag2ct x y = date2ct $ strof x y

ct2loc :: CalendarTime -> IO Integer
ct2loc = calendarTimeToEpoch

ct2utc :: CalendarTime -> Integer
ct2utc = calendarTimeUTCToEpoch

-- Convert CT to epoch time.
ct2epoch :: CalendarTime -> Maybe Integer
ct2epoch ct = 
    if ctYear ct < 1971 || ctHour ct > 24 then Nothing
       else case toClockTime ct of
                 TOD x _ -> Just x

-- Convert a date to a generic calendar time object.
-- Direct conversion.  Must adjust tz in calendar time object if necessary.
date2ct :: String -> Maybe CalendarTime
date2ct d =
    case matchRegexAll dregex d of
      Just (_, _, _, [year, month, day, hour, min, sec]) ->
          Just $ CalendarTime
                   {ctYear = read year,
                    ctMonth = toEnum ((read month) - 1),
                    ctDay = read day,
                    ctHour = read hour,
                    ctMin = read min,
                    ctSec = read sec,
                    ctPicosec = 0,
                    ctWDay = Sunday,
                    ctYDay = 0,
                    ctTZName = "",
                    ctTZ = 0,
                    ctIsDST = False}
      Nothing -> Nothing
      Just (_, _, _, x) -> error $ "Strange result: " ++ (show x)
    where
    dregex = mkRegex "^([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9])T([0-9][0-9])([0-9][0-9])([0-9][0-9])"


-- Program entry point
main :: IO ()
main = do time <- epochTime
          -- UIDs start from a negative timestamp and decrease from there
          let uid = (fromIntegral time) * (-1)
          doc <- parse
          let (addressdata, lastrid, lastuid) = getAddresses uid doc
          writeFile "addressbook.xml" (xml2str addressdata)
          putStrLn $ "Wrote addressbook.xml, rid 1 to " ++ (show lastrid) ++
                     ", uid " ++ (show uid) ++ " to " ++ (show lastuid)
          let (tododata, lastuidtodo) = getTodos (lastuid - 1) doc
          writeFile "todolist.xml" (xml2str tododata)
          putStrLn $ "Wrote todolist.xml, uid " ++ (show (lastuid - 1)) ++
                     " to " ++ (show lastuidtodo)
          let (dbdata, lastuiddb) = getDB (lastuidtodo - 1) doc
          writeFile "datebook.xml" (xml2str dbdata)
          putStrLn $ "Wrote datebook.xml, uid " ++ (show (lastuidtodo - 1)) ++
                     " to " ++ (show lastuiddb)
          putStrLn " *** Conversion completed successfully! ***"

-- Finds the literal children of the named tag, and returns it/them
tagof :: String -> CFilter
tagof x = keep /> tag x /> txt

-- Retruns the literal string that tagof would fine
strof :: String -> Content -> String
strof x y = verbatim $ tagof x $ y

{- Takes a list of (OldName, NewName) pairs.  Returns a list of (NewName,
CFilter) pairs that will yield the content from calling tagof on the oldname.
-}
mapattrs :: [(String, String)] -> Content -> [(String, CFilter)]
mapattrs [] _ = []
mapattrs (x:xs) doc = 
        case strof (fst x) doc of
           "" -> mapattrs xs doc  -- Omit this tag if the content is empty
           _ -> ((snd x), tagof (fst x)) : mapattrs xs doc

{- Like HaXml's numbered function, but instead of starting with 1 and
incrementing by 1, takes a start and a next. -}
versanumbered :: (Enum a, Show a) => a -> a -> CFilter -> LabelFilter String
versanumbered start next f = zip (map show [start,next..]) . f

----------------------------------------------------------------------
-- TODO LIST
------------------------------------------------------------

getTodos :: Integer -> Content -> ([Content], Integer)
getTodos startuid doc =
    (tasks `o` inputTop $ doc,
           startuid - count)
    where
    -- The top-level of the input
    inputTop :: CFilter
    inputTop = tag "Tasks" `o` children `o` tag "DTM"

    -- The top-level of the output
    tasks :: CFilter
    tasks = mkElem "Tasks"
            [row_task `oo` task_attrs]
            
    count = genericLength $ children `o` inputTop $ doc

    -- Each row if the input
    task_attrs :: LabelFilter String
    task_attrs = versanumbered startuid (startuid - 1)
                             (tag "Task" `o` children)

    -- Each row of the output
    row_task :: String -> CFilter
    row_task uid inp = mkElemAttr "Task" rowattrs [] inp
                   where
                   rowattrs = mapattrs todomap inp
                              ++ [("Uid", literal uid),
                                  ("Completed",
                                              if (strof "MARK" inp) == "0"
                                                 then literal "1"
                                                      else literal "0"
                                  )]
                              ++ case splitdate . strof "ETDY" $ inp of
                                     Nothing -> []
                                     Just (date, _) -> [("StartDate",
                                                         literal date)]
                              ++ case splitdate . strof "FNDY" $ inp of
                                     Nothing -> []
                                     Just (date, _) -> [("CompletedDate",
                                                         literal date)]
                              ++ case splitdate . strof "LTDY" $ inp of
                                     Nothing -> [("HasDate", literal "0")]
                                     Just (date, _) ->
                                         [("HasDate", literal "1")
                                         ,("DateYear", literal year)
                                         ,("DateMonth", literal month)
                                         ,("DateDay", literal day)]
                                         where (year, yr) = splitAt 4 date
                                               (month, mr) = splitAt 2 yr
                                               day = mr
                   todomap = [("TITL", "Summary")
                             ,("MEM1", "Description")
                             ,("PRTY", "Priority")
                             ]


----------------------------------------------------------------------
-- ADDRESS BOOK
----------------------------------------------------------------------

-- Main address book processor
getAddresses :: Integer -> Content -> ([Content], Integer, Integer)
getAddresses startuid doc = 
    (addressbook `o` inputTop $ doc,
     count,
     startuid - count)
    where 
        -- The <Contacts> tag -- top-level of the input
        inputTop :: CFilter
        inputTop = tag "Contacts" `o` children `o` tag "DTM"
        
        -- AddressBook -- the top level of the output
        addressbook :: CFilter
        addressbook = mkElem "AddressBook" 
                     [mkElem "RIDMax" [literal (show (count + 1))]
                     ,mkElem "Groups" []
                     ,mkElem "Contacts" [row_contact `oo` contact_attrs]
                     ]
        
        count = genericLength $ children `o` inputTop $ doc

        -- Each row of the input
        contact_attrs :: LabelFilter (String, String)
        contact_attrs = numbered `x` versanumbered startuid (startuid - 1)
                          $ tag "Contact" `o` children

        -- Each row of the output
        row_contact :: (String, String) -> CFilter
        row_contact (rid, uid) inp =
            mkElemAttr "Contact" rowattrs [] inp
            where rowattrs = 
                       [("FileAs", \x -> if (strof "FULL" x) `elem` ["", ",", ", "]
                                      then tagof "CPNY" x
                                      else tagof "FULL" x)
                       ,("rid", literal rid)
                       ,("Uid", literal uid)
                       ,("rinfo", literal "1")
                       ] ++ mapattrs addrmap inp

        -- The address mapping
        addrmap :: [(String, String)]
        addrmap = [("TITL", "Title"),          ("FNME", "FirstName"),
                   ("MNME", "MiddleName"),     ("LNME", "LastName"),
                   ("SUFX", "Suffix"),
                   --FileAs, Categories, UID handled earlier
                   ("DMAL", "DefaultEmail"),   ("MAL1", "Emails"),
                   ("HSTR", "HomeStreet"),     ("HCTY", "HomeCity"),
                   ("HSTA", "HomeState"),      ("HZIP", "HomeZip"),
                   ("HCTR", "Homecountry"),    ("TEL1", "HomePhone"),
                   ("FAX1", "HomeFax"),        ("CPS1", "HomeMobile"),
                   ("HWEB", "HomeWebPage"),    ("CPNY", "Company"),
                   ("BSTR", "BusinessStreet"), ("BCTY", "BusinessCity"),
                   ("BSTA", "BusinessState"),  ("BZIP", "BusinessZip"),
                   ("BCTR", "BusinessCountry"),("BWEB", "BusinessWebPage"),
                   ("PSTN", "JobTitle"),       ("SCTN", "Department"),
                   ("OFCE", "Office"),         ("TEL2", "BusinessPhone"),
                   ("FAX2", "BusinessFax"),    ("CPS2", "BusinessMobile"),
                   ("BPGR", "BusinessPager"),  ("PRFS", "Profession"),
                   ("ASST", "Assistant"),      ("MNGR", "Manager"),
                   ("SPUS", "Spouse"),         ("CLDR", "Children"),
                   ("GNDR", "Gender"),         ("BRTH", "Birthday"),
                   ("ANIV", "Anniversary"),    ("NCNM", "Nickname"),
                   ("MEM1", "Notes")
                  ]

                                           
----------------------------------------------------------------------
-- DATE BOOK
------------------------------------------------------------

-- Main date book processor
getDB :: Integer -> Content -> ([Content], Integer)
getDB startuid doc = 
    (events `o` inputTop $ doc,
            startuid - count)
    where

    -- The top-level of the input
    inputTop :: CFilter
    inputTop = tag "Events" `o` children `o` tag "DTM"

    -- The top level of the output
    events :: CFilter
    events = mkElem "DATEBOOK" [
                                mkElem "events"
                                           [row_event `oo` event_attrs]
                               ]

    count = genericLength $ children `o` inputTop $ doc

    -- Each row of the input
    event_attrs :: LabelFilter String
    event_attrs = versanumbered startuid (startuid - 1)
                      (filter corruptfilter_in . tag "Event" `o` children)
    
    -- Filter out corrupt rows on input.
    corruptfilter_in :: Content -> Bool
    corruptfilter_in inp = if  (strof "DSRP" inp /= "") &&
                               (strof "ADAY" inp `elem` ["1", "0"]) &&
                               (strof "TIM1" inp /= "NULL" || strof "TIM2" inp /= "NULL") &&
                               (strof "TIM1" inp >= "1989" || strof "TIM2" inp >= "1989")
                               then True
                               else False

    -- Each row of the output
    row_event :: String -> CFilter
    row_event uid inp = mkElemAttr "event" rowattrs [] inp
        where
        rowattrs = (mapattrs eventmap inp) ++ customattrs
                   ++ times inp ++ alarm inp ++ repeat inp
                      
        alarm :: Content -> [(String, CFilter)]
        alarm inp = case strof "ARON" inp of
                      "1" -> -- alarm on
                             if strof "ARMN" inp /= "0" then
                                    [("alarm", literal $ show $ (read ((strof "ARMN" inp))::Int) {-* 60-}),
                                                                                                              ("sound", literal "loud")]
                                    else []
                      _ -> []   -- alarm off

        rfrq :: Content -> [(String, CFilter)]
        rfrq inp = [("rfreq", tagof "RFRQ"),
                    ("rposition", tagof "RPOS"),
                    ("rweekdays", tagof "RDYS")]

        repeat :: Content -> [(String, CFilter)]
        repeat inp = 
            case strof "RTYP" inp of
              "255" -> []         -- No repeat
              "0" -> [("rtype", literal "Daily")] ++ rfrq inp
              "1" -> [("rtype", literal "Weekly")] ++ rfrq inp
              "2" -> [("rtype", literal "MonthlyDay")] ++ rfrq inp
              "3" -> [("rtype", literal "MonthlyDate")] ++ rfrq inp
              "4" -> [("rtype", literal "Yearly")] ++ rfrq inp
              _ -> []           -- unknown
            ++ case strof "REND" inp of
                 "1" -> [("rhasenddate", literal "1")] ++
                        case tag2ct "REDT" inp >>= return . ct2utc of
                          Nothing -> []
                          Just x -> [("enddt", literal (show x))]
                 _ -> []

        tag2loc :: String -> String -> 
                   (CalendarTime -> CalendarTime) -> Content -> 
                   IO [(String, CFilter)]
        tag2loc oldtagname newtagname func inp =
            case tag2ct oldtagname inp of
               Nothing -> return []
               Just ct -> do x <- ct2loc $ func ct
                             return $ [(newtagname, literal (show x))]
        tag2utc :: String -> String -> 
                   (CalendarTime -> CalendarTime) -> Content -> 
                   [(String, CFilter)]
        tag2utc oldtagname newtagname func inp =
            case tag2ct oldtagname inp of
               Nothing -> []
               Just ct -> let x = ct2utc $ func ct
                              in [(newtagname, literal (show x))]


        times :: Content -> [(String, CFilter)]
        times inp = case strof "ADAY" inp of
                  "1" ->  -- All-day item
                          do alsd <- tag2loc "ALSD" "start" 
                                       (\ct -> ct {ctHour = 0, ctMin = 0,
                                                   ctSec = 0}) inp
                             aled <- tag2loc "ALED" "end"
                                       (\ct -> ct {ctHour = 23, ctMin = 59,
                                                   ctSec = 0}) inp
                             return $ alsd ++ aled
                  _ -> -- Non-all-day item
                       return $ tag2utc "TIM1" "start" id inp
                             ++ tag2utc "TIM2" "end" id inp

        customattrs :: [(String, CFilter)]
        customattrs = 
            [
             ("categories", literal ""),
             ("uid", literal uid)
             --("created", literal $ show $ ((read uid)::Integer) * (-1)),
            ]
        eventmap = [("DSRP", "description"),
                    ("PLCE", "location"),
                    ("MEM1", "note")
                   ]
