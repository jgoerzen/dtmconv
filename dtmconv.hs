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
-- ADDRESS BOOK
----------------------------------------------------------------------

-- Main address book processor
getAddresses :: Integer -> Content -> ([Content], Integer, Integer)
getAddresses startuid doc = 
    (concatMap addressbook contactselem, 
     (read (concatMap lastrid contactselem))::Integer, 
     (read (concatMap lastuid contactselem))::Integer)
    where 
        -- The <Contacts> tag
        contactselem = (tag "Contacts" `o` children `o` tag "DTM") doc
        
        -- Children of the <Contact> tags...
        -- Expected to be passed the Contact tag
        rowdata :: CFilter
        -- Same as: rowdata = children `with` tag "Contact"
        rowdata = tag "Contact" `o` children

        -- The input rows, numbered.
        rows :: LabelFilter (String, String)
        rows = numbered `x` versanumbered startuid (startuid - 1) $ rowdata

        -- The output rows
        contactcomps :: CFilter
        contactcomps = rowfunc `oo` rows

        -- The last output row
        lastcontactcomp :: Content -> Content
        lastcontactcomp = head . reverse . contactcomps

        -- The last rid
        lastrid :: Content -> String
        lastrid = showattv . attrofelem "rid" . lastcontactcomp
        lastuid :: Content -> String
        lastuid = showattv . attrofelem "Uid" . lastcontactcomp
                  
        -- The entire address book file
        addressbook :: CFilter
        addressbook = mkElem "AddressBook" 
                     [mkElem "RIDMax" [literal (ridmax (concatMap children contactselem))]
                     ,mkElem "Groups" []
                     ,mkElem "Contacts" [contactcomps]
                     ]
        
        -- Calculate the maximum RID value
        ridmax :: [Content] -> String
        ridmax c = show . (+) 1 . maximum . map ((read::String->Integer) . showattv . attrofelem "card") $ c
                   
        -- Process each row
        rowfunc :: (String, String) -> CFilter
        rowfunc (rid, uid) x =
            mkElemAttr "Contact"
                           (
                       [("FileAs", \x -> if (strof "FULL" x) `elem` ["", ",", ", "]
                                      then tagof "CPNY" x
                                      else tagof "FULL" x)
                       ,("rid", literal rid)
                       ,("Uid", literal uid)
                       ,("rinfo", literal "1")
                       ] ++ mapattrs addrmap x)
                       [] x

        -- The address mapping
        addrmap :: [(String, String)]
        addrmap = [("TITL", "Title"),
                   ("FNME", "FirstName"),
                   ("MNME", "MiddleName"),
                   ("LNME", "LastName"),
                   ("SUFX", "Suffix"),
                   --FileAs handled later
                   --Categories not handled
                   --UID handled later
                   ("DMAL", "DefaultEmail"),
                   ("MAL1", "Emails"),
                   ("HSTR", "HomeStreet"),
                   ("HCTY", "HomeCity"),
                   ("HSTA", "HomeState"),
                   ("HZIP", "HomeZip"),
                   ("HCTR", "Homecountry"),
                   ("TEL1", "HomePhone"),
                   ("FAX1", "HomeFax"),
                   ("CPS1", "HomeMobile"),
                   ("HWEB", "HomeWebPage"),
                   ("CPNY", "Company"),
                   ("BSTR", "BusinessStreet"),
                   ("BCTY", "BusinessCity"),
                   ("BSTA", "BusinessState"),
                   ("BZIP", "BusinessZip"),
                   ("BCTR", "BusinessCountry"),
                   ("BWEB", "BusinessWebPage"),
                   ("PSTN", "JobTitle"),
                   ("SCTN", "Department"),
                   ("OFCE", "Office"),
                   ("TEL2", "BusinessPhone"),
                   ("FAX2", "BusinessFax"),
                   ("CPS2", "BusinessMobile"),
                   ("BPGR", "BusinessPager"),
                   ("PRFS", "Profession"),
                   ("ASST", "Assistant"),
                   ("MNGR", "Manager"),
                   ("SPUS", "Spouse"),
                   ("CLDR", "Children"),
                   ("GNDR", "Gender"),
                   ("BRTH", "Birthday"),
                   ("ANIV", "Anniversary"),
                   ("NCNM", "Nickname"),
                   ("MEM1", "Notes")
                  ]
                                           
    