-- arch-tag: DTM conversion program

{- 
TODO: categories
-}
{- Copyright (c) 2005 John Goerzen

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

attrofelem :: String -> Content -> AttValue
attrofelem attrname (CElem (Elem name al _)) =
    case lookup attrname al of
       Just x -> x
       Nothing -> error $ "attrofelem: no " ++ attrname ++ " in " ++ name
attrofelem _ _ =
    error "attrofelem: called on something other than a CElem"

showattv :: AttValue -> String
showattv (AttValue v) = worker v
                        where worker [] = []
                              worker (Left x:xs) = x ++ worker xs
                              worker (Right x:xs) = worker xs

parse =
    do c <- getContents
       return $ getContent $ xmlParse "(stdin)" c
    where getContent (Document _ _ e) = CElem e
       
xml2str =
    show . ppContent
    where 
        ppContent [CElem e] = element e
        ppContent []  = error "produced no output"
        ppContent _   = error "produced more than one output"

main = do doc <- parse
          writeFile "addressbook.xml" (xml2str (getAddresses doc))

tagof x = keep /> tag x /> txt
strof x y = verbatim $ tagof x $ y
strof2 x y = txt `o` children `o` tag x $ y

mapattrs [] _ = []
mapattrs (x:xs) doc = 
    let str = strof (fst x) doc
        tag = tagof (fst x)
        in
        {-
        (snd x, (strof2 (fst x))) : mapattrs xs doc
        -}
        if length str > 0
            then ((snd x), tag) : mapattrs xs doc
            else ((fst x), literal str) : mapattrs xs doc

getAddresses doc = 
    concatMap contacts contactselem
    where 
        contactselem = (tag "Contacts" `o` children `o` tag "DTM") doc
        rows = children `with` tag "Contact"
        contacts = mkElem "AddressBook" 
                     [mkElem "RIDMax" [literal (ridmax (concatMap children contactselem))]
                     ,mkElem "Groups" []
                     ,mkElem "Contacts" [concat . row 1 . rows ]
                     ]
        ridmax :: [Content] -> String
        ridmax c = show . maximum . map ((read::String->Integer) . showattv . attrofelem "card") $ c
                   
        row _ [] = []
        row rid (x:xs) =
            mkElemAttr "Contact"
                       (("FileAs", \x -> if (strof "FULL" x) `elem` ["", ",", ", "]
                                      then tagof "CPNY" x
                                      else tagof "FULL" x)
                        : mapattrs addrmap x)
                       [] x
            : row (rid + 1) xs

        addrmap = [("SYID", "uid"),
                   ("TITL", "Title"),
                   ("FNME", "FirstName"),
                   ("MNME", "MiddleName"),
                   ("LNME", "LastName"),
                   ("SUFX", "Suffix"),
                   --FileAs handled later
                   --Categories not handled
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
                                           
    