-- arch-tag: DTM conversion program
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

attrofelem :: String -> Content -> String
attrofelem attrname (CElem (Elem _ [al] _)) =
    case lookup attrname al of
       Just x -> x
       Nothing -> error $ "attrofelem: no " ++ attrname ++ " in " ++ (show al)
attrofelem _ _ =
    error "attrofelem: called on something other than a CElem"

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
          addr <- getAddresses doc
          writeFile "addressbook.xml" (xml2str addr)

getAddresses doc = 
    (contacts `o` tag "Contacts" `o` children `o` tag "DTM") doc
    where 
        rows = children `with` tag "Contact"
        contacts = mkElem "AddressBook" 
                     [mkElem "RIDMax" [literal (show $ ridmax $ tag "Contact")]
                     ,mkElem "Groups" []
                     ,mkElem "Contacts" [row `o` rows]
                     ]
        ridmax = maximum $ map (read . attrofelem "card")
            
        row = 
            let fname = keep /> tag "FNME" /> txt
                lname = keep /> tag "LNME" /> txt
            in
            mkElemAttr "Contact" 
                       [("FirstName", fname)
                       ,("LastName", lname)
                       ] []

                                           
    