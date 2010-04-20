----------------------------------------------------------------------
THIS SOFTWARE IS NO LONGER MAINTAINED.

dtmconv Copyright (c) 2005 John Goerzen
----------------------------------------------------------------------

DTM to Opie Converter

This tool is used to convert from Sharp's proprietary Zaurus PIM
storage format used on its newer ROMs to the XML format used by Opie.
It is useful whenever you are migrating from a Sharp ROM to something
such as OpenZaurus on your device.

dtmconv currently supports converting these items:

 * To-do list: full conversion except for categories

 * Address book: full conversion except for categories

 * Datebook: partial , buggy conversion (see note below)

 * Categories: no conversion

 * E-Mail: no conversion

----------------------------------------------------------------------
CONVERSION PROCEDURE

The general procedure is this:

1. While still using the Sharp ROM, run dtm2xml to generate DTM XML.
   Copy it to your desktop PC or a safe place.

2. Back up your unit and re-flash with the new image.

3. Convert the DTM XML to Opie XML.

4. Upload the new XML files.

For step 1, you'll need the Arm binary dtm2xml that's part of ZMacSync
1.5.2 or above.  Find it at
http://www.dsitri.de/wiki.php?page=ZMacSync.

For step 3, you need dtmconv, this program.  It's written in Haskell.
If you are running on Linux:

 * Install GHC or Hugs for your platform.  If your distribution
   doesn't have them, download from www.haskell.org/ghc or
   www.haskell.org/hugs.

 * Type "make" to compile if using ghc, or look at the Makefile for
   hugs commands.

If you have Windows:

 * Download GHC from www.haskell.org/ghc, then run:

   ghc -cpp --make -O2 -iHaXml-1.12/src -o dtmconv.exe dtmconv.hs

Now, run:

dtmconv < data.xml

Where data.xml is the file you saved from step 1.

Finally, copy the generated addressbook.xml and todolist.xml to the
appropriate places under ~/Applications on your Opie system.

Note on the datebook: I could never make it work right.  There was
some odd incoming corruption on some records, and even records that I
thought looked perfect on output were mis-parsed by Opie.  It's not
reliable.


arch-tag: README file for dtmconv
