-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936–2010)
--
-- This is a free program, which means it is proper to copy it and pass
-- it on to your friends. Consider it a developmental item for which
-- there is no charge. However, just for form, it is Copyrighted
-- (c). Permission is hereby freely given for any and all use of program
-- and data. You can sell it as your own, but at least tell me.
--
-- This version is distributed without obligation, but the developer
-- would appreciate comments and suggestions.
--
-- All parts of the WORDS system, source code and data files, are made freely
-- available to anyone who wishes to use them, for whatever purpose.

with Ada.Text_IO;
with Latin_Utils.Strings_Package;

package body Latin_Utils.General is

   ---------------------------------------------------------------------------

   procedure Load_Dictionary
      (Line : in out String;
       Last : in out Integer;
       D_K  :    out Latin_Utils.Dictionary_Package.Dictionary_Kind
      )
   is
      use Latin_Utils.Strings_Package;
   begin
      Ada.Text_IO.Put
         ("What dictionary to use, GENERAL or SPECIAL (Reply G or S) =>");
      Ada.Text_IO.Get_Line (Line, Last);
      if Last > 0  then
         if Trim (Line (Line'First .. Last))(1) = 'G' or else
            Trim (Line (Line'First .. Last))(1) = 'g'
         then
            D_K := Latin_Utils.Dictionary_Package.General;
         elsif Trim (Line (Line'First .. Last))(1) = 'S' or else
               Trim (Line (Line'First .. Last))(1) = 's'
         then
            D_K := Latin_Utils.Dictionary_Package.Special;
         else
            Ada.Text_IO.Put_Line ("No such dictionary");
            raise Ada.Text_IO.Data_Error;
         end if;
      end if;
   end Load_Dictionary;

   ---------------------------------------------------------------------------

end Latin_Utils.General;
