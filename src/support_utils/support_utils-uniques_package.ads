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

with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
package Support_Utils.Uniques_Package is

   type Unique_Item;
   type Unique_List is access Unique_Item;

   type Unique_Item is
      record
         Stem : Stem_Type      := Null_Stem_Type;
         Qual : Quality_Record := Null_Quality_Record;
         Kind : Kind_Entry     := Null_Kind_Entry;
         MNPC : Dict_IO.Count  := Null_MNPC;
         Succ : Unique_List;
      end record;

   type Latin_Uniques is array (Character range 'a' .. 'z') of Unique_List;
   Null_Latin_Uniques : Latin_Uniques := (others => null);

   Unq : Latin_Uniques := Null_Latin_Uniques;

   type Uniques_De_Array is
     array (Dict_IO.Positive_Count range <>) of Dictionary_Entry;

   Uniques_De : Uniques_De_Array (1 .. 100) :=
     (others => Null_Dictionary_Entry);

end Support_Utils.Uniques_Package;
