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

with Ada.Text_IO; use Ada.Text_IO;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Support_Utils.Addons_Package; use Support_Utils.Addons_Package;
with Support_Utils.Uniques_Package; use Support_Utils.Uniques_Package;
use Latin_Utils;
package Support_Utils.Line_Stuff is
   pragma Elaborate_Body;

   type Dictionary_Item;
   type Dictionary_List is access Dictionary_Item;
   type Dictionary_Item is
      record
         De   : Dictionary_Entry := Null_Dictionary_Entry;
         Succ : Dictionary_List;
      end record;

   type Dictionary is array (Character) of Dictionary_List;
   Null_Dictionary : Dictionary := (others => null);
   --DICT, UNIQUES, QUES : DICTIONARY := NULL_DICTIONARY;
   Dict, Uniques : Dictionary := Null_Dictionary;

   Dict_Loc : Dictionary := Null_Dictionary;

   type Tackon_Line is
      record
         Pofs : Part_Of_Speech_Type := Tackon;
         Tack : Stem_Type := Null_Stem_Type;
         Entr : Tackon_Entry := Null_Tackon_Entry;
         Mean : Meaning_Type := Null_Meaning_Type;
      end record;

   Null_Tackon_Line : Tackon_Line;

   package Tackon_Line_Io is
      Default_Width : Natural;
      procedure Get (F : in File_Type; P : out Tackon_Line);
      procedure Get (P : out Tackon_Line);
      procedure Put (F : in File_Type; P : in Tackon_Line);
      procedure Put (P : in Tackon_Line);
      procedure Get (S : in String; P : out Tackon_Line; Last : out Integer);
      procedure Put (S : out String; P : in Tackon_Line);
   end Tackon_Line_Io;

   type Prefix_Line is
      record
         Pofs : Part_Of_Speech_Type := Prefix;
         Fix  : Fix_Type := Null_Fix_Type;
         Connect : Character := ' ';
         Entr : Prefix_Entry := Null_Prefix_Entry;
         Mean : Meaning_Type := Null_Meaning_Type;
      end record;

   Null_Prefix_Line : Prefix_Line;

   package Prefix_Line_Io is
      Default_Width : Natural;
      procedure Get (F : in File_Type; P : out Prefix_Line);
      procedure Get (P : out Prefix_Line);
      procedure Put (F : in File_Type; P : in Prefix_Line);
      procedure Put (P : in Prefix_Line);
      procedure Get (S : in String; P : out Prefix_Line; Last : out Integer);
      procedure Put (S : out String; P : in Prefix_Line);
   end Prefix_Line_Io;

   type Suffix_Line is
      record
         Pofs : Part_Of_Speech_Type := Suffix;
         Fix  : Fix_Type := Null_Fix_Type;
         Connect    : Character := ' ';
         Entr : Suffix_Entry := Null_Suffix_Entry;
         Mean : Meaning_Type := Null_Meaning_Type;
      end record;

   Null_Suffix_Line : Suffix_Line;

   package Suffix_Line_Io is
      Default_Width : Natural;
      procedure Get (F : in File_Type; P : out Suffix_Line);
      procedure Get (P : out Suffix_Line);
      procedure Put (F : in File_Type; P : in Suffix_Line);
      procedure Put (P : in Suffix_Line);
      procedure Get (S : in String; P : out Suffix_Line; Last : out Integer);
      procedure Put (S : out String; P : in Suffix_Line);
   end Suffix_Line_Io;

   type Unique_Entry is
      record
         Stem : Stem_Type          := Null_Stem_Type;
         Qual : Quality_Record     := Null_Quality_Record;
         Kind : Kind_Entry         := Null_Kind_Entry;
         Tran : Translation_Record := Null_Translation_Record;
      end record;

   package Unique_Entry_Io is
      Default_Width : Field;
      procedure Get (F : in File_Type; P : out Unique_Entry);
      procedure Get (P : out Unique_Entry);
      procedure Put (F : in File_Type; P : in Unique_Entry);
      procedure Put (P : in Unique_Entry);
      procedure Get (S : in String; P : out Unique_Entry; Last : out Integer);
      procedure Put (S : out String; P : in Unique_Entry);
   end Unique_Entry_Io;

   procedure Load_Stem_File (D_K : Dictionary_Kind);

   procedure Load_Dictionary (Dict : in out Dictionary;
                              Dictionary_File_Name : String);

   procedure Load_Uniques (Unq : in out Latin_Uniques; File_Name : in String);

end Support_Utils.Line_Stuff;
