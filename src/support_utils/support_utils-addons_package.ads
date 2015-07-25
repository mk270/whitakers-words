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

with Ada.Text_IO; use type Ada.Text_IO.File_Type;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
use Latin_Utils;
package Support_Utils.Addons_Package is
   pragma Elaborate_Body;

   subtype Fix_Type is Stem_Type;
   Null_Fix_Type : constant Fix_Type := Null_Stem_Type;
   Max_Fix_Size : constant := Max_Stem_Size;

   subtype Target_Pofs_Type is Part_Of_Speech_Type range X .. V;

   type Target_Entry (Pofs : Target_Pofs_Type := X) is
      record
         case Pofs is
            when N  =>
               N : Noun_Entry;
               --NOUN_KIND : NOUN_KIND_TYPE;
            when Pron  =>
               Pron : Pronoun_Entry;
               --PRONOUN_KIND : PRONOUN_KIND_TYPE;
            when Pack  =>
               Pack : Propack_Entry;
               --PROPACK_KIND : PRONOUN_KIND_TYPE;
            when Adj  =>
               Adj : Adjective_Entry;
            when Num  =>
               Num : Numeral_Entry;
               --NUMERAL_VALUE : NUMERAL_VALUE_TYPE;
            when Adv  =>
               Adv : Adverb_Entry;
            when V  =>
               V : Verb_Entry;
               --VERB_KIND : VERB_KIND_TYPE;
            when others  =>
               null;
         end case;
      end record;

   Null_Target_Entry : Target_Entry;

   package Target_Entry_Io is
      Default_Width : Natural;
      procedure Get (F : in Ada.Text_IO.File_Type; P : out Target_Entry);
      procedure Get (P : out Target_Entry);
      procedure Put (F : in Ada.Text_IO.File_Type; P : in Target_Entry);
      procedure Put (P : in Target_Entry);
      procedure Get (S : in String; P : out Target_Entry; Last : out Integer);
      procedure Put (S : out String; P : in Target_Entry);
   end Target_Entry_Io;

   type Tackon_Entry is
      record
         Base : Target_Entry;
      end record;

   Null_Tackon_Entry : Tackon_Entry;

   package Tackon_Entry_Io is
      Default_Width : Natural;
      procedure Get (F : in Ada.Text_IO.File_Type; I : out Tackon_Entry);
      procedure Get (I : out Tackon_Entry);
      procedure Put (F : in Ada.Text_IO.File_Type; I : in Tackon_Entry);
      procedure Put (I : in Tackon_Entry);
      procedure Get (S : in String; I : out Tackon_Entry; Last : out Integer);
      procedure Put (S : out String; I : in Tackon_Entry);
   end Tackon_Entry_Io;

   type Prefix_Entry is
      record
         Root    : Part_Of_Speech_Type := X;
         Target  : Part_Of_Speech_Type := X;
      end record;

   Null_Prefix_Entry : Prefix_Entry;

   package Prefix_Entry_Io is
      Default_Width : Natural;
      procedure Get (F : in Ada.Text_IO.File_Type; P : out Prefix_Entry);
      procedure Get (P : out Prefix_Entry);
      procedure Put (F : in Ada.Text_IO.File_Type; P : in Prefix_Entry);
      procedure Put (P : in Prefix_Entry);
      procedure Get (S : in String; P : out Prefix_Entry; Last : out Integer);
      procedure Put (S : out String; P : in Prefix_Entry);
   end Prefix_Entry_Io;

   type Suffix_Entry is
      record
         Root       : Part_Of_Speech_Type := X;
         Root_Key   : Stem_Key_Type := 0;
         Target     : Target_Entry := Null_Target_Entry;
         Target_Key : Stem_Key_Type := 0;
      end record;

   Null_Suffix_Entry : Suffix_Entry;

   package Suffix_Entry_Io is
      Default_Width : Natural;
      procedure Get (F : in Ada.Text_IO.File_Type; P : out Suffix_Entry);
      procedure Get (P : out Suffix_Entry);
      procedure Put (F : in Ada.Text_IO.File_Type; P : in Suffix_Entry);
      procedure Put (P : in Suffix_Entry);
      procedure Get (S : in String; P : out Suffix_Entry; Last : out Integer);
      procedure Put (S : out String; P : in Suffix_Entry);
   end Suffix_Entry_Io;

   type Tackon_Item is
      record
         Pofs : Part_Of_Speech_Type := Tackon;
         Tack : Stem_Type := Null_Stem_Type;
         Entr : Tackon_Entry := Null_Tackon_Entry;
         MNPC : Integer := 0;
      end record;

   Null_Tackon_Item : Tackon_Item;

   type Prefix_Item is
      record
         Pofs : Part_Of_Speech_Type := Prefix;
         Fix  : Fix_Type := Null_Fix_Type;
         Connect : Character := ' ';
         Entr : Prefix_Entry := Null_Prefix_Entry;
         MNPC : Integer := 0;
      end record;

   Null_Prefix_Item : Prefix_Item;

   type Suffix_Item is
      record
         Pofs : Part_Of_Speech_Type := Suffix;
         Fix  : Fix_Type := Null_Fix_Type;
         Connect    : Character := ' ';
         Entr : Suffix_Entry := Null_Suffix_Entry;
         MNPC : Integer := 0;
      end record;

   Null_Suffix_Item : Suffix_Item;

   type Prefix_Array is array (Integer range <>) of Prefix_Item;
   type Tickon_Array is array (Integer range <>) of Prefix_Item;
   type Suffix_Array is array (Integer range <>) of Suffix_Item;
   type Tackon_Array is array (Integer range <>) of Tackon_Item;
   type Means_Array  is array (Integer range <>) of Meaning_Type;
   --  To simulate a DICT_IO file, as used previously

   Tackons  : Tackon_Array (1 .. 20);
   Packons  : Tackon_Array (1 .. 25);
   Tickons  : Prefix_Array (1 .. 10);
   Prefixes : Prefix_Array (1 .. 130);
   Suffixes : Suffix_Array (1 .. 185);
   Means    : Means_Array (1 .. 370);

   Number_Of_Tickons  : Integer := 0;
   Number_Of_Tackons  : Integer := 0;
   Number_Of_Packons  : Integer := 0;
   Number_Of_Prefixes : Integer := 0;
   Number_Of_Suffixes : Integer := 0;

   procedure Load_Addons (File_Name : in String);

   function Subtract_Tackon (W : String; X : Tackon_Item) return String;
   function Subtract_Prefix (W : String; X : Prefix_Item) return Stem_Type;
   function Subtract_Tickon (W : String; X : Prefix_Item) return Stem_Type
     renames Subtract_Prefix;
   function Subtract_Suffix (W : String; X : Suffix_Item) return Stem_Type;

   function Add_Prefix (Stem : Stem_Type;
                        Prefix : Prefix_Item) return Stem_Type;
   function Add_Suffix (Stem : Stem_Type;
                        Suffix : Suffix_Item) return Stem_Type;

end Support_Utils.Addons_Package;
