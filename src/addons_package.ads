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

with Text_IO; use type Text_IO.File_Type;
with inflections_package; use inflections_package;
with Dictionary_Package; use Dictionary_Package;
package addons_package is
   pragma Elaborate_Body;

   subtype fix_type is Stem_Type;
   null_fix_type : constant fix_type := Null_Stem_Type;
   max_fix_size : constant := Max_Stem_Size;

   subtype Target_pofs_type is part_of_speech_type range x..v;

   type Target_entry(pofs: Target_pofs_type := x) is
      record
         case pofs is
            when n  =>
               n : Noun_Entry;
               --NOUN_KIND : NOUN_KIND_TYPE;
            when pron  =>
               pron : Pronoun_Entry;
               --PRONOUN_KIND : PRONOUN_KIND_TYPE;
            when pack  =>
               pack : Propack_Entry;
               --PROPACK_KIND : PRONOUN_KIND_TYPE;
            when adj  =>
               adj : adjective_entry;
            when num  =>
               num : numeral_entry;
               --NUMERAL_VALUE : NUMERAL_VALUE_TYPE;
            when adv  =>
               adv : adverb_entry;
            when v  =>
               v : verb_entry;
               --VERB_KIND : VERB_KIND_TYPE;
            when others  =>
               null;
         end case;
      end record;

   null_Target_entry : Target_entry;

   package Target_entry_io is
      Default_Width : Natural;
      procedure Get(f : in Text_IO.File_Type; p : out Target_entry);
      procedure Get(p : out Target_entry);
      procedure Put(f : in Text_IO.File_Type; p : in Target_entry);
      procedure Put(p : in Target_entry);
      procedure Get(s : in String; p : out Target_entry; last : out Integer);
      procedure Put(s : out String; p : in Target_entry);
   end Target_entry_io;

   type tackon_entry is
      record
         base : Target_entry;
      end record;

   null_tackon_entry : tackon_entry;

   package tackon_entry_io is
      Default_Width : Natural;
      procedure Get(f : in Text_IO.File_Type; i : out tackon_entry);
      procedure Get(i : out tackon_entry);
      procedure Put(f : in Text_IO.File_Type; i : in tackon_entry);
      procedure Put(i : in tackon_entry);
      procedure Get(s : in String; i : out tackon_entry; last : out Integer);
      procedure Put(s : out String; i : in tackon_entry);
   end tackon_entry_io;

   type prefix_entry is
      record
         root    : part_of_speech_type := x;
         Target  : part_of_speech_type := x;
      end record;

   null_prefix_entry : prefix_entry;

   package prefix_entry_io is
      Default_Width : Natural;
      procedure Get(f : in Text_IO.File_Type; p : out prefix_entry);
      procedure Get(p : out prefix_entry);
      procedure Put(f : in Text_IO.File_Type; p : in prefix_entry);
      procedure Put(p : in prefix_entry);
      procedure Get(s : in String; p : out prefix_entry; last : out Integer);
      procedure Put(s : out String; p : in prefix_entry);
   end prefix_entry_io;

   type suffix_entry is
      record
         root       : part_of_speech_type := x;
         root_key   : stem_key_type := 0;
         Target     : Target_entry := null_Target_entry;
         Target_key : stem_key_type := 0;
      end record;

   null_suffix_entry : suffix_entry;

   package suffix_entry_io is
      Default_Width : Natural;
      procedure Get(f : in Text_IO.File_Type; p : out suffix_entry);
      procedure Get(p : out suffix_entry);
      procedure Put(f : in Text_IO.File_Type; p : in suffix_entry);
      procedure Put(p : in suffix_entry);
      procedure Get(s : in String; p : out suffix_entry; last : out Integer);
      procedure Put(s : out String; p : in suffix_entry);
   end suffix_entry_io;

   type tackon_item is
      record
         pofs: part_of_speech_type := tackon;
         tack : Stem_Type := Null_Stem_Type;
         entr : tackon_entry := null_tackon_entry;
         MNPC : Integer := 0;
      end record;

   null_tackon_item : tackon_item;

   type prefix_item is
      record
         pofs: part_of_speech_type := prefix;
         fix  : fix_type := null_fix_type;
         connect : Character := ' ';
         entr : prefix_entry := null_prefix_entry;
         MNPC : Integer := 0;
      end record;

   null_prefix_item : prefix_item;

   type suffix_item is
      record
         pofs: part_of_speech_type := suffix;
         fix  : fix_type := null_fix_type;
         connect    : Character := ' ';
         entr : suffix_entry := null_suffix_entry;
         MNPC : Integer := 0;
      end record;

   null_suffix_item : suffix_item;

   type prefix_array is array (Integer range <>) of prefix_item;
   type tickon_array is array (Integer range <>) of prefix_item;
   type suffix_array is array (Integer range <>) of suffix_item;
   type tackon_array is array (Integer range <>) of tackon_item;
   type means_array  is array (Integer range <>) of Meaning_Type;
   --  To simulate a DICT_IO file, as used previously

   tackons  : tackon_array(1..20);
   packons  : tackon_array(1..25);
   tickons  : prefix_array(1..10);
   prefixes : prefix_array(1..130);
   suffixes : suffix_array(1..185);
   means    : means_array(1..370);

   number_of_tickons  : Integer := 0;
   number_of_tackons  : Integer := 0;
   number_of_packons  : Integer := 0;
   number_of_prefixes : Integer := 0;
   number_of_suffixes : Integer := 0;

   procedure load_addons (file_name : in String);

   function subtract_tackon(w : String; x : tackon_item) return String;
   function subtract_prefix(w : String; x : prefix_item) return Stem_Type;
   function subtract_tickon(w : String; x : prefix_item) return Stem_Type
     renames subtract_prefix;
   function subtract_suffix(w : String; x : suffix_item) return Stem_Type;

   function add_prefix(stem : Stem_Type;
                       prefix : prefix_item) return Stem_Type;
   function add_suffix(stem : Stem_Type;
                       suffix : suffix_item) return Stem_Type;

end addons_package;
