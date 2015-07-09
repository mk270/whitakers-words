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

with Text_IO; use Text_IO;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with addons_package; use addons_package;
with uniques_package; use uniques_package;
use Latin_Utils;
package line_stuff is
   pragma Elaborate_Body;

   type dictionary_item;
   type dictionary_list is access dictionary_item;
   type dictionary_item is
      record
         de   : Dictionary_Entry := Null_Dictionary_Entry;
         succ : dictionary_list;
      end record;

   type dictionary is array (Character) of dictionary_list;
   null_dictionary : dictionary := (others => null);
   --DICT, UNIQUES, QUES : DICTIONARY := NULL_DICTIONARY;
   dict, uniques : dictionary := null_dictionary;

   dict_loc : dictionary := null_dictionary;

   type tackon_line is
      record
         pofs : Part_Of_Speech_Type := tackon;
         tack : Stem_Type := Null_Stem_Type;
         entr : tackon_entry := null_tackon_entry;
         mean : Meaning_Type := Null_Meaning_Type;
      end record;

   null_tackon_line : tackon_line;

   package tackon_line_io is
      Default_Width : Natural;
      procedure Get(f : in File_Type; p : out tackon_line);
      procedure Get(p : out tackon_line);
      procedure Put(f : in File_Type; p : in tackon_line);
      procedure Put(p : in tackon_line);
      procedure Get(s : in String; p : out tackon_line; last : out Integer);
      procedure Put(s : out String; p : in tackon_line);
   end tackon_line_io;

   type prefix_line is
      record
         pofs : Part_Of_Speech_Type := prefix;
         fix  : fix_type := null_fix_type;
         connect : Character := ' ';
         entr : prefix_entry := null_prefix_entry;
         mean : Meaning_Type := Null_Meaning_Type;
      end record;

   null_prefix_line : prefix_line;

   package prefix_line_io is
      Default_Width : Natural;
      procedure Get(f : in File_Type; p : out prefix_line);
      procedure Get(p : out prefix_line);
      procedure Put(f : in File_Type; p : in prefix_line);
      procedure Put(p : in prefix_line);
      procedure Get(s : in String; p : out prefix_line; last : out Integer);
      procedure Put(s : out String; p : in prefix_line);
   end prefix_line_io;

   type suffix_line is
      record
         pofs : Part_Of_Speech_Type := suffix;
         fix  : fix_type := null_fix_type;
         connect    : Character := ' ';
         entr : suffix_entry := null_suffix_entry;
         mean : Meaning_Type := Null_Meaning_Type;
      end record;

   null_suffix_line : suffix_line;

   package suffix_line_io is
      Default_Width : Natural;
      procedure Get(f : in File_Type; p : out suffix_line);
      procedure Get(p : out suffix_line);
      procedure Put(f : in File_Type; p : in suffix_line);
      procedure Put(p : in suffix_line);
      procedure Get(s : in String; p : out suffix_line; last : out Integer);
      procedure Put(s : out String; p : in suffix_line);
   end suffix_line_io;

   type unique_entry is
      record
         stem : Stem_Type          := Null_Stem_Type;
         qual : quality_record     := null_quality_record;
         kind : Kind_Entry         := Null_Kind_Entry;
         tran : Translation_Record := Null_Translation_Record;
      end record;

   package unique_entry_io is
      Default_Width : Field;
      procedure Get(f : in File_Type; p : out unique_entry);
      procedure Get(p : out unique_entry);
      procedure Put(f : in File_Type; p : in unique_entry);
      procedure Put(p : in unique_entry);
      procedure Get(s : in String; p : out unique_entry; last : out Integer);
      procedure Put(s : out String; p : in unique_entry);
   end unique_entry_io;

   procedure load_stem_file(d_k : Dictionary_Kind);

   procedure load_dictionary(dict : in out dictionary;
                             dictionary_file_name : String);

   procedure load_uniques(unq : in out latin_uniques; file_name : in String);

end line_stuff;
