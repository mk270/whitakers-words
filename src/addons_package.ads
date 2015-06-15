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

with text_io; use type text_io.file_type;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
package addons_package is
   pragma elaborate_body;

   subtype fix_type is stem_type;
   null_fix_type : constant fix_type := null_stem_type;
   max_fix_size : constant := max_stem_size;

   subtype target_pofs_type is part_of_speech_type range x..v;

   type target_entry(pofs: target_pofs_type := x) is
      record
         case pofs is
            when n  =>
               n : noun_entry;
               --NOUN_KIND : NOUN_KIND_TYPE;
            when pron  =>
               pron : pronoun_entry;
               --PRONOUN_KIND : PRONOUN_KIND_TYPE;
            when pack  =>
               pack : propack_entry;
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

   null_target_entry : target_entry;

   package target_entry_io is
      default_width : natural;
      procedure get(f : in text_io.file_type; p : out target_entry);
      procedure get(p : out target_entry);
      procedure put(f : in text_io.file_type; p : in target_entry);
      procedure put(p : in target_entry);
      procedure get(s : in string; p : out target_entry; last : out integer);
      procedure put(s : out string; p : in target_entry);
   end target_entry_io;

   type tackon_entry is
      record
         base : target_entry;
      end record;

   null_tackon_entry : tackon_entry;

   package tackon_entry_io is
      default_width : natural;
      procedure get(f : in text_io.file_type; i : out tackon_entry);
      procedure get(i : out tackon_entry);
      procedure put(f : in text_io.file_type; i : in tackon_entry);
      procedure put(i : in tackon_entry);
      procedure get(s : in string; i : out tackon_entry; last : out integer);
      procedure put(s : out string; i : in tackon_entry);
   end tackon_entry_io;

   type prefix_entry is
      record
         root    : part_of_speech_type := x;
         target  : part_of_speech_type := x;
      end record;

   null_prefix_entry : prefix_entry;

   package prefix_entry_io is
      default_width : natural;
      procedure get(f : in text_io.file_type; p : out prefix_entry);
      procedure get(p : out prefix_entry);
      procedure put(f : in text_io.file_type; p : in prefix_entry);
      procedure put(p : in prefix_entry);
      procedure get(s : in string; p : out prefix_entry; last : out integer);
      procedure put(s : out string; p : in prefix_entry);
   end prefix_entry_io;

   type suffix_entry is
      record
         root       : part_of_speech_type := x;
         root_key   : stem_key_type := 0;
         target     : target_entry := null_target_entry;
         target_key : stem_key_type := 0;
      end record;

   null_suffix_entry : suffix_entry;

   package suffix_entry_io is
      default_width : natural;
      procedure get(f : in text_io.file_type; p : out suffix_entry);
      procedure get(p : out suffix_entry);
      procedure put(f : in text_io.file_type; p : in suffix_entry);
      procedure put(p : in suffix_entry);
      procedure get(s : in string; p : out suffix_entry; last : out integer);
      procedure put(s : out string; p : in suffix_entry);
   end suffix_entry_io;

   type tackon_item is
      record
         pofs: part_of_speech_type := tackon;
         tack : stem_type := null_stem_type;
         entr : tackon_entry := null_tackon_entry;
         mnpc : integer := 0;
      end record;

   null_tackon_item : tackon_item;

   type prefix_item is
      record
         pofs: part_of_speech_type := prefix;
         fix  : fix_type := null_fix_type;
         connect : character := ' ';
         entr : prefix_entry := null_prefix_entry;
         mnpc : integer := 0;
      end record;

   null_prefix_item : prefix_item;

   type suffix_item is
      record
         pofs: part_of_speech_type := suffix;
         fix  : fix_type := null_fix_type;
         connect    : character := ' ';
         entr : suffix_entry := null_suffix_entry;
         mnpc : integer := 0;
      end record;

   null_suffix_item : suffix_item;

   type prefix_array is array (integer range <>) of prefix_item;
   type tickon_array is array (integer range <>) of prefix_item;
   type suffix_array is array (integer range <>) of suffix_item;
   type tackon_array is array (integer range <>) of tackon_item;
   type means_array  is array (integer range <>) of meaning_type;
   --  To simulate a DICT_IO file, as used previously

   tackons  : tackon_array(1..20);
   packons  : tackon_array(1..25);
   tickons  : prefix_array(1..10);
   prefixes : prefix_array(1..130);
   suffixes : suffix_array(1..185);
   means    : means_array(1..370);

   number_of_tickons  : integer := 0;
   number_of_tackons  : integer := 0;
   number_of_packons  : integer := 0;
   number_of_prefixes : integer := 0;
   number_of_suffixes : integer := 0;

   procedure load_addons (file_name : in string);

   function subtract_tackon(w : string; x : tackon_item) return string;
   function subtract_prefix(w : string; x : prefix_item) return stem_type;
   function subtract_tickon(w : string; x : prefix_item) return stem_type
     renames subtract_prefix;
   function subtract_suffix(w : string; x : suffix_item) return stem_type;

   function add_prefix(stem : stem_type;
                       prefix : prefix_item) return stem_type;
   function add_suffix(stem : stem_type;
                       suffix : suffix_item) return stem_type;

end addons_package;
