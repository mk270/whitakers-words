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

with text_io;
with direct_io;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
package english_support_package is

   eword_size    : constant := 24;
   aux_word_size : constant := 12;
   line_number_width : constant := 10;
   priority_width : constant := 3;

   subtype eword is string(1..eword_size);
   null_eword : eword := (others => ' ');
   subtype auxword is string(1..aux_word_size);
   null_auxword : auxword := (others => ' ');
   subtype priority_type is integer range 0..99;

   number_of_ewords : integer := 0;

   type ewds_record is
      record
         w    : eword := null_eword;
         aux  : auxword := null_auxword;
         n    : integer := 0;
         pofs : part_of_speech_type := x;
         freq : frequency_type := x;
         semi : integer := 0;
         kind : integer := 0;
         rank : integer := 0;
      end record;

   null_ewds_record : ewds_record := ((others => ' '),
                                      (others => ' '), 0, x, x, 0, 0, 0);

   type ewds_array is array (positive range <>) of ewds_record;

   package ewds_direct_io is new direct_io(ewds_record);

   package ewds_record_io is
      default_width : natural;
      procedure get(f : in text_io.file_type; p : out ewds_record);
      procedure get(p : out ewds_record);
      procedure put(f : in text_io.file_type; p : in ewds_record);
      procedure put(p : in ewds_record);
      procedure get(s : in string; p : out ewds_record;
                                   last : out integer);
      procedure put(s : out string; p : in ewds_record);
   end ewds_record_io;

   english_dictionary_available : array (dictionary_kind) of boolean := (false,
                                                                         false, false, false, false, false, false,  --  don't SEARCH
                                                                         false, false, false, false);

   ewds_file : ewds_direct_io.file_type;

end english_support_package;
