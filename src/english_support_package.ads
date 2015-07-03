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

with Text_IO;
with Direct_IO;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
package english_support_package is

   eword_size    : constant := 24;
   aux_word_size : constant := 12;
   line_number_width : constant := 10;
   priority_width : constant := 3;

   subtype eword is String(1..eword_size);
   null_eword : eword := (others => ' ');
   subtype auxword is String(1..aux_word_size);
   null_auxword : auxword := (others => ' ');
   subtype priority_type is Integer range 0..99;

   number_of_ewords : Integer := 0;

   type ewds_record is
      record
         w    : eword := null_eword;
         aux  : auxword := null_auxword;
         n    : Integer := 0;
         pofs : part_of_speech_type := x;
         freq : frequency_type := x;
         semi : Integer := 0;
         kind : Integer := 0;
         rank : Integer := 0;
      end record;

   null_ewds_record : ewds_record := ((others => ' '),
                                      (others => ' '), 0, x, x, 0, 0, 0);

   type ewds_array is array (Positive range <>) of ewds_record;

   package ewds_direct_io is new direct_io(ewds_record);

   package ewds_record_io is
      Default_Width : Natural;
      procedure Get(f : in Text_IO.File_Type; p : out ewds_record);
      procedure Get(p : out ewds_record);
      procedure Put(f : in Text_IO.File_Type; p : in ewds_record);
      procedure Put(p : in ewds_record);
      procedure Get(s : in String; p : out ewds_record;
                                   last : out Integer);
      procedure Put(s : out String; p : in ewds_record);
   end ewds_record_io;

   english_dictionary_available : array (dictionary_kind) of Boolean := (False,
                                                                         False, False, False, False, False, False,  --  don't SEARCH
                                                                         False, False, False, False);

   ewds_file : ewds_direct_io.File_Type;

end english_support_package;
