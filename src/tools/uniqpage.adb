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
with Strings_package; use Strings_package;
with latin_file_names; use latin_file_names;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with line_stuff; use line_stuff;
with dictionary_form;
procedure uniqpage is

   package Integer_IO is new text_io.Integer_IO (Integer);
   use text_io;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Kind_Entry_IO;
   use Translation_Record_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use geo_type_io;
   use frequency_type_io;
   use source_type_io;

   uniques_file, uniqpage : text_io.File_Type;

   s, line, blank_line, blanks : String (1 .. 400) := (others => ' ');
   l, ll, last : Integer := 0;

   stem : stem_type := null_stem_type;
   qual : quality_record;
   kind : kind_entry;
   tran : translation_record;
   mean : meaning_type;

   procedure get_line_unique (input : in text_io.File_Type; s : out String; last : out natural) is
   begin
      last := 0;
      text_io.Get_Line (input, s, last);
      if Trim (s (1 .. last)) /= ""  then   --  Rejecting blank lines
         null;
      end if;
   end get_line_unique;

begin
   Put_Line ("UNIQUES.LAT -> UNIQPAGE.PG");
   Put_Line ("Takes UNIQUES form, single lines it, puts # at begining,");
   Put_Line ("producing a .PG file for sorting to produce paper dictionary");
   Create (uniqpage, Out_File, "UNIQPAGE.PG");
   Open (uniques_file, In_File, "UNIQUES.LAT");

   Over_Lines :
    while not End_Of_File (uniques_file)  loop
       line := blanks;
       get_line_unique (uniques_file, line, last);      --  STEM
       stem := Head (Trim (line (1 .. last)), Max_Stem_Size);

       line := blanks;
       get_line_unique (uniques_file, line, last);    --  QUAL, KIND, TRAN
       quality_record_io.Get (line (1 .. last), qual, l);
       Get (line (l+1 .. last), qual.pofs, kind, l);
       Age_Type_IO.Get (line (l+1 .. last), tran.Age, l);
       Area_Type_IO.Get (line (l+1 .. last), tran.Area, l);
       geo_type_io.Get (line (l+1 .. last), tran.geo, l);
       frequency_type_io.Get (line (l+1 .. last), tran.freq, l);
       source_type_io.Get (line (l+1 .. last), tran.source, l);

       line := blanks;
       get_line_unique (uniques_file, line, l);         --  MEAN
       mean := Head (Trim (line (1 .. l)), Max_Meaning_Size);

       --      while not END_OF_FILE (UNIQUES_FILE) loop
       --         S := BLANK_LINE;
       --         GET_LINE (INPUT, S, LAST);
       --         if TRIM (S (1 .. LAST)) /= ""  then   --  Rejecting blank lines
       --
       --

       text_io.put (uniqpage, "#" & stem);

       quality_record_io.put (uniqpage, qual);

       -- PART := (V, (QUAL.V.CON, KIND.V_KIND));

       if (qual.pofs = v)  and then  (kind.v_kind in gen .. perfdef)  then
          text_io.put (uniqpage, "  " & verb_kind_type'image (kind.v_kind) & "  ");
       end if;

       text_io.put (uniqpage, " [");
       Age_Type_IO.put (uniqpage, tran.Age);
       Area_Type_IO.put (uniqpage, tran.Area);
       geo_type_io.put (uniqpage, tran.geo);
       frequency_type_io.put (uniqpage, tran.freq);
       source_type_io.put (uniqpage, tran.source);
       text_io.put (uniqpage, "]");

       put (uniqpage, " :: ");
       Put_Line (uniqpage, mean);

       --end if;  --  Rejecting blank lines
    end loop Over_Lines;

    Close (uniqpage);
exception
   when text_io.data_error  =>
      null;
   when others =>
      Put_Line (s (1 .. last));
      Close (uniqpage);

end uniqpage;
