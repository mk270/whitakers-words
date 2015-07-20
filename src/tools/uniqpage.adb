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
with Strings_package; use Strings_package;
with latin_file_names; use latin_file_names;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with line_stuff; use line_stuff;
with dictionary_form;
procedure uniqpage is

   package Integer_IO is new Text_IO.Integer_IO (Integer);
   use Text_IO;
   use Dictionary_Entry_IO;
   use Party_Entry_IO;
   use Kind_Entry_IO;
   use Translation_Record_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;

   uniques_file, uniqpage : Text_IO.File_Type;

   s, line, blank_line, blanks : String (1 .. 400) := (others => ' ');
   l, ll, last : Integer := 0;

   stem : Stem_Type := Null_Stem_Type;
   qual : quality_record;
   kind : kind_entry;
   tran : translation_record;
   mean : Meaning_Type;

   procedure get_line_unique (input : in Text_IO.File_Type; s : out String; last : out Natural) is
   begin
      last := 0;
      Text_IO.Get_Line (input, s, last);
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
       Get (line (l + 1 .. last), qual.pofs, kind, l);
       Age_Type_IO.Get (line (l + 1 .. last), tran.Age, l);
       Area_Type_IO.Get (line (l + 1 .. last), tran.Area, l);
       Geo_Type_IO.Get (line (l + 1 .. last), tran.Geo, l);
       Frequency_Type_IO.Get (line (l + 1 .. last), tran.Freq, l);
       Source_Type_IO.Get (line (l + 1 .. last), tran.Source, l);

       line := blanks;
       get_line_unique (uniques_file, line, l);         --  MEAN
       mean := Head (Trim (line (1 .. l)), Max_Meaning_Size);

       --      while not END_OF_FILE (UNIQUES_FILE) loop
       --         S := BLANK_LINE;
       --         GET_LINE (INPUT, S, LAST);
       --         if TRIM (S (1 .. LAST)) /= ""  then   --  Rejecting blank lines
       --
       --

       Text_IO.put (uniqpage, "#" & stem);

       quality_record_io.put (uniqpage, qual);

       -- PART := (V, (QUAL.V.CON, KIND.V_KIND));

       if (qual.pofs = v)  and then  (kind.v_kind in gen .. perfdef)  then
          Text_IO.put (uniqpage, "  " & verb_kind_type'image (kind.v_kind) & "  ");
       end if;

       Text_IO.put (uniqpage, " [");
       Age_Type_IO.put (uniqpage, tran.Age);
       Area_Type_IO.put (uniqpage, tran.Area);
       Geo_Type_IO.put (uniqpage, tran.Geo);
       Frequency_Type_IO.put (uniqpage, tran.Freq);
       Source_Type_IO.put (uniqpage, tran.Source);
       Text_IO.put (uniqpage, "]");

       put (uniqpage, " :: ");
       Put_Line (uniqpage, mean);

       --end if;  --  Rejecting blank lines
    end loop Over_Lines;

    Close (uniqpage);
exception
   when Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line (s (1 .. last));
      Close (uniqpage);

end uniqpage;
