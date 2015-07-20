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
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
--with latin_file_names; use latin_file_names;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
--with line_stuff; use line_stuff;
procedure listord is
   --   LISTORD    Takes # (DICTORD) long format to ED file
   --   (3 lines per entry so it is all on one screen)
   --   LISTORD.IN -> LISTORD.OUT
   package Integer_IO is new Text_IO.Integer_IO (Integer);
   use Text_IO;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Translation_Record_IO;
   use Kind_Entry_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;

   Start_Stem_1  : constant := 81;
   Start_Stem_2  : constant := Start_Stem_1 + Max_Stem_Size + 1;
   Start_Stem_3  : constant := Start_Stem_2 + Max_Stem_Size + 1;
   Start_Stem_4  : constant := Start_Stem_3 + Max_Stem_Size + 1;
   start_part    : constant := Start_Stem_4 + Max_Stem_Size + 1;
   start_tran    : constant Integer :=
     start_part +
     Integer (Part_Entry_IO.Default_Width + 1);

   input, output : Text_IO.File_Type;
   de : Dictionary_Entry;

   s : String (1 .. 400) := (others => ' ');
   blank_line : constant String (1 .. 400) := (others => ' ');
   l, last : Integer := 0;
   j : Natural := 0;

begin
   Put_Line ("LISTORD    Takes # (DICTORD) long format to ED file");
   Put_Line ("(3 lines per entry so it is all on one screen)");
   Put_Line ("LISTORD.IN -> LISTORD.OUT");

   Create (output, Out_File, "LISTORD.OUT");
   Open (input, In_File, "LISTORD.IN");

   Over_Lines :
   while not End_Of_File (input) loop
      j := j + 1;
      s := blank_line;
      Get_Line (input, s, last);
      if Trim (s (1 .. last)) /= ""  then   --  Rejecting blank lines

         Form_De :
         begin

            de.Stems (1) :=
              s (Start_Stem_1 .. Start_Stem_1 + Max_Stem_Size - 1);
            de.Stems (2) :=
              s (Start_Stem_2 .. Start_Stem_2 + Max_Stem_Size - 1);
            de.Stems (3) :=
              s (Start_Stem_3 .. Start_Stem_3 + Max_Stem_Size - 1);
            de.Stems (4) :=
              s (Start_Stem_4 .. Start_Stem_4 + Max_Stem_Size - 1);

            Get (s (start_part .. last), de.Part, l);
            --GET (S (L + 1 .. LAST), DE.PART.POFS, DE.KIND, L);
            Get (s (l + 1 .. last), de.Tran.Age, l);
            Get (s (l + 1 .. last), de.Tran.Area, l);
            Get (s (l + 1 .. last), de.Tran.Geo, l);
            Get (s (l + 1 .. last), de.Tran.Freq, l);
            Get (s (l + 1 .. last), de.Tran.Source, l);
            de.Mean := Head (s (l + 2 .. last), Max_Meaning_Size);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is STRING,
            --    not ENUM

         exception
            when others =>
               Put_Line ("Exception");
               Put_Line (s (1 .. last));
               Integer_IO.Put (Integer (j)); New_Line;
               Put (de); New_Line;
               raise;
         end Form_De;

         Put_Line (output, s (1 .. 78));
         Put_Line (output, s (Start_Stem_1 .. start_part - 1));
         Put (output, s (start_part .. start_tran - 1)); Put (output, "      ");
         Put (output, de.Tran.Age); Put (output, " ");
         Put (output, de.Tran.Area); Put (output, " ");
         Put (output, de.Tran.Geo); Put (output, " ");
         Put (output, de.Tran.Freq); Put (output, " ");
         Put (output, de.Tran.Source); New_Line (output);
         Put_Line (output, Trim (de.Mean));

      end if;  --  Rejecting blank lines
   end loop Over_Lines;

   Close (output);
exception
   when Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line (s (1 .. last));
      Integer_IO.Put (Integer (j)); New_Line;
      Close (output);

end listord;
