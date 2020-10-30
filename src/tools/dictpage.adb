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
-- with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
-- with Support_Utils.Line_Stuff; use Support_Utils.Line_Stuff;
with Support_Utils.Dictionary_Form;
procedure Dictpage is
   --  DICTPAGE.IN -> DICTPAGE.OUT
   --  Takes DICTLINE form, puts # and dictionary form at beginning,
   --  a file that can be sorted to produce word order of paper dictionary
   package Integer_IO is new Text_IO.Integer_IO (Integer);
   use Text_IO;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;

   Start_Stem_1  : constant := 1;
   Start_Stem_2  : constant := Start_Stem_1 + Max_Stem_Size + 1;
   Start_Stem_3  : constant := Start_Stem_2 + Max_Stem_Size + 1;
   Start_Stem_4  : constant := Start_Stem_3 + Max_Stem_Size + 1;
   Start_Part    : constant := Start_Stem_4 + Max_Stem_Size + 1;

   Input, Output : Text_IO.File_Type;
   De : Dictionary_Entry;

   S : String (1 .. 400) := (others => ' ');
   Blank_Line : constant String (1 .. 400) := (others => ' ');
   L, Last : Integer := 0;
   J : constant Integer := 1;

begin
   Put_Line ("DICTPAGE.IN -> DICTPAGE.OUT");
   Put_Line
     ("Takes DICTLINE form, puts # and dictionary form at beginning, a file");
   Put_Line
     ("for sorting with ::, " &
     "a DICTPAGE.RAW to process for paper-like dictionary");
   Put_Line ("Process result with PAGE2HTM to create a pretty display");

   Create (Output, Out_File, "DICTPAGE.OUT");
   Open (Input, In_File, "DICTPAGE.IN");

   Over_Lines :
   while not End_Of_File (Input) loop
      S := Blank_Line;
      Get_Line (Input, S, Last);
      if Trim (S (1 .. Last)) /= ""  then   --  Rejecting blank lines

         Form_De :
         begin

            De.Stems (1) :=
              S (Start_Stem_1 .. Max_Stem_Size);
            De.Stems (2) :=
              S (Start_Stem_2 .. Start_Stem_2 + Max_Stem_Size - 1);
            De.Stems (3) :=
              S (Start_Stem_3 .. Start_Stem_3 + Max_Stem_Size - 1);
            De.Stems (4) :=
              S (Start_Stem_4 .. Start_Stem_4 + Max_Stem_Size - 1);

            Get (S (Start_Part .. Last), De.Part, L);
            Get (S (L + 1 .. Last), De.Tran.Age, L);
            Get (S (L + 1 .. Last), De.Tran.Area, L);
            Get (S (L + 1 .. Last), De.Tran.Geo, L);
            Get (S (L + 1 .. Last), De.Tran.Freq, L);
            Get (S (L + 1 .. Last), De.Tran.Source, L);
            De.Mean := Head (S (L + 2 .. Last), Max_Meaning_Size);
            --  Note that this allows initial blanks
            --  L+2 skips over the SPACER, required because this is STRING,
            --    not ENUM

         exception
            when others =>
               Put_Line ("Exception");
               Put_Line (S (1 .. Last));
               Integer_IO.Put (J); New_Line;
               Put (De); New_Line;
         end Form_De;

         Put (Output, "#" & Support_Utils.Dictionary_Form (De));

         Text_IO.Put (Output, " [");
         Age_Type_IO.Put (Output, De.Tran.Age);
         Area_Type_IO.Put (Output, De.Tran.Area);
         Geo_Type_IO.Put (Output, De.Tran.Geo);
         Frequency_Type_IO.Put (Output, De.Tran.Freq);
         Source_Type_IO.Put (Output, De.Tran.Source);
         Text_IO.Put (Output, "]");

         Put (Output, " :: ");
         Put_Line (Output, De.Mean);

      end if;  --  Rejecting blank lines
   end loop Over_Lines;

   Close (Output);
exception
   when Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line (S (1 .. Last));
      Integer_IO.Put (J); New_Line;
      Close (Output);

end Dictpage;
