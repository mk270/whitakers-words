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

separate (Dictionary_Package)
package body Translation_Record_IO is
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;

   ---------------------------------------------------------------------------

   Spacer : Character := ' ';
   --LINE : STRING(1..250);

   ---------------------------------------------------------------------------

   procedure Get (File : in Ada.Text_IO.File_Type; Item: out Translation_Record)
   is
   begin
      Get (File, Item.Age);
      Get (File, Spacer);
      Get (File, Item.Area);
      Get (File, Spacer);
      Get (File, Item.Geo);
      Get (File, Spacer);
      Get (File, Item.Freq);
      Get (File, Spacer);
      Get (File, Item.Source);
      --GET(F, SPACER);
      --GET_LINE(F, LINE, LAST);
      --TR.MEAN := HEAD(LINE(1..LAST), MAX_MEANING_SIZE);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Translation_Record) is
   begin
      Get (Item.Age);
      Get (Spacer);
      Get (Item.Area);
      Get (Spacer);
      Get (Item.Geo);
      Get (Spacer);
      Get (Item.Freq);
      Get (Spacer);
      Get (Item.Source);
      --GET(SPACER);
      --GET_LINE(LINE, LAST);
      --TR.MEAN := HEAD(LINE(1..LAST), MAX_MEANING_SIZE);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in Text_IO.File_Type; Item : in Translation_Record) is
   begin
      Put (File, Item.Age);
      Put (File, ' ');
      Put (File, Item.Area);
      Put (File, ' ');
      Put (File, Item.Geo);
      Put (File, ' ');
      Put (File, Item.Freq);
      Put (File, ' ');
      Put (File, Item.Source);
      --PUT(F, ' ');
      --PUT(F, TR.MEAN);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Translation_Record) is
   begin
      Age_Type_IO.Put (Item.Age);
      Text_IO.Put (' ');
      Area_Type_IO.Put (Item.Area);
      Text_IO.Put (' ');
      Geo_Type_IO.Put (Item.Geo);
      Text_IO.Put (' ');
      Frequency_Type_IO.Put (Item.Freq);
      Text_IO.Put (' ');
      Source_Type_IO.Put (Item.Source);
      --TEXT_IO.PUT(' ');
      --TEXT_IO.PUT(TR.MEAN);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      ( Source : in  String;
        Target : out Translation_Record;
        Last   : out Integer
      )
   is
      -- Used to compute lower bound of string
      Low : Integer := Source'First - 1;
   begin
      Get (Source (Low + 1 .. Source'Last), Target.Age, Low);
      --PUT(TR.AGE); TEXT_IO.PUT('-');
      Low := Low + 1;
      Get (Source (Low + 1 .. Source'Last), Target.Area, Low);
      --PUT(TR.AREA); TEXT_IO.PUT('-');
      Low := Low + 1;
      Get (Source (Low + 1 .. Source'Last), Target.Geo, Low);
      --PUT(TR.GEO); TEXT_IO.PUT('-');
      Low := Low + 1;
      Get (Source (Low + 1 .. Source'Last), Target.Freq, Low);
      --PUT(TR.FREQ); TEXT_IO.PUT('-');
      Low := Low + 1;
      Get (Source (Low + 1 .. Source'Last), Target.Source, Last);
      --PUT(TR.SOURCE); TEXT_IO.PUT('-');
      --L := M + 1;
      --M := L + MAX_MEANING_SIZE;
      --TR.MEAN := HEAD(S(L+1..S'LAST), MAX_MEANING_SIZE);
      --LAST := M;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Translation_Record)
   is
      -- Used to compute bounds of substrings
      Low  : Integer := 0;
      High : Integer := 0;
   begin
      -- Put Age_Type
      High := Low + Age_Type_IO.Default_Width;
      Put (Target (Target'First + Low .. High), Item.Age);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Area_Type
      High := Low + Area_Type_IO.Default_Width;
      Put (Target (Low + 1 .. High), Item.Area);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Geo_Type
      High := Low + Geo_Type_IO.Default_Width;
      Put (Target (Target'First + Low .. High), Item.Geo);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Frequency_Type
      High := Low + Frequency_Type_IO.Default_Width;
      Put (Target (Low + 1 .. High), Item.Freq);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Source_Type
      High := Low + Source_Type_IO.Default_Width;
      Put (Target (Low + 1 .. High), Item.Source);
      --L := M + 1;
      --S(L) :=  ' ';
      --M := L + MAX_MEANING_SIZE;
      --S(L+1..M) :=  TR.MEAN;

      -- Fill remainder of string
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Translation_Record_IO;
