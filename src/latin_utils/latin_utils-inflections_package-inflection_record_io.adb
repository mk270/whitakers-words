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

separate (Latin_Utils.Inflections_Package)
package body Inflection_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Inflection_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Quality_Record_IO.Get (File, Item.Qual);
      Get (File, Spacer);
      Stem_Key_Type_IO.Get (File, Item.Key);
      Get (File, Spacer);
      Ending_Record_IO.Get (File, Item.Ending);
      Get (File, Spacer);
      Age_Type_IO.Get (File, Item.Age);
      Get (File, Spacer);
      Frequency_Type_IO.Get (File, Item.Freq);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Inflection_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Quality_Record_IO.Get (Item.Qual);
      Get (Spacer);
      Stem_Key_Type_IO.Get (Item.Key);
      Get (Spacer);
      Ending_Record_IO.Get (Item.Ending);
      Get (Spacer);
      Age_Type_IO.Get (Item.Age);
      Get (Spacer);
      Frequency_Type_IO.Get (Item.Freq);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Inflection_Record) is
   begin
      Quality_Record_IO.Put (File, Item.Qual);
      Put (File, ' ');
      Stem_Key_Type_IO.Put (File, Item.Key, 1);
      Put (File, ' ');
      Ending_Record_IO.Put (File, Item.Ending);
      Put (File, ' ');
      Age_Type_IO.Put (File, Item.Age);
      Put (File, ' ');
      Frequency_Type_IO.Put (File, Item.Freq);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Inflection_Record) is
   begin
      Quality_Record_IO.Put (Item.Qual);
      Put (' ');
      Stem_Key_Type_IO.Put (Item.Key, 1);
      Put (' ');
      Ending_Record_IO.Put (Item.Ending);
      Put (' ');
      Age_Type_IO.Put (Item.Age);
      Put (' ');
      Frequency_Type_IO.Put (Item.Freq);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
     (Source : in  String;
      Target : out Inflection_Record;
      Last   : out Integer
     )
   is
      Pe  : Inflection_Record;
      Low : Integer := Source'First - 1;
   begin
      Last   := 0;
      Target := Pe;
      Quality_Record_IO.Get (Source (Low + 1 .. Source'Last), Target.Qual, Low);
      Low := Low + 1;
      Stem_Key_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Key, Low);
      Low := Low + 1;
      Ending_Record_IO.Get
         (Source (Low + 1 .. Source'Last), Target.Ending, Low);
      Low := Low + 1;
      Age_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Age, Low);
      Low := Low + 1;
      Frequency_Type_IO.Get
         (Source (Low + 1 .. Source'Last), Target.Freq, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Inflection_Record)
   is
      -- Used to compute bounds of substrings
      Low  : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Quality_Record
      High := Low + Quality_Record_IO.Default_Width;
      Quality_Record_IO.Put (Target (Low + 1 .. High), Item.Qual);

      -- Put Stem_Key_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + 1;
      Stem_Key_Type_IO.Put (Target (Low + 1 .. High), Item.Key);

      -- Put Ending_Record
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Ending_Record_IO.Default_Width;
      Ending_Record_IO.Put (Target (Low + 1 .. High), Item.Ending);

      -- Put Age_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + 1;
      Age_Type_IO.Put (Target (Low + 1 .. High), Item.Age);

      -- Put Frequence_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + 1;
      Frequency_Type_IO.Put (Target (Low + 1 .. High), Item.Freq);

      -- Fill remainder of String
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Inflection_Record_IO;
