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
package body Verb_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Verb_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Decn_Record_IO.Get (File, Item.Con);
      Get (File, Spacer);
      Tense_Voice_Mood_Record_IO.Get (File, Item.Tense_Voice_Mood);
      Get (File, Spacer);
      Person_Type_IO.Get (File, Item.Person);
      Get (File, Spacer);
      Number_Type_IO.Get (File, Item.Number);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Verb_Record)
   is
      Spacer : Character := ' ';
      pragma Unreferenced (Spacer);
   begin
      Decn_Record_IO.Get (Item.Con);
      Get (Spacer);
      Tense_Voice_Mood_Record_IO.Get (Item.Tense_Voice_Mood);
      Get (Spacer);
      Person_Type_IO.Get (Item.Person);
      Get (Spacer);
      Number_Type_IO.Get (Item.Number);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Verb_Record) is
   begin
      Decn_Record_IO.Put (File, Item.Con);
      Put (File, ' ');
      Tense_Voice_Mood_Record_IO.Put (File, Item.Tense_Voice_Mood);
      Put (File, ' ');
      Person_Type_IO.Put (File, Item.Person);
      Put (File, ' ');
      Number_Type_IO.Put (File, Item.Number);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Verb_Record) is
   begin
      Decn_Record_IO.Put (Item.Con);
      Put (' ');
      Tense_Voice_Mood_Record_IO.Put (Item.Tense_Voice_Mood);
      Put (' ');
      Person_Type_IO.Put (Item.Person);
      Put (' ');
      Number_Type_IO.Put (Item.Number);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Verb_Record;
       Last   : out Integer
      )
   is
      -- Used for computing lower bound of substrings
      Low : Integer := Source'First - 1;
   begin
      Decn_Record_IO.Get (Source (Low + 1 .. Source'Last), Target.Con, Low);
      Low := Low + 1;
      Tense_Voice_Mood_Record_IO.Get
         (Source (Low + 1 .. Source'Last), Target.Tense_Voice_Mood, Low);
      Low := Low + 1;
      Person_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Person, Low);
      Low := Low + 1;
      Number_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Number, Last);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Verb_Record)
   is
      -- Used for computing bounds of substrings
      Low  : Integer := Target'First - 1;
      High : Integer := 0;
   begin
      -- Put Decn_Record
      High := Low + Decn_Record_IO.Default_Width;
      Decn_Record_IO.Put (Target (Low + 1 .. High), Item.Con);

      -- Put Tense_Voice_Mood_Record
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Tense_Voice_Mood_Record_IO.Default_Width;
      Tense_Voice_Mood_Record_IO.Put
         (Target (Low + 1 .. High), Item.Tense_Voice_Mood);

      -- Put Person_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Person_Type_IO.Default_Width;
      Person_Type_IO.Put (Target (Low + 1 .. High), Item.Person);

      -- Put Number_Type
      Low := High + 1;
      Target (Low) :=  ' ';
      High := Low + Number_Type_IO.Default_Width;
      Number_Type_IO.Put (Target (Low + 1 .. High), Item.Number);

      -- Fill remainder of String
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Verb_Record_IO;
