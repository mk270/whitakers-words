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

separate (Latin_Utils.Dictionary_Package)
package body Translation_Record_IO is

   ---------------------------------------------------------------------------

   procedure Get
     (File : in Ada.Text_IO.File_Type;
      Item : out Translation_Record)
   is
      Spacer : Character;
      pragma Unreferenced (Spacer);
   begin
      Age_Type_IO.Get (File, Item.Age);
      Ada.Text_IO.Get (File, Spacer);
      Area_Type_IO.Get (File, Item.Area);
      Ada.Text_IO.Get (File, Spacer);
      Geo_Type_IO.Get (File, Item.Geo);
      Ada.Text_IO.Get (File, Spacer);
      Frequency_Type_IO.Get (File, Item.Freq);
      Ada.Text_IO.Get (File, Spacer);
      Source_Type_IO.Get (File, Item.Source);
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Translation_Record)
   is
      Spacer : Character;
      pragma Unreferenced (Spacer);
   begin
      Age_Type_IO.Get (Item.Age);
      Ada.Text_IO.Get (Spacer);
      Area_Type_IO.Get (Item.Area);
      Ada.Text_IO.Get (Spacer);
      Geo_Type_IO.Get (Item.Geo);
      Ada.Text_IO.Get (Spacer);
      Frequency_Type_IO.Get (Item.Freq);
      Ada.Text_IO.Get (Spacer);
      Source_Type_IO.Get (Item.Source);
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in Ada.Text_IO.File_Type; Item : in Translation_Record)
   is
   begin
      Age_Type_IO.Put (File, Item.Age);
      Ada.Text_IO.Put (File, ' ');
      Area_Type_IO.Put (File, Item.Area);
      Ada.Text_IO.Put (File, ' ');
      Geo_Type_IO.Put (File, Item.Geo);
      Ada.Text_IO.Put (File, ' ');
      Frequency_Type_IO.Put (File, Item.Freq);
      Ada.Text_IO.Put (File, ' ');
      Source_Type_IO.Put (File, Item.Source);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Translation_Record) is
   begin
      Age_Type_IO.Put (Item.Age);
      Ada.Text_IO.Put (' ');
      Area_Type_IO.Put (Item.Area);
      Ada.Text_IO.Put (' ');
      Geo_Type_IO.Put (Item.Geo);
      Ada.Text_IO.Put (' ');
      Frequency_Type_IO.Put (Item.Freq);
      Ada.Text_IO.Put (' ');
      Source_Type_IO.Put (Item.Source);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Translation_Record;
       Last   : out Integer
      )
   is
      -- Used to compute lower bound of string
      Low : Integer := Source'First - 1;
   begin
      Age_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Age, Low);
      Low := Low + 1;
      Area_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Area, Low);
      Low := Low + 1;
      Geo_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Geo, Low);
      Low := Low + 1;
      Frequency_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Freq, Low);
      Low := Low + 1;
      Source_Type_IO.Get (Source (Low + 1 .. Source'Last), Target.Source, Last);
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
      Age_Type_IO.Put (Target (Target'First + Low .. High), Item.Age);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Area_Type
      High := Low + Area_Type_IO.Default_Width;
      Area_Type_IO.Put (Target (Low + 1 .. High), Item.Area);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Geo_Type
      High := Low + Geo_Type_IO.Default_Width;
      Geo_Type_IO.Put (Target (Target'First + Low .. High), Item.Geo);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Frequency_Type
      High := Low + Frequency_Type_IO.Default_Width;
      Frequency_Type_IO.Put (Target (Low + 1 .. High), Item.Freq);
      Low := High + 1;
      Target (Low) :=  ' ';

      -- Put Source_Type
      High := Low + Source_Type_IO.Default_Width;
      Source_Type_IO.Put (Target (Low + 1 .. High), Item.Source);

      -- Fill remainder of string
      Target (High + 1 .. Target'Last) := (others => ' ');
   end Put;

   ---------------------------------------------------------------------------

end Translation_Record_IO;
