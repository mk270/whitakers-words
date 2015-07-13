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
package body Stem_Type_IO is

   ---------------------------------------------------------------------------

   procedure Get (File : in File_Type; Item : out Stem_Type) is
      C : Character := ' ';
   begin
      Item := Null_Stem_Type;
      for K in 1 .. Stem_Type_IO.Default_Width loop
         Get (File, C);
         if (C not in 'A' .. 'Z') and (C not in 'a' .. 'z') then
            exit;
         else
            Item (K) := C;
         end if;
      end loop;
   end Get;

   ---------------------------------------------------------------------------

   procedure Get (Item : out Stem_Type) is
      C : Character := ' ';
   begin
      Item := Null_Stem_Type;
      for K in 1 .. Stem_Type_IO.Default_Width loop
         Ada.Text_IO.Get (C);
         if (C not in 'A' .. 'Z') and (C not in 'a' .. 'z') then
            exit;
         else
            Item (K) := C;
         end if;
      end loop;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (File : in File_Type; Item : in Stem_Type) is
   begin
      Ada.Text_IO.Put (File, Item);
   end Put;

   ---------------------------------------------------------------------------

   procedure Put (Item : in Stem_Type) is
   begin
      Ada.Text_IO.Put (Item);
   end Put;

   ---------------------------------------------------------------------------

   procedure Get
      (Source : in  String;
       Target : out Stem_Type;
       Last   : out Integer
      )
   is
      C : Character;
   begin
      Target := Null_Stem_Type;
      Last := 0;
      for K in 1 .. Stem_Type_IO.Default_Width loop
         C := Source (K);
         if (C not in 'A' .. 'Z') and (C not in 'a' .. 'z')  then
            exit;
         else
            Target (K) := C;
            Last := K;
         end if;
      end loop;
   end Get;

   ---------------------------------------------------------------------------

   procedure Put (Target : out String; Item : in Stem_Type) is
   begin
      Target (Target'First .. Target'First + Stem_Type_IO.Default_Width - 1) :=
         Item;
   end Put;

   ---------------------------------------------------------------------------

end Stem_Type_IO;
