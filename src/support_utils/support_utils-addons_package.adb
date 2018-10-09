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

with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Latin_Utils.Preface;
package body Support_Utils.Addons_Package is
   use Ada.Text_IO;
   use Part_Of_Speech_Type_IO;
   use Target_Entry_Io;
   use Stem_Key_Type_IO;

   function Equ (C, D : Character) return Boolean is
   begin
      if (D = 'u') or (D = 'v')  then
         return (C = 'u') or (C = 'v');
      else
         return C = D;
      end if;
   end Equ;

   function Equ (S, T : String) return Boolean is
   begin
      if S'Length /= T'Length  then
         return False;
      end if;

      for I in 1 .. S'Length  loop
         if not Equ (S (S'First + I - 1), T (T'First + I - 1))  then
            return False;
         end if;
      end loop;

      return True;
   end Equ;

   procedure Load_Addons (File_Name : in String) is
      use Tackon_Entry_Io;
      use Prefix_Entry_Io;
      use Suffix_Entry_Io;

      S : String (1 .. 100);
      L, Last, Tic, Pre, Suf, Tac, Pac : Integer := 0;
      Addons_File : Ada.Text_IO.File_Type;
      Pofs : Part_Of_Speech_Type;
      Mean : Meaning_Type := Null_Meaning_Type;
      M : Integer := 1;
      Tn : Tackon_Entry;
      Pm : Prefix_Item;
      Ts : Stem_Type;

      procedure Extract_Fix (S : in String;
                             Xfix : out Fix_Type; Xc : out Character) is
         St : constant String := Trim (S);
         L : constant Integer := St'Length;
         J : Integer := 0;
      begin
         for I in 1 .. L  loop
            J := I;
            exit when (I < L) and then (St (I + 1) = ' ');
         end loop;
         Xfix := Head (St (1 .. J), Max_Fix_Size);
         if J = L  then     --  there is no CONNECT CHARACTER
            Xc := ' ';
            return;
         else
            for I in J + 1 .. L  loop
               if St (I) /= ' '  then
                  Xc := St (I);
                  exit;
               end if;
            end loop;
         end if;
         return;
      end Extract_Fix;

   begin
      Open (Addons_File, In_File, File_Name);
      Preface.Put ("ADDONS");
      Preface.Put (" loading ");

      --FIXME this code looks like it's duplicated somewhere else

      --    if DICT_IO.IS_OPEN (DICT_FILE (D_K))  then
      --      DICT_IO.DELETE (DICT_FILE (D_K));
      --    end if;
      --    DICT_IO.CREATE (DICT_FILE (D_K), DICT_IO.INOUT_FILE,
      ---ADD_FILE_NAME_EXTENSION (DICT_FILE_NAME, DICTIONARY_KIND'IMAGE (D_K)));
      --       "");
      --
      while not End_Of_File (Addons_File)  loop

         Get_Non_Comment_Line (Addons_File, S, Last);
         --TEXT_IO.PUT_LINE (S (1 .. LAST));
         Get (S (1 .. Last), Pofs, L);
         case Pofs is
            when Tackon  =>
               Ts := Head (Trim (S (L + 1 .. Last)), Max_Stem_Size);

               Get_Line (Addons_File, S, Last);
               Get (S (1 .. Last), Tn, L);
               Get_Line (Addons_File, S, Last);
               Mean := Head (S (1 .. Last), Max_Meaning_Size);

               if  Tn.Base.Pofs = Pack   and then
                 (Tn.Base.Pack.Decl.Which = 1 or
                 Tn.Base.Pack.Decl.Which = 2)  and then
                 Mean (1 .. 9) = "PACKON w/"
               then
                  Pac := Pac + 1;
                  Packons (Pac).Pofs := Pofs;
                  Packons (Pac).Tack := Ts;
                  Packons (Pac).Entr := Tn;
                  --            DICT_IO.SET_INDEX (DICT_FILE (D_K), M);
                  --            DE.MEAN := MEAN;
                  --            DICT_IO.WRITE (DICT_FILE (D_K), DE);
                  Packons (Pac).MNPC := M;
                  Means (M) := Mean;
                  M := M + 1;
               else
                  Tac := Tac + 1;
                  Tackons (Tac).Pofs := Pofs;
                  Tackons (Tac).Tack := Ts;
                  Tackons (Tac).Entr := Tn;
                  --            DICT_IO.SET_INDEX (DICT_FILE (D_K), M);
                  --            DE.MEAN := MEAN;
                  --            DICT_IO.WRITE (DICT_FILE (D_K), DE);
                  --            --DICT_IO.WRITE (DICT_FILE (D_K), MEAN);
                  Tackons (Tac).MNPC := M;
                  Means (M) := Mean;
                  M := M + 1;
               end if;

               Number_Of_Packons  := Pac;
               Number_Of_Tackons  := Tac;

            when Prefix  =>

               Extract_Fix (S (L + 1 .. Last), Pm.Fix, Pm.Connect);
               Get_Line (Addons_File, S, Last);
               Get (S (1 .. Last), Pm.Entr, L);
               Get_Line (Addons_File, S, Last);
               Mean := Head (S (1 .. Last), Max_Meaning_Size);

               if Pm.Entr.Root = Pack then
                  Tic := Tic + 1;
                  Tickons (Tic).Pofs := Pofs;
                  Tickons (Tic).Fix  := Pm.Fix;
                  Tickons (Tic).Connect  := Pm.Connect;
                  Tickons (Tic).Entr := Pm.Entr;
                  --            DICT_IO.SET_INDEX (DICT_FILE (D_K), M);
                  --            DE.MEAN := MEAN;
                  --            DICT_IO.WRITE (DICT_FILE (D_K), DE);
                  --            --DICT_IO.WRITE (DICT_FILE (D_K), MEAN);
                  Tickons (Tic).MNPC := M;
                  Means (M) := Mean;
                  M := M + 1;
               else
                  Pre := Pre + 1;
                  Prefixes (Pre).Pofs := Pofs;
                  Prefixes (Pre).Fix  := Pm.Fix;
                  Prefixes (Pre).Connect  := Pm.Connect;
                  Prefixes (Pre).Entr := Pm.Entr;
                  --            DICT_IO.SET_INDEX (DICT_FILE (D_K), M);
                  --            DICT_IO.WRITE (DICT_FILE (D_K), DE);
                  --            --DICT_IO.WRITE (DICT_FILE (D_K), MEAN);
                  Prefixes (Pre).MNPC := M;
                  Means (M) := Mean;
                  M := M + 1;
               end if;

               Number_Of_Tickons  := Tic;
               Number_Of_Prefixes := Pre;

            when Suffix  =>
               Suf := Suf + 1;
               Suffixes (Suf).Pofs := Pofs;
               --TEXT_IO.PUT_LINE (S (1 .. LAST));
               Extract_Fix (S (L + 1 .. Last),
                 Suffixes (Suf).Fix, Suffixes (Suf).Connect);
               --TEXT_IO.PUT ("@1");
               Get_Line (Addons_File, S, Last);
               --TEXT_IO.PUT ("@2");
               --TEXT_IO.PUT_LINE (S (1 .. LAST) & "<");
               --TEXT_IO.PUT ("@2");
               Get (S (1 .. Last), Suffixes (Suf).Entr, L);
               --TEXT_IO.PUT ("@3");
               Get_Line (Addons_File, S, Last);
               --TEXT_IO.PUT ("@4");
               Mean := Head (S (1 .. Last), Max_Meaning_Size);
               --TEXT_IO.PUT ("@5");
               --
               --        DICT_IO.SET_INDEX (DICT_FILE (D_K), M);
               --        DE.MEAN := MEAN;
               --        DICT_IO.WRITE (DICT_FILE (D_K), DE);
               --        --DICT_IO.WRITE (DICT_FILE (D_K), MEAN);
               Suffixes (Suf).MNPC := M;
               Means (M) := Mean;
               M := M + 1;

               Number_Of_Suffixes := Suf;

            when others  =>
               Ada.Text_IO.Put_Line
                 ("Bad ADDON    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
               Ada.Text_IO.Put_Line (S (1 .. Last));
               raise Ada.Text_IO.Data_Error;
         end case;

      end loop;

      Preface.Put (Tac, 1); Preface.Put ("+");
      Preface.Put (Pac, 2); Preface.Put (" TACKONS ");
      Preface.Put (Tic, 1); Preface.Put ("+");
      Preface.Put (Pre, 3); Preface.Put (" PREFIXES ");
      Preface.Put (Suf, 3); Preface.Put (" SUFFIXES ");

      Preface.Set_Col (60); Preface.Put_Line ("--  Loaded correctly");
      Close (Addons_File);

   exception
      when Ada.Text_IO.Name_Error  =>
         Preface.Put_Line ("No ADDONS file ");
         null;
      when Ada.Text_IO.Data_Error  =>
         Preface.Put_Line (S (1 .. Last));
         Preface.Put_Line ("No further ADDONS read ");
         Close (Addons_File);
      when others      =>
         Preface.Put_Line ("Exception in LOAD_ADDONS");
         Preface.Put_Line (S (1 .. Last));
   end Load_Addons;

   function Subtract_Tackon (W : String; X : Tackon_Item) return String is
      Wd : constant String := Trim (W);
      L  : constant Integer := Wd'Length;
      Xf : constant String := Trim (X.Tack);
      Z  : constant Integer := Xf'Length;
   begin
      --PUT_LINE ("In SUB TACKON " & INTEGER'IMAGE (L) & INTEGER'IMAGE (Z));
      if Words_Mdev (Use_Tackons) and then
        L > Z  and then
        --WD (L-Z + 1 .. L) = XF (1 .. Z)  then
        Equ (Wd (L - Z + 1 .. L),  Xf (1 .. Z))
      then
         --PUT ("In SUBTRACT_TACKON we got a hit   "); PUT_LINE (X.TACK);
         return Wd (1 .. L - Z);
      else
         --PUT ("In SUBTRACT_TACKON    NO    hit   "); PUT_LINE (X.TACK);
         return W;
      end if;
   end Subtract_Tackon;

   function Subtract_Prefix (W : String; X : Prefix_Item) return Stem_Type is
      Wd : constant String := Trim (W);
      Xf : constant String := Trim (X.Fix);
      Z  : constant Integer := Xf'Length;
      St : Stem_Type := Head (Wd, Max_Stem_Size);
   begin
      if Words_Mdev (Use_Prefixes) and then
        X /= Null_Prefix_Item and then
        Wd'Length > Z  and then
        --WD (1 .. Z) = XF (1 .. Z)  and then
        Equ (Wd (1 .. Z),  Xf (1 .. Z)) and then
        ((X.Connect = ' ') or (Wd (Z + 1) = X.Connect))
      then
         St (1 .. Wd'Length - Z) := Wd (Z + 1 .. Wd'Last);
         St (Wd'Length - Z + 1 .. Max_Stem_Size) :=
           Null_Stem_Type (Wd'Length - Z + 1 .. Max_Stem_Size);
      end if;
      return St;
   end Subtract_Prefix;

   function Subtract_Suffix (W : String; X : Suffix_Item) return Stem_Type is
      Wd : constant String := Trim (W);
      L  : constant Integer := Wd'Length;
      Xf : constant String := Trim (X.Fix);
      Z  : constant Integer := Xf'Length;
      St : Stem_Type := Head (Wd, Max_Stem_Size);
   begin
      --PUT_LINE ("In SUBTRACT_SUFFIX  Z = " & INTEGER'IMAGE (Z) &
      --"  CONNECT >" & X.CONNECT & '<');
      if Words_Mdev (Use_Suffixes) and then
        X /= Null_Suffix_Item and then
        Wd'Length > Z  and then
        --WD (L-Z + 1 .. L) = XF (1 .. Z)  and then
        Equ (Wd (L - Z + 1 .. L),  Xf (1 .. Z))  and then
        ((X.Connect = ' ') or (Wd (L - Z) = X.Connect))
      then
         --PUT_LINE ("In SUBTRACT_SUFFIX we got a hit");
         St (1 .. Wd'Length - Z) := Wd (1 .. Wd'Length - Z);
         St (Wd'Length - Z + 1 .. Max_Stem_Size) :=
           Null_Stem_Type (Wd'Length - Z + 1 .. Max_Stem_Size);
      end if;
      return St;
   end Subtract_Suffix;

   function Add_Prefix (Stem : Stem_Type;
                        Prefix : Prefix_Item) return Stem_Type is
      Fpx : constant String := Trim (Prefix.Fix) & Stem;
   begin
      if Words_Mdev (Use_Prefixes)  then
         return Head (Fpx, Max_Stem_Size);
      else
         return Stem;
      end if;
   end Add_Prefix;

   function Add_Suffix (Stem : Stem_Type;
                        Suffix : Suffix_Item) return Stem_Type is
      Fpx : constant String := Trim (Stem) & Suffix.Fix;
   begin
      if Words_Mdev (Use_Suffixes)  then
         return Head (Fpx, Max_Stem_Size);
      else
         return Stem;
      end if;
   end Add_Suffix;

   package body Target_Entry_Io is separate;
   package body Tackon_Entry_Io is separate;
   package body Prefix_Entry_Io is separate;
   package body Suffix_Entry_Io is separate;

begin

   Prefix_Entry_Io.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
     Part_Of_Speech_Type_IO.Default_Width;
   Target_Entry_Io.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
     Numeral_Entry_IO.Default_Width; --  Largest

   Suffix_Entry_Io.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
     2 + 1 +
     Target_Entry_Io.Default_Width + 1 +
     2;
   Tackon_Entry_Io.Default_Width := Target_Entry_Io.Default_Width;

end Support_Utils.Addons_Package;
