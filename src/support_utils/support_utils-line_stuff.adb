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

with Ada.Strings.Fixed;
with Support_Utils.Word_Support_Package; use Support_Utils.Word_Support_Package;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Support_Utils.Char_Utils;
with Latin_Utils.Preface;
package body Support_Utils.Line_Stuff is

   procedure Load_Dictionary (Dict : in out Dictionary;
                              Dictionary_File_Name : String)  is
      --  For loading a DICTIONARY list from a file
      --  Only used now for DICT.LOC

      Dictionary_File : File_Type;
      Blk_Stem : constant Stem_Type := Null_Stem_Type;
      Sts : Stems_Type := Null_Stems_Type;
      Pt  : Part_Entry  := Null_Part_Entry;
      Tran : Translation_Record := Null_Translation_Record;
      Value : constant Numeral_Value_Type := 0;
      Mean : Meaning_Type := Null_Meaning_Type;

      Fc1, Fc2, Fc3, Fc4 : Character;

      Line, St_Line : String (1 .. 100) := (others => ' ');
      Blank_Line : constant String (1 .. 100) := (others => ' ');
      L, Ll, Lll, Last    : Integer := 0;
      Number_Of_Dictionary_Entries : Integer := 0;

      procedure Get_Stem (S : in String;
                          Stem : out Stem_Type; Last : out Integer) is
         I  : Integer := 1;
         L  : Integer := Ada.Strings.Fixed.Index_Non_Blank (S);
      begin
         Stem := Null_Stem_Type;
         --  Count until the first blank
         --  Return that String
         while L <= S'Last and then S (L) /= ' '  loop
            Stem (I) := S (L);
            I := I + 1;
            L := L + 1;
         end loop;
         --  Return  last
         Last := L;

      end Get_Stem;

   begin

      Open (Dictionary_File, In_File, Dictionary_File_Name);
      Preface.Put ("Dictionary loading");

      while not End_Of_File (Dictionary_File)  loop
         --TEXT_IO.PUT_LINE ("GETTING");
         St_Line := Blank_Line;
         Get_Non_Comment_Line (Dictionary_File, St_Line, Last);      --  STEMS

         Line := Blank_Line;
         --TEXT_IO.PUT ("1 ");
         Get_Non_Comment_Line (Dictionary_File, Line, L);
         --  PART
         --TEXT_IO.PUT ("2 ");
         Part_Entry_IO.Get (Line (1 .. L), Pt, Ll);
         --TEXT_IO.PUT ("3 ");
         ----  KIND_ENTRY_IO.GET (LINE (LL + 1 .. L), PT.POFS, KIND, LL);
         --TEXT_IO.PUT ("4 ");
         Translation_Record_IO.Get (Line (Ll + 1 .. L), Tran, Lll);
         --TEXT_IO.PUT ("5 ");
         --TEXT_IO.PUT_LINE ("READ PART");

         --  Specialize for parts
         --  If ADV then look if the CO is something other than X
         --  If so (like POS) then only that stem is active,
         --   and the others => xxx
         --  Same for ADJ
         --  If the ADJ or ADV stems have different first letters then make them
         --  different dictionary entries  --  Do this in LOAD and in DICT.DIC
         --TEXT_IO.PUT_LINE ("GETTING STEMS IN LOAD_DICTIONARY");

         Sts := Null_Stems_Type;
         Ll := 1;
         --  Extract up to 4 stems
         for I in 1 .. Number_Of_Stems (Pt.Pofs)  loop   --  EXTRACT STEMS
            Get_Stem (St_Line (Ll .. Last), Sts (I), Ll);
         end loop;

         --for I in 1 .. NUMBER_OF_STEMS (PT.POFS)  loop
         --  TEXT_IO.PUT (STS (I));
         --end loop;
         --TEXT_IO.NEW_LINE;

         Line := Blank_Line;
         Get_Non_Comment_Line (Dictionary_File, Line, L);         --  MEANING
         Mean := Head (Trim (Line (1 .. L)), Max_Meaning_Size);
         --TEXT_IO.PUT_LINE ("READ MEANING");

         --  Now take care of other first letters in a gross way
         Fc1 := Lower_Case (Sts (1)(1));
         Fc2 := Lower_Case (Sts (2)(1));
         Fc3 := Lower_Case (Sts (3)(1));
         Fc4 := Lower_Case (Sts (4)(1));

         Char_Utils.V_To_U_And_J_To_I (Fc1);
         Char_Utils.V_To_U_And_J_To_I (Fc2);
         Char_Utils.V_To_U_And_J_To_I (Fc3);
         Char_Utils.V_To_U_And_J_To_I (Fc4);

         if Pt.Pofs = N  then
            if Sts (2)(1) /= Sts (1)(1)  and then
              Sts (2)(1) /= ' '        and then
              Sts (2)(1 .. 3) /= ZZZ_Stem (1 .. 3)
            then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Sts (1), ZZZ_Stem, Blk_Stem, Blk_Stem),
                 --PT, KIND, TRAN, MEAN), DICT (FC1));
                 Pt, Tran, Mean), Dict (Fc1));
               Dict (Fc2) :=
                 new Dictionary_Item'(((ZZZ_Stem, Sts (2), Blk_Stem, Blk_Stem),
                 --PT, KIND, TRAN, MEAN), DICT (FC2) );
                 Pt, Tran, Mean), Dict (Fc2));
            else
               --DICT (FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
               Dict (Fc1) := new Dictionary_Item'((Sts, Pt, Tran, Mean),
                 Dict (Fc1));
            end if;

         elsif Pt.Pofs = Pron or Pt.Pofs = Pack then
            if Sts (2)(1) /= Sts (1)(1)  and then
              Sts (2)(1) /= ' '        and then
              Sts (2)(1 .. 3) /= ZZZ_Stem (1 .. 3)
            then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Sts (1), ZZZ_Stem, Blk_Stem, Blk_Stem),
                 --PT, KIND, TRAN, MEAN), DICT (FC1));
                 Pt, Tran, Mean), Dict (Fc1));
               Dict (Fc2) :=
                 new Dictionary_Item'(((ZZZ_Stem, Sts (2), Blk_Stem, Blk_Stem),
                 --PT, KIND, TRAN, MEAN), DICT (FC2));
                 Pt, Tran, Mean), Dict (Fc2));
            else
               --DICT (FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
               Dict (Fc1) := new Dictionary_Item'((Sts, Pt, Tran, Mean),
                 Dict (Fc1));
            end if;

         elsif Pt.Pofs = Adj then
            if Pt.Adj.Co = X then   --  X for all KINDs
               if (Sts (2)(1) /= Sts (1)(1) and then
                 Sts (2)(1) /= ' '  and then
                 Sts (2)(1 .. 3) /= ZZZ_Stem (1 .. 3)) or
                 (Sts (3)(1) /= Sts (1)(1) and then
                 Sts (3)(1) /= ' '  and then
                 Sts (3)(1 .. 3) /= ZZZ_Stem (1 .. 3)) or
                 (Sts (4)(1) /= Sts (1)(1) and then
                 Sts (4)(1) /= ' '  and then
                 Sts (4)(1 .. 3) /= ZZZ_Stem (1 .. 3))
               then
                  Dict (Fc1) :=
                    new Dictionary_Item'(((Sts (1), Blk_Stem,
                    Blk_Stem, Blk_Stem),
                    (Adj, (Pt.Adj.Decl, Pos)),
                    --KIND, TRAN, MEAN), DICT (FC1));
                    Tran, Mean), Dict (Fc1));
                  Dict (Fc2) :=
                    new Dictionary_Item'(((ZZZ_Stem, Sts (2),
                    Blk_Stem, Blk_Stem),
                    (Adj, (Pt.Adj.Decl, Pos)),
                    --KIND, TRAN, MEAN), DICT (FC2));
                    Tran, Mean), Dict (Fc2));
                  Dict (Fc3) :=
                    new Dictionary_Item'(((ZZZ_Stem, ZZZ_Stem,
                    Sts (3), Blk_Stem),
                    (Adj, (Pt.Adj.Decl, Comp)),
                    --KIND, TRAN, MEAN), DICT (FC3));
                    Tran, Mean), Dict (Fc3));
                  Dict (Fc4) :=
                    new Dictionary_Item'(((ZZZ_Stem, ZZZ_Stem,
                    ZZZ_Stem, Sts (4)),
                    (Adj, (Pt.Adj.Decl, Super)),
                    --KIND, TRAN, MEAN), DICT (FC4));
                    Tran, Mean), Dict (Fc4));
               end if;
            elsif Pt.Adj.Co = Pos then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Sts (1), Blk_Stem, Blk_Stem, Blk_Stem),
                 --(ADJ, (PT.ADJ.DECL, POS)), KIND, TRAN, MEAN),
                 (Adj, (Pt.Adj.Decl, Pos)), Tran, Mean),
                 Dict (Fc1));
               Dict (Fc2) :=
                 new Dictionary_Item'(((Blk_Stem,  Sts (2), Blk_Stem, Blk_Stem),
                 --(ADJ, (PT.ADJ.DECL, POS)), KIND, TRAN, MEAN),
                 (Adj, (Pt.Adj.Decl, Pos)), Tran, Mean),
                 Dict (Fc2));
            elsif Pt.Adj.Co = Comp then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Blk_Stem, Blk_Stem, Sts (1), Blk_Stem),
                 --(ADJ, (PT.ADJ.DECL, COMP)), KIND, TRAN, MEAN),
                 (Adj, (Pt.Adj.Decl, Comp)), Tran, Mean),
                 Dict (Fc1));
            elsif Pt.Adj.Co   = Super then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Blk_Stem, Blk_Stem, Blk_Stem, Sts (1)),
                 --(ADJ, (PT.ADJ.DECL, SUPER)), KIND, TRAN, MEAN),
                 (Adj, (Pt.Adj.Decl, Super)), Tran, Mean),
                 Dict (Fc1));

            else
               --DICT (FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
               Dict (Fc1) := new Dictionary_Item'((Sts, Pt, Tran, Mean),
                 Dict (Fc1));
            end if;

         elsif Pt.Pofs = Adv  then
            if Pt.Adv.Co   = X  then   --  X for all KINDs
               if (Sts (2)(1) /= Sts (1)(1) and then
                 Sts (2)(1) /= ' '  and then
                 Sts (2)(1 .. 3) /= ZZZ_Stem (1 .. 3)) or
                 (Sts (3)(1) /= Sts (1)(1) and then
                 Sts (3)(1) /= ' '  and then
                 Sts (3)(1 .. 3) /= ZZZ_Stem (1 .. 3))
               then
                  Dict (Fc1) :=
                    new Dictionary_Item'(((Sts (1),
                    Blk_Stem, Blk_Stem, Blk_Stem),
                    --(ADV, (CO => POS)), KIND, TRAN, MEAN), DICT (FC1));
                    (Adv, (Co => Pos)), Tran, Mean), Dict (Fc1));
                  Dict (Fc2) :=
                    new Dictionary_Item'(((Sts (2),
                    Blk_Stem, Blk_Stem, Blk_Stem),
                    --(ADV, (CO => COMP)), KIND, TRAN, MEAN), DICT (FC2));
                    (Adv, (Co => Comp)), Tran, Mean), Dict (Fc2));
                  Dict (Fc3) :=
                    new Dictionary_Item'(((Sts (3),
                    Blk_Stem, Blk_Stem, Blk_Stem),
                    --(ADV, (CO => SUPER)), KIND, TRAN, MEAN), DICT (FC3));
                    (Adv, (Co => Super)), Tran, Mean), Dict (Fc3));
               end if;
            elsif Pt.Adv.Co = Pos then          --  just a specific KIND
               Dict (Fc1) :=
                 new Dictionary_Item'(((Sts (1), Blk_Stem, Blk_Stem, Blk_Stem),
                 --(ADV, (CO => POS)), KIND, TRAN, MEAN),
                 (Adv, (Co => Pos)), Tran, Mean),
                 Dict (Fc1));
            elsif Pt.Adv.Co = Comp then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Blk_Stem, Sts (1), Blk_Stem, Blk_Stem),
                 --(ADV, (CO => COMP)), KIND, TRAN, MEAN),
                 (Adv, (Co => Comp)), Tran, Mean),
                 Dict (Fc1));
            elsif Pt.Adv.Co = Super then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Blk_Stem, Blk_Stem, Sts (1), Blk_Stem),
                 --(ADV, (CO => SUPER)), KIND, TRAN, MEAN),
                 (Adv, (Co => Super)), Tran, Mean),
                 Dict (Fc1));
            else
               --DICT (FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
               Dict (Fc1) := new Dictionary_Item'((Sts, Pt, Tran, Mean),
                 Dict (Fc1));
            end if;

         elsif Pt.Pofs = V  then
            if (Sts (2)(1) /= Sts (1)(1) and then
              Sts (2)(1) /= ' '  and then
              Sts (2)(1 .. 3) /= ZZZ_Stem (1 .. 3)) or
              (Sts (3)(1) /= Sts (1)(1) and then
              Sts (3)(1) /= ' '  and then
              Sts (3)(1 .. 3) /= ZZZ_Stem (1 .. 3)) or
              (Sts (4)(1) /= Sts (1)(1) and then
              Sts (4)(1) /= ' '  and then
              Sts (4)(1 .. 3) /= ZZZ_Stem (1 .. 3))
            then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Sts (1), ZZZ_Stem, ZZZ_Stem, ZZZ_Stem),
                 --PT, KIND, TRAN, MEAN), DICT (FC1));
                 Pt, Tran, Mean), Dict (Fc1));
               Dict (Fc2) :=
                 new Dictionary_Item'(((ZZZ_Stem, Sts (2), ZZZ_Stem, ZZZ_Stem),
                 --PT, KIND, TRAN, MEAN), DICT (FC2));
                 Pt, Tran, Mean), Dict (Fc2));
               Dict (Fc3) :=
                 new Dictionary_Item'(((ZZZ_Stem, ZZZ_Stem, Sts (3), ZZZ_Stem),
                 --PT, KIND, TRAN, MEAN), DICT (FC3));
                 Pt, Tran, Mean), Dict (Fc3));
               Dict (Fc4) :=
                 new Dictionary_Item'(((ZZZ_Stem, ZZZ_Stem, ZZZ_Stem, Sts (4)),
                 --PT, KIND, TRAN, MEAN), DICT (FC4));
                 Pt, Tran, Mean), Dict (Fc4));
            else
               --DICT (FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
               Dict (Fc1) := new Dictionary_Item'((Sts, Pt, Tran, Mean),
                 Dict (Fc1));
            end if;

         elsif Pt.Pofs = Num  then
            if Pt.Num.Sort = X  then   --  X for all KINDs
               if Sts (1)(1) /= ' ' and then
                 Sts (1)(1 .. 3) /= ZZZ_Stem (1 .. 3)
               then
                  Dict (Fc1) :=
                    new Dictionary_Item'(((Sts (1), Blk_Stem,
                    Blk_Stem, Blk_Stem),
                    --(NUM, (PT.NUM.DECL, CARD)), KIND, TRAN, MEAN),
                    (Num, (Pt.Num.Decl, Card, Value)), Tran, Mean),
                    Dict (Fc1));
               end if;
               if Sts (2)(1) /= ' ' and then
                 Sts (2)(1 .. 3) /= ZZZ_Stem (1 .. 3)
               then
                  Dict (Fc2) :=
                    new Dictionary_Item'(((ZZZ_Stem, Sts (2),
                    Blk_Stem, Blk_Stem),
                    --(NUM, ((0, 0), ORD)), KIND, TRAN, MEAN),
                    (Num, ((0, 0), Ord, Value)), Tran, Mean),
                    Dict (Fc2));
               end if;
               if Sts (3)(1) /= ' ' and then
                 Sts (3)(1 .. 3) /= ZZZ_Stem (1 .. 3)
               then
                  Dict (Fc3) :=
                    new Dictionary_Item'(((ZZZ_Stem, ZZZ_Stem,
                    Sts (3), Blk_Stem),
                    --(NUM, (PT.NUM.DECL, DIST)), KIND, TRAN, MEAN),
                    (Num, (Pt.Num.Decl, Dist, Value)), Tran, Mean),
                    Dict (Fc3));
               end if;
               if Sts (4)(1) /= ' ' and then
                 Sts (4)(1 .. 3) /= ZZZ_Stem (1 .. 3)
               then
                  Dict (Fc4) :=
                    new Dictionary_Item'(((ZZZ_Stem, ZZZ_Stem,
                    ZZZ_Stem, Sts (4)),
                    --(NUM, (PT.NUM.DECL, ADVERB)), KIND, TRAN, MEAN),
                    (Num, (Pt.Num.Decl, Adverb, Value)), Tran, Mean),
                    Dict (Fc4));
               end if;
            elsif Pt.Num.Sort = Card  then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Sts (1), Blk_Stem, Blk_Stem, Blk_Stem),
                 --(NUM, (PT.NUM.DECL, CARD)), KIND, TRAN, MEAN),
                 (Num, (Pt.Num.Decl, Card, Value)), Tran, Mean),
                 Dict (Fc1));
            elsif Pt.Num.Sort = Ord   then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Blk_Stem, Sts (1), Blk_Stem, Blk_Stem),
                 --(NUM, (PT.NUM.DECL, ORD)), KIND, TRAN, MEAN),
                 (Num, (Pt.Num.Decl, Ord, Value)), Tran, Mean),
                 Dict (Fc1));
            elsif Pt.Num.Sort = Dist  then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Blk_Stem, Blk_Stem, Sts (1), Blk_Stem),
                 --(NUM, (PT.NUM.DECL, DIST)), KIND, TRAN, MEAN),
                 (Num, (Pt.Num.Decl, Dist, Value)), Tran, Mean),
                 Dict (Fc1));
            elsif Pt.Num.Sort = Adverb  then
               Dict (Fc1) :=
                 new Dictionary_Item'(((Blk_Stem, Blk_Stem, Blk_Stem, Sts (1)),
                 --(NUM, (PT.NUM.DECL, ADVERB)), KIND, TRAN, MEAN),
                 (Num, (Pt.Num.Decl, Adverb, Value)), Tran, Mean),
                 Dict (Fc1));
            end if;

         else
            --DICT (FC1) := new DICTIONARY_ITEM'((STS, PT, KIND, TRAN, MEAN),
            Dict (Fc1) := new Dictionary_Item'((Sts, Pt, Tran, Mean),
              Dict (Fc1));

         end if;
         Number_Of_Dictionary_Entries := Number_Of_Dictionary_Entries + 1;
      end loop;
      Close (Dictionary_File);
      Preface.Set_Col (33); Preface.Put ("--  ");
      Preface.Put (Number_Of_Dictionary_Entries, 6);
      Preface.Put (" entries"); Preface.Set_Col (55);
      Preface.Put_Line ("--  Loaded correctly");
   exception
      when others   =>
         Preface.Put_Line ("    LOAD_DICTIONARY exception        !!!!!!!!!!");
         Preface.Put_Line (St_Line (1 .. Last));
         Preface.Put_Line (Line (1 .. L));
         Close (Dictionary_File);
         Preface.Set_Col (33); Preface.Put ("--  ");
         Preface.Put (Number_Of_Dictionary_Entries, 6);
         Preface.Put (" entries"); Preface.Set_Col (55);
         Preface.Put_Line ("--  Loaded anyway   ");
   end Load_Dictionary;

   procedure Load_Stem_File (D_K : Dictionary_Kind)  is
      --  This is used to load a dictionary access file, like DIC.LOC
      --  It uses the single first letter index rather than the two letter
      --  This dictionary must be searched with a somewhat different procedure
      --  Not used when one loads from a regular STEMFILE (which uses
      --  two letters)
      --use LATIN_DEBUG;
      use Stem_Io;
      use Dict_IO;
      I : Stem_Io.Count := 1;
      --M_P_R : MEANING_TYPE;
      M : Dict_IO.Positive_Count := 1;
      Dlc : Dictionary := Dict_Loc;
      --DS : DICTIONARY_STEM;
   begin
      --PUT_LINE ("LOAD_STEM_FILE for LOC");
      if Is_Open (Stem_File (D_K))  then
         Delete (Stem_File (D_K));
      end if;
      Create (Stem_File (D_K), Inout_File,
        Add_File_Name_Extension (Stem_File_Name,
        Dictionary_Kind'Image (D_K)));
      --PUT_LINE ("LOAD_STEM_FILE for LOC - Created STEM_FILE");
      if Is_Open (Dict_File (D_K))  then
         Delete (Dict_File (D_K));
      end if;
      Create (Dict_File (D_K), Inout_File,
        Add_File_Name_Extension (Dict_File_Name,
        Dictionary_Kind'Image (D_K)));
      --PUT_LINE ("LOAD_STEM_FILE for LOC - Created DICT_FILE");

      --PUT_LINE ("L_D_F  Start  M = " & INTEGER'IMAGE (INTEGER (M)));

      for Fc in Character range 'a' .. 'z'  loop
         --  LOAD_DICTIONARY should have assured that all v were in u
         --LATIN_DEBUG.PUT_LINE ("L_D_F  Entering FC loop");
         Ddlf (Fc, 'a', D_K) := I;
         Ddll (Fc, 'a', D_K) := 0;
         while Dlc (Fc) /= null  loop

            Dict_IO.Set_Index (Dict_File (D_K), M);
            -- %%%%%%%%%%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!%%%%%%%%%%%%%%%%%%%
            --PUT_LINE (DLC (FC).DE.TRAN.MEAN);
            -- M_P_R := DLC (FC).DE.TRAN.MEAN;
            --DICT_IO.WRITE (DICT_FILE (D_K), M_P_R);   --@@@@@@@@@@@@@@@@@@@@@
            Dict_IO.Write (Dict_File (D_K), Dlc (Fc).De);
            for K in Stem_Key_Type range 1 .. 4  loop
               if Dlc (Fc).De.Stems (K) /= Null_Stem_Type  and
                 Dlc (Fc).De.Stems (K) /= ZZZ_Stem
               then
                  Write (Stem_File (D_K),
                    (Dlc (Fc).De.Stems (K), Dlc (Fc).De.Part, K, M));
                  Ddll (Fc, 'a', D_K) := I;
                  I := I + 1;
               end if;
            end loop;
            Dlc (Fc) := Dlc (Fc).Succ;
            M := M + 1;
            --PUT_LINE ("L_D_F  22222  M = " & INTEGER'IMAGE (INTEGER (M)));
         end loop;
         --PUT_LINE ("L_D_F  33333  M = " & INTEGER'IMAGE (INTEGER (M)));
      end loop;
      --PUT_LINE ("L_D_F  44444  M = " & INTEGER'IMAGE (INTEGER (M)));
   end Load_Stem_File;

   package body Tackon_Line_Io is separate;
   package body Prefix_Line_Io is separate;
   package body Suffix_Line_Io is separate;
   package body Unique_Entry_Io is separate;

   procedure Load_Uniques (Unq : in out Latin_Uniques; File_Name : in String) is
      use Quality_Record_IO;
      use Kind_Entry_IO;
      use Dict_IO;

      Uniques_File : Ada.Text_IO.File_Type;
      Blanks : constant String (1 .. 100) := (others => ' ');
      Line, Stem_Line : String (1 .. 100) := (others => ' ');
      Last, L : Integer := 0;
      Stem : Stem_Type := Null_Stem_Type;
      Qual : Quality_Record;
      Kind : Kind_Entry;
      --PART : PART_ENTRY := NULL_PART_ENTRY;
      Tran : Translation_Record := Null_Translation_Record;
      MNPC : MNPC_Type := Null_MNPC;
      Mean : Meaning_Type := Null_Meaning_Type;
      M : Dict_IO.Positive_Count := 1;

      Number_Of_Uniques_Entries : Integer := 0;

   begin
      --TEXT_IO.PUT_LINE ("UNIQUES started");
      Ada.Text_IO.Open (Uniques_File, Ada.Text_IO.In_File, File_Name);
      Preface.Set_Col (1);
      Preface.Put ("UNIQUES file loading");

      --    if DICT_IO.IS_OPEN (DICT_FILE (D_K))  then
      --      DICT_IO.DELETE (DICT_FILE (D_K));
      --    end if;
      --    DICT_IO.CREATE (DICT_FILE (D_K), DICT_IO.INOUT_FILE,  "");
      --ADD_FILE_NAME_EXTENSION (DICT_FILE_NAME, DICTIONARY_KIND'IMAGE (D_K)));

      while not End_Of_File (Uniques_File)  loop
         Stem_Line := Blanks;
         Get_Line (Uniques_File, Stem_Line, Last);      --  STEM
         Stem := Head (Trim (Stem_Line (1 .. Last)), Max_Stem_Size);

         Line := Blanks;
         Get_Line (Uniques_File, Line, Last);    --  QUAL, KIND, TRAN
         Get (Line (1 .. Last), Qual, L);
         Get (Line (L + 1 .. Last), Qual.Pofs, Kind, L);
         -- FIXME: Why not Translation_Record_IO.Get ?
         Age_Type_IO.Get (Line (L + 1 .. Last), Tran.Age, L);
         Area_Type_IO.Get (Line (L + 1 .. Last), Tran.Area, L);
         Geo_Type_IO.Get (Line (L + 1 .. Last), Tran.Geo, L);
         Frequency_Type_IO.Get (Line (L + 1 .. Last), Tran.Freq, L);
         Source_Type_IO.Get (Line (L + 1 .. Last), Tran.Source, L);

         Line := Blanks;
         Get_Line (Uniques_File, Line, L);         --  MEAN
         Mean := Head (Trim (Line (1 .. L)), Max_Meaning_Size);
         --@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         declare
            Unique_De : Dictionary_Entry;
            Part      : Part_Entry         := Null_Part_Entry;
         begin
            case Part.Pofs is
               when N  =>
                  Part := (N, (Qual.Noun.Decl, Qual.Noun.Gender, Kind.N_Kind));
               when Pron =>
                  Part := (Pron, (Qual.Pron.Decl, Kind.Pron_Kind));
               when Pack =>
                  Part := (Pack, (Qual.Pack.Decl, Kind.Pack_Kind));
               when Adj =>
                  Part := (Adj, (Qual.Adj.Decl, Qual.Adj.Comparison));
               when Num =>
                  Part := (Num, (Qual.Num.Decl, Qual.Num.Sort, Kind.Num_Value));
               when Adv =>
                  Part := (Adv, (Co => Qual.Adv.Comparison));
               when V =>
                  Part := (V, (Qual.Verb.Con, Kind.V_Kind));
               when others  =>
                  Part := Null_Part_Entry;
            end case;

            Unique_De.Stems := (Stem,
              Null_Stem_Type, Null_Stem_Type, Null_Stem_Type);
            Unique_De.Part  :=  Part;
            --UNIQUE_DE.KIND  :=  KIND;
            Unique_De.Tran  :=  Tran;
            Unique_De.Mean  :=  Mean;

            --        DICT_IO.SET_INDEX (DICT_FILE (D_K), M);
            --        DICT_IO.WRITE (DICT_FILE (D_K), UNIQUE_DE);

            Uniques_De (M) := Unique_De;
         end;
         --@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

         MNPC := M;

         if Lower_Case (Stem (1)) = 'v' then
            Unq ('u') :=
              new Unique_Item'(Stem, Qual, Kind, MNPC, Unq (Lower_Case ('u')));
         elsif Lower_Case (Stem (1)) = 'j' then
            Unq ('i') :=
              new Unique_Item'(Stem, Qual, Kind, MNPC, Unq (Lower_Case ('i')));
         else
            Unq (Lower_Case (Stem (1))) :=
              new Unique_Item'(Stem, Qual, Kind, MNPC,
              Unq (Lower_Case (Stem (1))));
         end if;

         M := M + 1;
         Number_Of_Uniques_Entries := Integer (M) - 1;

      end loop;
      Close (Uniques_File);
      Preface.Set_Col (33);
      Preface.Put ("--  "); Preface.Put (Number_Of_Uniques_Entries, 6);
      Preface.Put (" entries");
      Preface.Set_Col (55); Preface.Put_Line ("--  Loaded correctly");
   exception
      when Ada.Text_IO.Name_Error  =>
         Preface.Put_Line ("There is no UNIQUES file");
      when others   =>
         Preface.New_Line;
         Preface.Put_Line
           ("LOAD_UNIQUES exception        !!!!!!!!!!!!!!!!!!!!!");
         Preface.Put_Line (Stem_Line (1 .. Last));
         Preface.Put_Line (Line (1 .. L));
         Close (Uniques_File);
         Preface.Set_Col (33);
         Preface.Put ("--  "); Preface.Put (Number_Of_Uniques_Entries, 6);
         Preface.Put (" entries");
         Preface.Set_Col (55); Preface.Put_Line ("--  Loaded before error");
         --raise;
   end Load_Uniques;

begin

   --  PARSE_LINE_IO.DEFAULT_WIDTH :=
   --                                   MAX_STEM_SIZE + 1 +
   --                                   INFLECTION_RECORD_IO.DEFAULT_WIDTH + 1 +
   --                                   DICTIONARY_KIND_IO.DEFAULT_WIDTH + 1 +
   --                                   MAX_MEANING_SIZE;

   Prefix_Line_Io.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
     Max_Stem_Size + 1 +
     1 + 1 +
     Prefix_Entry_Io.Default_Width + 1 +
     Max_Meaning_Size;
   Suffix_Line_Io.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
     Max_Stem_Size + 1 +
     1 + 1 +
     Suffix_Entry_Io.Default_Width + 1 +
     Max_Meaning_Size;
   Tackon_Line_Io.Default_Width := Part_Of_Speech_Type_IO.Default_Width + 1 +
     Max_Stem_Size + 1 +
     Tackon_Entry_Io.Default_Width + 1 +
     Max_Meaning_Size;

   Unique_Entry_Io.Default_Width := Max_Stem_Size + 1 +
     Inflection_Record_IO.Default_Width + 1 +
     Translation_Record_IO.Default_Width;

end Support_Utils.Line_Stuff;
