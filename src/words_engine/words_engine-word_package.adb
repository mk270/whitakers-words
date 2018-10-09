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

--
-- This file still needs a lot of work.
-- To do:
--
--  * analyse all the things which can be factored back together
--  * factor together the 9 instances of "Sxx (M) := "
--  * factor together the two branches of Apply_Suffix ()
--

with Support_Utils.Addons_Package; use Support_Utils.Addons_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Config;  use Latin_Utils.Config;
with Support_Utils.Uniques_Package; use Support_Utils.Uniques_Package;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Latin_Utils.Preface;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Support_Utils.Line_Stuff; use Support_Utils.Line_Stuff;
with Words_Engine.English_Support_Package;
use Words_Engine.English_Support_Package;

package body Words_Engine.Word_Package is

   Inflections_Sections_File : Lel_Section_Io.File_Type;

   procedure Pause (Output : Ada.Text_IO.File_Type) is
      Pause_Line : String (1 .. 300);
      Pause_Last : Integer := 0;
   begin
      if Words_Mdev (Pause_In_Screen_Output)  then
         if Method = Interactive  then
            if Ada.Text_IO.Name (Output) =
              Ada.Text_IO.Name (Ada.Text_IO.Standard_Output)
            then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output,
               "                          MORE - hit RETURN/ENTER to continue");
               Ada.Text_IO.Get_Line
                 (Ada.Text_IO.Standard_Input, Pause_Line, Pause_Last);
            end if;
         elsif Method = Command_Line_Input  then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output,
              "                          MORE - hit RETURN/ENTER to continue");
            Ada.Text_IO.Get_Line (Ada.Text_IO.Standard_Input,
              Pause_Line, Pause_Last);
         elsif Method = Command_Line_Files  then
            null;                       --  Do not PAUSE
         end if;
      end if;
   exception
      when others  =>
         Ada.Text_IO.Put_Line ("Unexpected exception in PAUSE");
   end Pause;

   function Ltu (C, D : Character) return Boolean is
   begin
      case D is
         when 'v' =>
            return C < 'u';
         when 'j' =>
            return C < 'i';
         when 'V' =>
            return C < 'U';
         when 'J' =>
            return C < 'I';
         when others =>
            return C < D;
      end case;
   end Ltu;

   function Equ (C, D : Character) return Boolean is
   begin
      case D is
         when 'u' | 'v' =>
            return (C = 'u') or (C = 'v');
         when 'i' | 'j' =>
            return (C = 'i') or (C = 'j');
         when 'U' | 'V' =>
            return (C = 'U') or (C = 'V');
         when 'I' | 'J' =>
            return (C = 'I') or (C = 'J');
         when others =>
            return C = D;
      end case;
   end Equ;

   function Gtu (C, D : Character) return Boolean is
   begin
      case D is
         when 'u' =>
            return C > 'v';
         when 'i' =>
            return C > 'j';
         when 'U' =>
            return C > 'V';
         when 'I' =>
            return C > 'J';
         when others =>
            return C > D;
      end case;
   end Gtu;

   function Ltu (S, T : String) return Boolean is
   begin
      for I in 1 .. S'Length  loop   --  Not TRIMed, so same length
         if Equ (S (S'First + I - 1), T (T'First + I - 1))  then
            null;
         elsif Gtu (S (S'First + I - 1), T (T'First + I - 1))  then
            return False;
         elsif Ltu (S (S'First + I - 1), T (T'First + I - 1))  then
            return True;
         end if;
      end loop;
      return False;
   end Ltu;

   function Gtu (S, T : String) return Boolean is
   begin
      for I in 1 .. S'Length  loop   --  Not TRIMed, so same length
         if Equ (S (S'First + I - 1), T (T'First + I - 1))  then
            null;
         elsif Ltu (S (S'First + I - 1), T (T'First + I - 1))  then
            return False;
         elsif Gtu (S (S'First + I - 1), T (T'First + I - 1))  then
            return True;
         end if;
      end loop;
      return False;
   end Gtu;

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

   procedure Run_Uniques
     (S : in String;
      Pa : in out Parse_Array; Pa_Last : in out Integer)
   is
      Sl : constant String        --  BAD NAME!!!!!!!!!!!!!!!!!!
        := Lower_Case (Trim (S));
      St : constant Stem_Type := Head (Sl, Max_Stem_Size);
      Unql : Unique_List;   --  Unique list for a letter
   begin
      if Sl (Sl'First) = 'v'  then
         Unql := Unq ('u');   --  Unique list for a letter
      elsif Sl (Sl'First) = 'j'  then
         Unql := Unq ('i');   --  Unique list for a letter
      else
         Unql := Unq (Sl (Sl'First));   --  Unique list for a letter
      end if;

      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE ("Called UNIQUES with =>" & SL & "|");

      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE ("UNQL ");

      while Unql /= null  loop
         --  If there is a match, add to PA
         --TEXT_IO.PUT_LINE ("UNIQUE =>" & UNQL.PR.STEM);
         --if ST = LOWER_CASE (UNQL.PR.STEM)  then
         if Equ (St, Lower_Case (Unql.Stem)) then
            Pa_Last := Pa_Last + 1;
            Pa (Pa_Last) := (Unql.Stem,
              (Unql.Qual,
              0,
              Null_Ending_Record,
              X,
              X),
              Unique,
              Unql.MNPC);

         end if;
         Unql := Unql.Succ;
      end loop;

   end Run_Uniques;

   procedure Run_Inflections
     (S : in String;
      Sl : in out Sal;
      Restriction : Dict_Restriction := Regular)
   is
      --  Tries all possible inflections against the Input word in S
      --  and constructs a STEM_LIST of those that survive SL
      use Lel_Section_Io;
      Word : constant String := Lower_Case (Trim (S));
      Last_Of_Word : constant Character := Word (Word'Last);
      Length_Of_Word   : constant Integer := Word'Length;
      Stem_Length  : Integer := 0;
      Pr   : Parse_Record;
      M : Integer := 1;

   begin
      --TEXT_IO.NEW_LINE;
      --TEXT_IO.PUT_LINE ("Called RUN_INFLECTIONS with =>" & WORD & "|");
      if Word'Length = 0  then
         Sl (M) := Null_Parse_Record;
         return;
      end if;

      Sa := Not_A_Stem_Array;

      --  Add all of these to list of possible ending records
      --  since the blank ending agrees with everything
      --  PACK/PRON have no blank endings
      if ((Restriction /= Pack_Only) and (Restriction /= Qu_Pron_Only))
        and then (Word'Length <= Max_Stem_Size)
      then
         for I in Belf (0, ' ') .. Bell (0, ' ')  loop
            Pr := (Word & Null_Stem_Type
              (Length_Of_Word + 1 .. Stem_Type'Length),
              Bel (I), Default_Dictionary_Kind, Null_MNPC);
            Sl (M) := Pr;
            M := M + 1;
         end loop;

         --  Is always a possibility (null ending)
         Sa (Length_Of_Word) := Pr.Stem;
      end if;

      --  Here we read in the INFLECTIONS_SECTION that is applicable
      if Restriction = Regular  then
         case Last_Of_Word is
            when 'a' | 'c' | 'd' | 'e' | 'i'  =>
               Read (Inflections_Sections_File, Lel, 1);
            when 'm' | 'n' | 'o' | 'r'  =>
               Read (Inflections_Sections_File, Lel, 2);
            when 's'  =>
               Read (Inflections_Sections_File, Lel, 3);
            when 't' | 'u'  =>
               Read (Inflections_Sections_File, Lel, 4);
            when others  =>
               --PUT_LINE ("Only blank inflections are found");
               return;
         end case;
      elsif Restriction = Pack_Only  or Restriction = Qu_Pron_Only  then
         Read (Inflections_Sections_File, Lel, 4);
      end if;

      --  Now do the non-blank endings      --  Only go to LENGTH_OF_WORD
      for Z in reverse 1 .. Integer'Min (Max_Ending_Size, Length_Of_Word)  loop

         --  Check if Z agrees with a PDL SIZE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
         --  Maybe make PDL on size, if it has to be a list,
         --  or order by size if array
         if Lell (Z, Last_Of_Word) > 0  then   --  Any likely inflections at all

            for I in Lelf (Z, Last_Of_Word) .. Lell (Z, Last_Of_Word) loop
               if Equ (Lower_Case (Lel (I).Ending.Suf (1 .. Z)),
                 Lower_Case (Word (Word'Last - Z + 1 .. Word'Last)))
               then
                  --  Add to list of possible ending records
                  --STEM_LENGTH := WORD'LENGTH - LEL (I).ENDING.SIZE;
                  Stem_Length := Word'Length - Z;

                  if Stem_Length <= Max_Stem_Size  then
                     --  Reject too long words
                     --  Check if LEL IR agrees with PDL IR  !!!!!!!!
                     Pr := (Word (Word'First .. Stem_Length) &
                       Null_Stem_Type (Stem_Length + 1 .. Max_Stem_Size),
                       Lel (I), Default_Dictionary_Kind, Null_MNPC);
                     Sl (M) := Pr;
                     M := M + 1;

                     Sa (Stem_Length) := Pr.Stem;
                     --  Gets set dozens of times
                     --  Could order the endings by length (suffix sort)
                     --  so length changes slowly

                     --PUT_LINE ("LENGTH = " & INTEGER'IMAGE (STEM_LENGTH)
                     --& "   SA =>" & PR.STEM & "|");
                  end if;
               end if;
            end loop;
         end if;
      end loop;
   end Run_Inflections;

   procedure Try_To_Load_Dictionary (D_K : Dictionary_Kind) is
   begin
      Stem_Io.Open (Stem_File (D_K), Stem_Io.In_File,
        Add_File_Name_Extension (Stem_File_Name,
        Dictionary_Kind'Image (D_K)));
      Dict_IO.Open (Dict_File (D_K), Dict_IO.In_File,
        Add_File_Name_Extension (Dict_File_Name,
        Dictionary_Kind'Image (D_K)));
      Load_Indices_From_Indx_File (D_K);
      Dictionary_Available (D_K) := True;

   exception
      when others  =>
         Dictionary_Available (D_K) := False;
   end Try_To_Load_Dictionary;

   procedure Dictionary_Search (Ssa : Stem_Array_Type;
                                D_K : Dictionary_Kind;
                                Restriction : Dict_Restriction := Regular) is
      --  Prepares a PDL list of possible dictionary hits
      --  Search a dictionary (D_K) looking for all stems that match
      --  any of the stems that are physically possible with Latin inflections
      use Stem_Io;

      --type NAT_32 is Range 0 .. 2**31-1;   --###############
      J, J1, J2, Jj : Stem_Io.Count := 0;

      Index_On : constant String := Ssa (Ssa'Last);
      Index_First, Index_Last : Stem_Io.Count := 0;
      Ds : Dictionary_Stem;
      First_Try, Second_Try : Boolean := True;

      function First_Two (W : String) return String is
         --  'v' could be represented by 'u', like the
         --  new Oxford Latin Dictionary

         --  Fixes the first two letters of a word/stem which can be done right
         S : constant String := Lower_Case (W);
         Ss : String (W'Range) := W;

         function Ui (C : Character) return Character  is
         begin
            if C = 'v' then
               return 'u';
            elsif C = 'V' then
               return 'U';
            elsif C = 'j' then
               return 'i';
            elsif C = 'J' then
               return 'I';
            else
               return C;
            end if;
         end Ui;

      begin

         if S'Length = 1  then
            Ss (S'First) := Ui (W (S'First));
         else
            Ss (S'First)   := Ui (W (S'First));
            Ss (S'First + 1) := Ui (W (S'First + 1));
         end if;

         return Ss;
      end First_Two;

      procedure Load_Pdl is
      begin
         case Restriction is
            when Regular    =>
               if not (Ds.Part.Pofs = Pack  or
                 (Ds.Part.Pofs = Pron  and then
                 (Ds.Part.Pron.Decl.Which = 1)))
               then
                  Pdl_Index := Pdl_Index + 1;
                  Pdl (Pdl_Index) := Pruned_Dictionary_Item'(Ds, D_K);
               end if;

            when Pack_Only  =>
               if Ds.Part.Pofs = Pack  then
                  Pdl_Index := Pdl_Index + 1;
                  Pdl (Pdl_Index) := Pruned_Dictionary_Item'(Ds, D_K);
               end if;

            when Qu_Pron_Only  =>
               if Ds.Part.Pofs = Pron  and then
                 (Ds.Part.Pron.Decl.Which = 1)
               then
                  Pdl_Index := Pdl_Index + 1;
                  Pdl (Pdl_Index) := Pruned_Dictionary_Item'(Ds, D_K);
               end if;

            when others =>
               Pdl_Index := Pdl_Index + 1;
               Pdl (Pdl_Index) := Pruned_Dictionary_Item'(Ds, D_K);
         end case;

      end Load_Pdl;

   begin
      --  Now go through the dictionary list DL for the first letters
      --  and make a reduced dictionary list PDL

      if D_K = Local  then
         Index_First := First_Index ((First_Two (Index_On)(1), 'a'), D_K);
         Index_Last  := Last_Index ((First_Two (Index_On)(1), 'a'), D_K);
      else
         Index_First := First_Index (First_Two (Index_On), D_K);
         Index_Last  := Last_Index (First_Two (Index_On), D_K);
      end if;

      if Index_First > 0  and then Index_First <= Index_Last then

         J1 := Index_First;    --######################
         J2 := Index_Last;

         Stem_Array_Loop :
         for K in Ssa'Range  loop
            if Trim (Ssa (K))'Length > 1  then
               --  This may be checking for 0 and 1 letter SSAs which
               --  are done elsewhere

               if D_K = Local  then
                  --  Special processing for unordered DICT.LOC
                  for J in J1 .. J2  loop
                     --  Sweep exaustively through the scope
                     Set_Index (Stem_File (D_K), Stem_Io.Count (J));
                     Read (Stem_File (D_K), Ds);

                     if Equ (Lower_Case (Ds.Stem), Ssa (K))  then
                        Load_Pdl;
                     end if;
                  end loop;
               else                     --  Regular dictionaries
                  First_Try := True;

                  Second_Try := True;

                  J := (J1 + J2) / 2;

                  Binary_Search :
                  loop
                     if (J1 = J2 - 1) or (J1 = J2) then
                        if First_Try  then
                           J := J1;
                           First_Try := False;
                        elsif Second_Try  then
                           J := J2;
                           Second_Try := False;
                        else
                           Jj := J;
                           exit Binary_Search;
                        end if;
                     end if;

                     Set_Index (Stem_File (D_K), J);
                     Read (Stem_File (D_K), Ds);

                     if  Ltu (Lower_Case (Ds.Stem), Ssa (K))  then
                        J1 := J;
                        J := (J1 + J2) / 2;
                     elsif  Gtu (Lower_Case (Ds.Stem), Ssa (K))  then
                        J2 := J;
                        J := (J1 + J2) / 2;
                     else
                        for I in reverse J1 .. J  loop
                           Set_Index (Stem_File (D_K), Stem_Io.Count (I));
                           Read (Stem_File (D_K), Ds);

                           if Equ (Lower_Case (Ds.Stem), Ssa (K))  then
                              Jj := I;
                              Load_Pdl;

                           else
                              exit;
                           end if;
                        end loop;

                        for I in J + 1 .. J2  loop
                           Set_Index (Stem_File (D_K), Stem_Io.Count (I));
                           Read (Stem_File (D_K), Ds);

                           if Equ (Lower_Case (Ds.Stem), Ssa (K))  then
                              Jj := I;
                              Load_Pdl;

                           else
                              exit Binary_Search;
                           end if;
                        end loop;
                        exit Binary_Search;
                     end if;
                  end loop Binary_Search;
                  J1 := Jj;
                  J2 := Index_Last;
               end if;               --  On LOCAL check
            end if;               --  On LENGTH > 1
         end loop Stem_Array_Loop;
      end if;
   end Dictionary_Search;

   procedure Search_Dictionaries (Ssa : in Stem_Array_Type;
                                  Restriction : Dict_Restriction := Regular) is
      use Stem_Io;
      Fc : Character := ' ';
   begin
      Pdl := (others => Null_Pruned_Dictionary_Item);
      Pdl_Index := 0;
      --PUT_LINE ("Search for blank stems");
      --  BDL is always used, so it is loaded initially and not called from disk
      --  Check all stems of the dictionary entry against the reduced stems

      --  Determine if there is a pure blank "  " stem
      if Len (Ssa (Ssa'First)) = 0    then
         --  a size would help?
         --PUT ("HIT on blank stem   I = ");PUT ('1');
         --PUT ("  STEM = ");PUT_LINE (BDL (1).STEM);
         --PDL := new PRUNED_DICTIONARY_ITEM'(BDL (1), GENERAL, PDL);
         Pdl_Index := Pdl_Index + 1;
         Pdl (Pdl_Index) := Pruned_Dictionary_Item'(Bdl (1), General);
      end if;
      --  Now there is only one blank stem (2 of to_be),
      --  but need not always be so

      --  Determine if there is a blank stem  (SC = ' ')
      --  Prepare for the possibility that one stem is short but there
      --  are others
      Fc := ' ';
      if Ssa (Ssa'First)(1) = ' ' then
         if Ssa'Length > 1  and then Ssa (Ssa'First + 1)(2) = ' '  then
            Fc := Ssa (Ssa'First + 1)(1);
         end if;
      elsif Ssa (Ssa'First)(2) = ' '  then
         Fc := Ssa (Ssa'First)(1);
      end if;

      --  If there is a single letter stem  (FC /= ' ') then
      if Fc /= ' '  then
         for I in 2 .. Bdl_Last  loop
            --  Check all stems of the dictionary entry against the
            --  reduced stems
            --if LOWER_CASE (BDL (I).STEM (1)) = FC  then
            if Equ (Lower_Case (Bdl (I).Stem (1)),  Fc)  then
               Pdl_Index := Pdl_Index + 1;
               Pdl (Pdl_Index) := Pruned_Dictionary_Item'(Bdl (I), General);
            end if;
         end loop;
      end if;

      if Ssa'Length = 0  then
         --        PUT_LINE ("Empty stem array, don't bother searching");
         return;
         --      elsif LEN (SSA (SSA'LAST)) <= 1  then
         --        PUT_LINE ("No two letter stems, have done searching");
         --      else
         --        PUT_LINE ("Searching Dictionaries");
      end if;

      for D_K in Dictionary_Kind  loop
         if Dictionary_Available (D_K)  then
            if not Is_Open (Stem_File (D_K))  then
               Open (Stem_File (D_K), Stem_Io.In_File,
                 Add_File_Name_Extension (Stem_File_Name,
                 Dictionary_Kind'Image (D_K)));
            end if;
            Dictionary_Search (Ssa, D_K, Restriction);
            Close (Stem_File (D_K));  --??????
         end if;
      end loop;

   end Search_Dictionaries;

   procedure Change_Language (C : Character) is
   begin  if Upper_Case (C) = 'L'  then
      Language := Latin_To_English;
      Preface.Put_Line
        ("Language changed to " & Language_Type'Image (Language));
   elsif Upper_Case (C) = 'E'  then
      if English_Dictionary_Available (General)  then
         Language := English_To_Latin;
         Preface.Put_Line
           ("Language changed to " & Language_Type'Image (Language));
         Preface.Put_Line
           ("Input a single English word (+ part of speech - " &
            "N, ADJ, V, PREP, . .. )");
      else
         Preface.Put_Line ("No English dictionary available");
      end if;
   else
      Preface.Put_Line
        ("Bad LANGUAGE Input - no change, remains " &
         Language_Type'Image (Language));
   end if;
   exception
      when others  =>
         Preface.Put_Line ("Bad LANGUAGE Input - no change, remains " &
           Language_Type'Image (Language));
   end Change_Language;

   procedure Word (Raw_Word : in String;
                   Pa       : in out Parse_Array;
                   Pa_Last  : in out Integer)
   is
      Input_Word : constant String  := Lower_Case (Raw_Word);
      Pa_Save    : constant Integer := Pa_Last;

      procedure Order_Stems (Sx : in out Sal) is
         use Dict_IO;
         Hits : Integer := 0;
         Sl : Sal := Sx;
         Sl_Last : Integer := 0;
         Sm : Parse_Record;
      begin
         if Sx (1) = Null_Parse_Record  then
            return;
         end if;
         --PUT_LINE ("ORDERing_STEMS");

         for I in Sl'Range  loop
            exit when Sl (I) = Null_Parse_Record;
            Sl_Last := Sl_Last + 1;
         end loop;
         --PUT_LINE ("In ORDER  SL_LAST = " & INTEGER'IMAGE (SL_LAST));

         --  Bubble sort since this list should usually be very small (1-5)
         Hit_Loop :
         loop
            Hits := 0;

            Switch :
            begin
               --  Need to remove duplicates in ARRAY_STEMS
               --  This sort is very sloppy
               --  One problem is that it can mix up some of the order of
               --  PREFIX, XXX, LOC; I ought to do this for every set of
               --  results from different approaches not just in one fell
               --  swoop at the end !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               declare
                  function Compare (L : Parse_Record;
                                    R : Parse_Record) return Boolean is
                  begin
                     if (R.MNPC < L.MNPC) or else
                       (R.MNPC = L.MNPC and then
                       R.IR.Ending.Size <
                       L.IR.Ending.Size) or else
                       (R.MNPC = L.MNPC and then
                       R.IR.Ending.Size =
                       L.IR.Ending.Size and then
                       R.IR.Qual < L.IR.Qual) or else
                       (R.MNPC = L.MNPC and then
                       R.IR.Ending.Size =
                       L.IR.Ending.Size and then
                       R.IR.Qual = L.IR.Qual and then
                       R.D_K  < L.D_K)
                     then
                        return True;
                     else
                        return False;
                     end if;
                  end Compare;
               begin
                  Inner_Loop :
                  for I in 1 .. Sl_Last - 1  loop
                     if Sl (I + 1) /= Null_Parse_Record  then
                        -- the following condition is absurd and should be
                        -- rewritten
                        if Compare (Sl (I), Sl (I + 1)) then
                           Sm := Sl (I);
                           Sl (I) := Sl (I + 1);
                           Sl (I + 1) := Sm;
                           Hits := Hits + 1;
                        end if;
                     else
                        exit Inner_Loop;
                     end if;
                  end loop Inner_Loop;
               end;
            end Switch;

            exit Hit_Loop when Hits = 0;
         end loop Hit_Loop;
         Sx := Sl;
      end Order_Stems;

      procedure Array_Stems
        (Sx : in Sal;
         Pa : in out Parse_Array; Pa_Last : in out Integer)
      is
         Sl : constant Sal := Sx;
         Opr : Parse_Record := Null_Parse_Record;
      begin

         if Sl (1) = Null_Parse_Record  then
            return;
         else

            Opr := Null_Parse_Record;
            for I in Sl'Range  loop
               if Sl (I) /= Null_Parse_Record  then
                  --PUT ('*'); PUT (SL (I)); NEW_LINE;

                  Supress_Key_Check :
                  declare
                     function "<=" (A, B : Parse_Record) return Boolean is
                        use Dict_IO;
                     begin  --  !!!!!!!!!!!!!!!!!!!!!!!!!!
                        if A.IR.Qual = B.IR.Qual and then
                          A.MNPC = B.MNPC
                        then
                           return True;
                        else
                           return False;
                        end if;
                     end "<=";
                  begin
                     if Sl (I) <= Opr then
                        --  Get rid of duplicates, if ORDER is OK
                        --PUT ('-'); PUT (SL (I)); NEW_LINE;
                        null;
                     else
                        Pa_Last := Pa_Last + 1;
                        Pa (Pa_Last) := Sl (I);
                        Opr := Sl (I);
                     end if;
                  end Supress_Key_Check;
               else
                  exit;
               end if;
            end loop;
         end if;
      end Array_Stems;

      procedure Reduce_Stem_List
        (Sl : in Sal;
         Sxx : in out Sal;
         --  Need in out if want to print it at the end
         --procedure REDUCE_STEM_LIST (SL : in SAL; SXX : out SAL;
         Prefix : in Prefix_Item := Null_Prefix_Item;
         Suffix : in Suffix_Item := Null_Suffix_Item)
      is
         MNPC_Part : MNPC_Type := Null_MNPC;
         Pdl_Part : Part_Entry;
         Com : Comparison_Type := X;
         Num_Sort : Numeral_Sort_Type := X;
         Ls : Integer := 0;
         M : Integer := 0;

         Pdl_Key : Stem_Key_Type;
         Pdl_P   : Part_Of_Speech_Type;
         --sl_key  : Stem_Key_Type;
         --sl_p    : Part_Of_Speech_Type;

         function "<=" (Left, Right : Part_Of_Speech_Type) return Boolean is
         begin
            if Right = Left  or else
              (Left = Pack and Right = Pron)  or else
              Right = X
            then
               return True;
            else
               return False;
            end if;
         end "<=";

         function "<=" (Left, Right : Gender_Type)   return Boolean is
         begin
            if Right = Left               or else
              (Right = C and Left /= N)  or else
              Right = X
            then
               return True;
            else
               return False;
            end if;
         end "<=";

         function "<=" (Left, Right : Stem_Key_Type)   return Boolean is
         begin
            if Right = Left or else Right = 0 then
               return True;
            else
               return False;
            end if;
         end "<=";

      begin
         Sxx := (others => Null_Parse_Record);
         --  Essentially initializing
         --  For the reduced dictionary list PDL
         M := 0;

         On_Pdl :
         for J in 1 .. Pdl_Index  loop

            Pdl_Part := Pdl (J).Ds.Part;
            Pdl_Key := Pdl (J).Ds.Key;
            MNPC_Part := Pdl (J).Ds.MNPC;

            --  Is there any point in going through the process for this PDL
            Pdl_P  := Pdl (J).Ds.Part.Pofs;  --  Used only for FIX logic below

            --  If there is no SUFFIX then carry on
            if Suffix = Null_Suffix_Item then
               --  No suffix working, fall through
               null;
            elsif
              --  No suffix for abbreviations
              (Pdl_P = N    and then Pdl_Part.N.Decl = (9, 8)) or
              (Pdl_P = Adj  and then Pdl_Part.Adj.Decl = (9, 8))
            then
               --   Can be no suffix on abbreviation");
               goto End_Of_Pdl_Loop;
            else
               --  There is SUFFIX, see if it agrees with PDL
               --  Does SUFFIX agree in ROOT
               if Pdl_P <= Suffix.Entr.Root  and then
                 ((Pdl_Key <= Suffix.Entr.Root_Key)  or else
                 ((Pdl_Key = 0) and then
                 ((Pdl_P = N) or (Pdl_P = Adj) or (Pdl_P = V)) and then
                 ((Suffix.Entr.Root_Key = 1) or (Suffix.Entr.Root_Key = 2))))
               then
                  --  Transform PDL_PART to TARGET
                  case Suffix.Entr.Target.Pofs is
                     when N =>    Pdl_Part := (N,    Suffix.Entr.Target.N);
                     when Pron => Pdl_Part := (Pron, Suffix.Entr.Target.Pron);
                     when Adj =>  Pdl_Part := (Adj,  Suffix.Entr.Target.Adj);
                     when Num =>  Pdl_Part := (Num,  Suffix.Entr.Target.Num);
                     when Adv =>  Pdl_Part := (Adv,  Suffix.Entr.Target.Adv);
                     when V =>    Pdl_Part := (V,    Suffix.Entr.Target.V);
                     when others => null; --  No others so far, except X = all
                  end case;
                  Pdl_Key := Suffix.Entr.Target_Key;
                  Pdl_P  := Pdl_Part.Pofs;  --  Used only for FIX logic below
               else
                  --PUT_LINE ("In REDUCE_STEM_LIST   There is no legal suffix");
                  --            exit;
                  goto End_Of_Pdl_Loop;
               end if;
            end if;

            if Prefix = Null_Prefix_Item then      --  No PREFIX, drop through
               null;
            elsif
              --  No prefix for abbreviations
              (Pdl_P = N    and then Pdl_Part.N.Decl = (9, 8)) or
              (Pdl_P = Adj  and then Pdl_Part.Adj.Decl = (9, 8)) or
              (Pdl_P = Interj  or Pdl_P = Conj)  --  or INTERJ or CONJ
            then
               goto End_Of_Pdl_Loop;
            else
               if (Pdl_P = Prefix.Entr.Root)  or    --  = ROOT
                 (Pdl_Part.Pofs = Prefix.Entr.Root)  --  or part mod by suf
               then
                  null;
               elsif Prefix.Entr.Root = X then  --   or ROOT = X
                  null;
               else
                  goto End_Of_Pdl_Loop;
               end if;
            end if;

            --  SUFFIX and PREFIX either agree or don't exist
            --  (agrees with everything)
            Ls := Len (Add_Suffix
              (Add_Prefix (Pdl (J).Ds.Stem, Prefix), Suffix));
            On_Sl :
            for I in Sl'Range loop
               exit On_Sl when Sl (I) = Null_Parse_Record;

               if Ls  = Len (Sl (I).Stem)  then

                  --  Scan through the whole unreduced stem list
                  --  Single out those stems that match (pruned) dictionary
                  --   entries
                  --^^^^^should be able to do this better with new arrangement

                  --sl_key := sl (i).ir.key;
                  --sl_p := sl (i).ir.qual.pofs;

                  if (
                    ((Pdl_Key <= Sl (I).IR.Key))  or else
                    ((Pdl_Key = 0)  and then
                    (((Pdl_P = N) or (Pdl_P = Adj) or (Pdl_P = V)) and then
                    ((Sl (I).IR.Key = 1) or (Sl (I).IR.Key = 2))))
                     )  and then   --  and KEY
                    (Pdl_Part.Pofs  = Eff_Part (Sl (I).IR.Qual.Pofs))
                  then
                     if Pdl_Part.Pofs = N                            and then
                       Pdl_Part.N.Decl <= Sl (I).IR.Qual.Noun.Decl      and then
                       Pdl_Part.N.Gender <= Sl (I).IR.Qual.Noun.Gender
                     then
                        --  Need to transfer the gender of the noun
                        --  dictionary item
                        M := M + 1;
                        Sxx (M) :=
                          (Stem => Subtract_Prefix (Sl (I).Stem, Prefix),
                           IR => (
                            Qual => (
                             Pofs => N,
                             Noun => (
                              Pdl_Part.N.Decl,
                              Sl (I).IR.Qual.Noun.Of_Case,
                              Sl (I).IR.Qual.Noun.Number,
                              Pdl_Part.N.Gender)),
                            Key => Sl (I).IR.Key,
                            Ending => Sl (I).IR.Ending,
                            Age => Sl (I).IR.Age,
                            Freq => Sl (I).IR.Freq),
                           D_K => Pdl (J).D_K,
                           MNPC => MNPC_Part);

                     elsif Pdl_Part.Pofs = Pron and then
                       Pdl_Part.Pron.Decl <= Sl (I).IR.Qual.Pron.Decl
                     then
                        --PUT (" HIT  PRON  ");
                        --  Need to transfer the kind of the pronoun
                        --  dictionary item
                        M := M + 1;
                        Sxx (M) :=
                          (Stem => Subtract_Prefix (Sl (I).Stem, Prefix),
                           IR => (
                            Qual => (
                             Pofs => Pron,
                             Pron => (
                              Pdl_Part.Pron.Decl,
                              Sl (I).IR.Qual.Pron.Of_Case,
                              Sl (I).IR.Qual.Pron.Number,
                              Sl (I).IR.Qual.Pron.Gender)),
                            Key => Sl (I).IR.Key,
                            Ending => Sl (I).IR.Ending,
                            Age => Sl (I).IR.Age,
                            Freq => Sl (I).IR.Freq),
                           D_K => Pdl (J).D_K,
                           MNPC => MNPC_Part);

                     elsif (Pdl_Part.Pofs = Adj) and then
                       (Pdl_Part.Adj.Decl <= Sl (I).IR.Qual.Adj.Decl) and then
                       ((Sl (I).IR.Qual.Adj.Comparison <= Pdl_Part.Adj.Co) or
                       ((Sl (I).IR.Qual.Adj.Comparison = X) or
                       (Pdl_Part.Adj.Co = X)))
                     then
                        --  Note the reversal on comparisom
                        --PUT (" HIT  ADJ   ");
                        --  Need to transfer the gender of the dictionary item
                        --  Need to transfer the CO of the ADJ dictionary item
                        if Pdl_Part.Adj.Co in Pos .. Super  then
                           --  If the dictionary entry has a unique CO, use it
                           Com := Pdl_Part.Adj.Co;
                        else
                           --  Otherwise, the entry is X, generate a CO from KEY
                           Com := Adj_Comp_From_Key (Pdl_Key);
                        end if;
                        M := M + 1;
                        Sxx (M) :=
                          (Stem => Subtract_Prefix (Sl (I).Stem, Prefix),
                           IR => (
                            Qual => (
                             Pofs => Adj,
                             Adj => (
                              Pdl_Part.Adj.Decl,
                              Sl (I).IR.Qual.Adj.Of_Case,
                              Sl (I).IR.Qual.Adj.Number,
                              Sl (I).IR.Qual.Adj.Gender,
                              Com)),
                            Key => Sl (I).IR.Key,
                            Ending => Sl (I).IR.Ending,
                            Age => Sl (I).IR.Age,
                            Freq => Sl (I).IR.Freq),
                           D_K => Pdl (J).D_K,
                           MNPC => MNPC_Part);

                     elsif (Pdl_Part.Pofs = Num) and then
                       (Pdl_Part.Num.Decl <= Sl (I).IR.Qual.Num.Decl) and then
                       (Pdl_Key         = Sl (I).IR.Key)
                     then
                        --PUT(" HIT  NUM    ");
                        if Pdl_Part.Num.Sort = X  then
                           --  If the entry is X, generate a CO from KEY
                           Num_Sort := Num_Sort_From_Key (Pdl_Key);
                        else
                           --  Otherwise, the dictionary entry has a
                           --  unique CO, use it
                           Num_Sort := Pdl_Part.Num.Sort;
                        end if;
                        M := M + 1;
                        Sxx (M) :=
                          (Stem => Subtract_Prefix (Sl (I).Stem, Prefix),
                           IR => (
                            Qual => (
                             Pofs => Num,
                             Num => (
                              Pdl_Part.Num.Decl,
                              Sl (I).IR.Qual.Num.Of_Case,
                              Sl (I).IR.Qual.Num.Number,
                              Sl (I).IR.Qual.Num.Gender,
                              Num_Sort)),
                            Key => Sl (I).IR.Key,
                            Ending => Sl (I).IR.Ending,
                            Age => Sl (I).IR.Age,
                            Freq => Sl (I).IR.Freq),
                           D_K => Pdl (J).D_K,
                           MNPC => MNPC_Part);

                     elsif (Pdl_Part.Pofs = Adv) and then
                       ((Pdl_Part.Adv.Co <= Sl (I).IR.Qual.Adv.Comparison) or
                       ((Sl (I).IR.Qual.Adv.Comparison = X) or
                       (Pdl_Part.Adv.Co = X)))
                     then
                        --PUT (" HIT  ADV   ");
                        --  Need to transfer the CO of the ADV dictionary item
                        if Pdl_Part.Adv.Co in Pos .. Super  then
                           --  If the dictionary entry has a unique CO, use it
                           Com := Pdl_Part.Adv.Co;
                        else
                           --  The entry is X and we need to generate
                           --  a COMP from the KEY
                           Com := Adv_Comp_From_Key (Pdl_Key);
                        end if;
                        M := M + 1;
                        Sxx (M) :=
                          (Stem => Subtract_Prefix (Sl (I).Stem, Prefix),
                           IR => (
                            Qual => (
                             Pofs => Adv,
                             Adv => (
                              Comparison => Com)),
                            Key => Sl (I).IR.Key,
                            Ending => Sl (I).IR.Ending,
                            Age => Sl (I).IR.Age,
                            Freq => Sl (I).IR.Freq),
                           D_K => Pdl (J).D_K,
                           MNPC => MNPC_Part);

                     elsif Pdl_Part.Pofs = V then
                        --TEXT_IO.PUT_LINE ("V found, now check CON");
                        if Sl (I).IR.Qual.Pofs = V     and then
                          (Pdl_Part.V.Con <= Sl (I).IR.Qual.Verb.Con)
                        then
                           --TEXT_IO.PUT (" HIT  V     ");
                           M := M + 1;
                           Sxx (M) :=
                             (Stem => Subtract_Prefix (Sl (I).Stem, Prefix),
                              IR => (
                               Qual => (
                                Pofs => V,
                                Verb => (
                                 Pdl_Part.V.Con,
                                 Sl (I).IR.Qual.Verb.Tense_Voice_Mood,
                                 Sl (I).IR.Qual.Verb.Person,
                                 Sl (I).IR.Qual.Verb.Number)),
                               Key => Sl (I).IR.Key,
                               Ending => Sl (I).IR.Ending,
                               Age => Sl (I).IR.Age,
                               Freq => Sl (I).IR.Freq),
                              D_K => Pdl (J).D_K,
                              MNPC => MNPC_Part);

                        elsif Sl (I).IR.Qual.Pofs = Vpar   and then
                          (Pdl_Part.V.Con <= Sl (I).IR.Qual.Vpar.Con)
                        then
                           --PUT (" HIT  VPAR  ");
                           M := M + 1;
                           Sxx (M) :=
                             (Stem => Subtract_Prefix (Sl (I).Stem, Prefix),
                              IR => (
                               Qual => (
                                Pofs => Vpar,
                                Vpar => (
                                 Pdl_Part.V.Con,
                                 Sl (I).IR.Qual.Vpar.Of_Case,
                                 Sl (I).IR.Qual.Vpar.Number,
                                 Sl (I).IR.Qual.Vpar.Gender,
                                 Sl (I).IR.Qual.Vpar.Tense_Voice_Mood)),
                               Key => Sl (I).IR.Key,
                               Ending => Sl (I).IR.Ending,
                               Age => Sl (I).IR.Age,
                               Freq => Sl (I).IR.Freq),
                              D_K => Pdl (J).D_K,
                              MNPC => MNPC_Part);

                        elsif Sl (I).IR.Qual.Pofs = Supine   and then
                          (Pdl_Part.V.Con <= Sl (I).IR.Qual.Supine.Con)
                        then
                           --PUT (" HIT  SUPINE");
                           M := M + 1;
                           Sxx (M) :=
                             (Stem => Subtract_Prefix (Sl (I).Stem, Prefix),
                              IR => (
                               Qual => (
                                Pofs => Supine,
                                Supine => (
                                 Pdl_Part.V.Con,
                                 Sl (I).IR.Qual.Supine.Of_Case,
                                 Sl (I).IR.Qual.Supine.Number,
                                 Sl (I).IR.Qual.Supine.Gender)),
                               Key => Sl (I).IR.Key,
                               Ending => Sl (I).IR.Ending,
                               Age => Sl (I).IR.Age,
                               Freq => Sl (I).IR.Freq),
                              D_K => Pdl (J).D_K,
                              MNPC => MNPC_Part);
                        end if;

                     elsif Pdl_Part.Pofs = Prep and then
                       Pdl_Part.Prep.Obj = Sl (I).IR.Qual.Prep.Of_Case
                     then
                        --PUT (" HIT  PREP  ");
                        M := M + 1;
                        Sxx (M) :=
                          (Subtract_Prefix (Sl (I).Stem, Prefix), Sl (I).IR,
                          Pdl (J).D_K, MNPC_Part);

                     elsif Pdl_Part.Pofs = Conj then
                        --PUT (" HIT  CONJ  ");
                        M := M + 1;
                        Sxx (M) :=
                          (Subtract_Prefix (Sl (I).Stem, Prefix), Sl (I).IR,
                          Pdl (J).D_K, MNPC_Part);

                     elsif Pdl_Part.Pofs = Interj then
                        --PUT (" HIT  INTERJ ");
                        M := M + 1;
                        Sxx (M) :=
                          (Subtract_Prefix (Sl (I).Stem, Prefix), Sl (I).IR,
                          Pdl (J).D_K, MNPC_Part);

                     end if;

                  end if;
               end if;
            end loop On_Sl;

         <<End_Of_Pdl_Loop>> null;
         end loop On_Pdl;
      end Reduce_Stem_List;

      procedure Apply_Prefix
        (Sa : in Stem_Array_Type;
         Suffix : in Suffix_Item;
         Sx : in Sal;
         Sxx : in out Sal;
         Pa : in out Parse_Array;
         Pa_Last : in out Integer)
      is
         --  Worry about the stem changing re-cipio from capio
         --  Correspondence of parts, need EFF for VPAR
         --  The prefixes should be ordered with the longest/most likely first
         Ssa : Stem_Array;
         L : Integer :=  0;

      begin
         --PUT_LINE ("Entering APPLY_PREFIX");
         Sxx := (others => Null_Parse_Record);    --  !!!!!!!!!!!!!!!!!!!!!!!

         if Words_Mdev (Use_Prefixes)  then

            for I in 1 .. Number_Of_Prefixes  loop
               --  Loop through PREFIXES
               L :=  0;
               for J in Sa'Range  loop
                  --  Loop through stem array
                  if Sa (J)(1) = Prefixes (I).Fix (1) then
                     --  Cuts down a little -- do better
                     if Subtract_Prefix (Sa (J), Prefixes (I)) /=
                       Head (Sa (J), Max_Stem_Size)
                     then
                        L := L + 1;
                        --  We have a hit, make new stem array item
                        Ssa (L) := Head (Subtract_Prefix (Sa (J), Prefixes (I)),
                          Max_Stem_Size);
                        --  And that has prefix subtracted to match dict
                     end if; --  with prefix subtracted stems
                  end if;
               end loop;

               if L > 0  then
                  --  There has been a prefix hit
                  Search_Dictionaries (Ssa (1 .. L));
                  --  So run new dictionary search

                  if  Pdl_Index /= 0     then
                     --  Dict search was successful
                     Reduce_Stem_List (Sx, Sxx, Prefixes (I), Suffix);

                     if Sxx (1) /= Null_Parse_Record  then
                        --  There is reduced stem result
                        Pa_Last := Pa_Last + 1;
                        --  So add prefix line to parse array
                        Pa (Pa_Last).IR :=
                          ((Prefix, Null_Prefix_Record), 0,
                          Null_Ending_Record, X, X);
                        Pa (Pa_Last).Stem :=
                          Head (Prefixes (I).Fix, Max_Stem_Size);
                        Pa (Pa_Last).MNPC := Dict_IO.Count (Prefixes (I).MNPC);
                        Pa (Pa_Last).D_K  := Addons;
                        exit;      --  Because we accept only one prefix
                     end if;
                  end if;
               end if;
            end loop;      --  Loop on I for PREFIXES
         end if;  --  On USE_PREFIXES
      end Apply_Prefix;

      procedure Apply_Suffix
        (Sa : in Stem_Array_Type;
         Sx : in Sal;
         Sxx : in out Sal;
         Pa : in out Parse_Array;
         Pa_Last : in out Integer)
      is
         Ssa : Stem_Array;
         L : Integer :=  0;
         Suffix_Hit : Integer := 0;
         --            use TEXT_IO;
         --            use INFLECTIONS_PACKAGE.INTEGER_IO;

      begin
         for I in 1 .. Number_Of_Suffixes  loop       --  Loop through SUFFIXES
            L :=  0;                                 --  Take as many as fit

            for J in Sa'Range  loop                  --  Loop through stem array
               if Subtract_Suffix (Sa (J), Suffixes (I)) /=
                 Head (Sa (J), Max_Stem_Size)
               then
                  L := L + 1;
                  --  We have a hit, make new stem array item
                  Ssa (L) := Head (Subtract_Suffix (Sa (J), Suffixes (I)),
                    Max_Stem_Size);
                  --  And that has prefix subtracted to match dict
               end if;
            end loop;    --  Loop on J through SA

            if L > 0  then
               --  There has been a suffix hit
               Search_Dictionaries (Ssa (1 .. L));
               --  So run new dictionary search
               --  For suffixes we allow as many as match

               if Pdl_Index /= 0 then
                  --  Dict search was successful
                  Suffix_Hit := I;

                  Reduce_Stem_List (Sx, Sxx, Null_Prefix_Item, Suffixes (I));

                  if Sxx (1) /= Null_Parse_Record  then
                     --  There is reduced stem result
                     Pa_Last := Pa_Last + 1;
                     --  So add suffix line to parse array
                     Pa (Pa_Last).IR :=
                       ((Suffix, Null_Suffix_Record),
                       0, Null_Ending_Record, X, X);
                     Pa (Pa_Last).Stem :=
                       Head (Suffixes (Suffix_Hit).Fix, Max_Stem_Size);
                     --  Maybe it would better if suffix.fix was of stem size
                     Pa (Pa_Last).MNPC :=
                       Dict_IO.Count (Suffixes (Suffix_Hit).MNPC);
                     Pa (Pa_Last).D_K  := Addons;
                     ---
                     for I in Sxx'Range  loop
                        exit when Sxx (I) = Null_Parse_Record;
                        Pa_Last := Pa_Last + 1;
                        Pa (Pa_Last) := Sxx (I);
                     end loop;
                     ---
                  end if;

               else   --  there is suffix (L /= 0) but no dictionary hit
                  Suffix_Hit := I;
                  Apply_Prefix
                    (Ssa (1 .. L), Suffixes (I), Sx, Sxx, Pa, Pa_Last);
                  if Sxx (1) /= Null_Parse_Record  then
                     --  There is reduced stem result
                     Pa_Last := Pa_Last + 1;
                     --  So add suffix line to parse array
                     Pa (Pa_Last).IR :=
                       ((Suffix, Null_Suffix_Record),
                       0, Null_Ending_Record, X, X);
                     Pa (Pa_Last).Stem := Head
                       (Suffixes (Suffix_Hit).Fix, Max_Stem_Size);
                     Pa (Pa_Last).MNPC :=
                       Dict_IO.Count (Suffixes (Suffix_Hit).MNPC);
                     Pa (Pa_Last).D_K  := Addons;

                     for I in Sxx'Range  loop    --  Set this set of results
                        exit when Sxx (I) = Null_Parse_Record;
                        Pa_Last := Pa_Last + 1;
                        Pa (Pa_Last) := Sxx (I);
                     end loop;
                  end if;
               end if;
            end if;             --  with suffix subtracted stems
         end loop;      --  Loop on I for SUFFIXES
      end Apply_Suffix;

      procedure Prune_Stems
        (Input_Word : String;
         Sx : in Sal;
         Sxx : in out Sal)
      is
         J : Integer := 0;
         --SXX : SAL;

      begin
         if Sx (1) = Null_Parse_Record  then
            return;
         end if;

         -----------------------------------------------------------------

         Generate_Reduced_Stem_Array :
         begin
            J := 1;
            for Z in 0 .. Integer'Min (Max_Stem_Size, Len (Input_Word))  loop
               if Sa (Z) /= Not_A_Stem  then
                  --PUT (Z); PUT (J); PUT ("  "); PUT_LINE (SA (Z));
                  Ssa (J) := Sa (Z);
                  Ssa_Max := J;
                  J := J + 1;
               end if;
            end loop;
         end Generate_Reduced_Stem_Array;

         if not Words_Mdev (Do_Only_Fixes)  then
            --  Just bypass main dictionary search

            Search_Dictionaries (Ssa (1 .. Ssa_Max));

         end if;

         if (((Pa_Last = 0)  and            --  No Uniques or Syncope
           (Pdl_Index = 0))  --)   and then    --  No dictionary match
           or Words_Mdev (Do_Fixes_Anyway))  and then
           Words_Mode (Do_Fixes)
         then

            ----So try prefixes and suffixes,
            --- Generate a new SAA array, search again

            if Sxx (1) = Null_Parse_Record  then
               --  We could not find a match with suffix
               Apply_Prefix (Ssa (1 .. Ssa_Max),
                 Null_Suffix_Item, Sx, Sxx, Pa, Pa_Last);
            end if;
            --------------
            if Sxx (1) = Null_Parse_Record  then
               --  We could not find a match with suffix
               Apply_Suffix (Ssa (1 .. Ssa_Max), Sx, Sxx, Pa, Pa_Last);
               if Sxx (1) = Null_Parse_Record  then
                  --  We could not find a match with suffix
                  ----So try prefixes, Generate a new SAA array, search again
                  ----Need to use the new SSA, modified to include suffixes
                  Apply_Prefix (Ssa (1 .. Ssa_Max),
                    Null_Suffix_Item, Sx, Sxx, Pa, Pa_Last);
                  --------------
               end if;       --  Suffix failed
            end if;       --  Suffix failed
         else
            Reduce_Stem_List (Sx, Sxx, Null_Prefix_Item, Null_Suffix_Item);
            if Pa_Last = 0  and then  Sxx (1) = Null_Parse_Record  then
               --------------
               if Words_Mode (Do_Fixes)  then
                  Apply_Suffix (Ssa (1 .. Ssa_Max), Sx, Sxx, Pa, Pa_Last);
                  if Sxx (1) = Null_Parse_Record  then
                     --  We could not find a match with suffix
                     ----So try prefixes, Generate a new SAA array, search again
                     ----Need to use the new SSA, modified to include suffixes
                     Apply_Prefix (Ssa (1 .. Ssa_Max), Null_Suffix_Item,
                       Sx, Sxx, Pa, Pa_Last);
                  end if;   --  Suffix failed
               end if;     --  If DO_FIXES then do
            end if;       --  First search passed but SXX null
         end if;         --  First search failed

      end Prune_Stems;

      procedure Process_Packons (Input_Word : String) is

         Stem_Length  : Integer := 0;
         Pr   : Parse_Record;
         M : Integer := 1;
         De : Dictionary_Entry;
         Mean : Meaning_Type;
         Packon_First_Hit : Boolean := False;
         Sl : Sal := (others => Null_Parse_Record);
         Sl_Nulls : constant Sal := (others => Null_Parse_Record);

      begin

         Over_Packons :
         for K in Packons'Range  loop
            -- Do whole set, more than one may apply
            --  PACKON if the TACKON ENTRY is PRON

            For_Each_Packon :
            declare
               Xword : constant String :=
                 Subtract_Tackon (Input_Word, Packons (K));
               Word : String (1 .. Xword'Length) := Xword;
               Packon_Length : constant Integer :=
                 Trim (Packons (K).Tack)'Length;
               Last_Of_Word : Character := Word (Word'Last);
               Length_Of_Word   : constant Integer := Word'Length;
            begin
               Sl := Sl_Nulls;      --  Initialize SL to nulls
               if Word  /= Input_Word  then
                  Packon_First_Hit := True;

                  if Packons (K).Tack (1 .. 3) = "dam"
                    and  Last_Of_Word = 'n'
                  then
                     --  Takes care of the m - > n shift with dam
                     Word (Word'Last) := 'm';
                     Last_Of_Word := 'm';
                  end if;

                  --  No blank endings in these pronouns
                  Lel_Section_Io.Read (Inflections_Sections_File, Lel, 4);

                  M := 0;

                  On_Inflects :
                  for Z in reverse 1 .. Integer'Min (6, Length_Of_Word)  loop
                     --  optimum for qu-pronouns
                     if Pell (Z, Last_Of_Word) > 0  then
                        --  Any possible inflections at all
                        for I in Pelf
                          (Z, Last_Of_Word) .. Pell (Z, Last_Of_Word) loop
                           if (Z <= Length_Of_Word)  and then
                             ((Equ (Lel (I).Ending.Suf (1 .. Z),
                             Word (Word'Last - Z + 1 .. Word'Last)))  and
                             (Lel (I).Qual.Pron.Decl <=
                             Packons (K).Entr.Base.Pack.Decl))
                           then
                              --  Have found an ending that is a possible match
                              --  And INFLECT agrees with PACKON.BASE
                              --  Add to list of possible ending records
                              Stem_Length := Word'Length - Z;
                              Pr := (Head (Word (Word'First .. Stem_Length),
                                Max_Stem_Size),
                                Lel (I), Default_Dictionary_Kind, Null_MNPC);
                              M := M + 1;
                              Sl (M) := Pr;
                              Ssa (1) := Head
                                (Word
                                (Word'First .. Word'First + Stem_Length - 1),
                                Max_Stem_Size);
                              --  may Get set several times
                           end if;
                        end loop;
                     end if;
                  end loop On_Inflects;

                  --  Only one stem will emerge
                  Pdl_Index := 0;
                  Search_Dictionaries (Ssa (1 .. 1),
                    Pack_Only);
                  --  Now have a PDL, scan for agreement

                  Pdl_Loop :
                  for J in 1 .. Pdl_Index  loop
                     --  Go through all dictionary hits to see
                     --  M used here where I is used in REDUCE,
                     --  maybe make consistent
                     M := 1;

                     Sl_Loop :
                     while Sl (M) /= Null_Parse_Record  loop
                        --  Over all inflection hits
                        --  if this stem is possible
                        --  call up the meaning to check for "(w/-"
                        Dict_IO.Set_Index (Dict_File (Pdl (J).D_K),
                          Pdl (J).Ds.MNPC);
                        Dict_IO.Read (Dict_File (Pdl (J).D_K), De);
                        Mean := De.Mean;

                        -- there is no way this condition can be True;
                        -- packon_length - 1 /= packon_length

                        --  Does attached PACKON agree
                        if Trim (Mean)(1 .. 4) = "(w/-" and then
                          Trim (Mean)(5 .. 4 + Packon_Length) =
                          Trim (Packons (K).Tack)
                        then
                           if Pdl (J).Ds.Part.Pack.Decl =
                             Sl (M).IR.Qual.Pron.Decl
                           then  --  or
                              if Packon_First_Hit then
                                 Pa_Last := Pa_Last + 1;
                                 Pa (Pa_Last) := (Packons (K).Tack,
                                   ((Tackon, Null_Tackon_Record), 0,
                                   Null_Ending_Record, X, X),
                                   Addons,
                                   Dict_IO.Count ((Packons (K).MNPC)));
                                 Packon_First_Hit := False;
                              end if;
                              Pa_Last := Pa_Last + 1;
                              Pa (Pa_Last) := (
                                Stem => Sl (M).Stem,
                                IR => (
                                 Qual => (
                                  Pofs => Pron,
                                  Pron => (
                                   Pdl (J).Ds.Part.Pack.Decl,
                                   Sl (M).IR.Qual.Pron.Of_Case,
                                   Sl (M).IR.Qual.Pron.Number,
                                   Sl (M).IR.Qual.Pron.Gender)),
                                 Key => Sl (M).IR.Key,
                                 Ending => Sl (M).IR.Ending,
                                 Age => Sl (M).IR.Age,
                                 Freq => Sl (M).IR.Freq),
                                D_K => Pdl (J).D_K,
                                MNPC => Pdl (J).Ds.MNPC);
                              --end if;
                           end if;
                        end if;
                        M := M + 1;

                     end loop Sl_Loop;

                  end loop Pdl_Loop;

               end if;
            end For_Each_Packon;

            Packon_First_Hit := False;

         end loop Over_Packons;
      end Process_Packons;

      procedure Process_Qu_Pronouns
        (Input_Word : String;
         Qkey : Stem_Key_Type := 0)
      is
         Word : constant String := Lower_Case (Trim (Input_Word));
         Last_Of_Word : constant Character := Word (Word'Last);
         Length_Of_Word   : constant Integer := Word'Length;
         Stem_Length  : Integer := 0;
         M : Integer := 0;
         Pr   : Parse_Record;
         Sl : Sal := (others => Null_Parse_Record);

      begin
         --TEXT_IO.PUT_LINE ("PROCESS_QU_PRONOUNS   " & INPUT_WORD);

         --  No blank endings in these pronouns
         Lel_Section_Io.Read (Inflections_Sections_File, Lel, 4);

         --  M used here while I is used in REDUCE, maybe make consistent
         M := 0;

         On_Inflects :
         for Z in reverse 1 .. Integer'Min (4, Length_Of_Word)  loop
            --  optimized for qu-pronouns
            if Pell (Z, Last_Of_Word) > 0  then
               --  Any possible inflections at all
               for I in Pelf (Z, Last_Of_Word) .. Pell (Z, Last_Of_Word) loop
                  if (Z <= Length_Of_Word)  and then
                    Lel (I).Key = Qkey  and then
                    Equ (Lel (I).Ending.Suf (1 .. Z),
                    Word (Word'Last - Z + 1 .. Word'Last))
                  then
                     --  Have found an ending that is a possible match
                     --  Add to list of possible ending records
                     Stem_Length := Word'Length - Z;
                     Pr := (Head (Word (Word'First .. Stem_Length),
                       Max_Stem_Size),
                       Lel (I), Default_Dictionary_Kind, Null_MNPC);
                     M := M + 1;
                     Sl (M) := Pr;
                     Ssa (1) :=
                       Head (Word (Word'First .. Word'First + Stem_Length - 1),
                       Max_Stem_Size);
                     --  may Get set several times
                  end if;
               end loop;
            end if;
         end loop On_Inflects;

         --  Only one stem will emerge
         Pdl_Index := 0;
         Search_Dictionaries (Ssa (1 .. 1),
           Qu_Pron_Only);
         --  Now have a PDL, scan for agreement

         Pdl_Loop :
         for J in 1 .. Pdl_Index  loop
            --  Go through all dictionary hits to see
            M := 1;

            Sl_Loop :
            while Sl (M) /= Null_Parse_Record  loop
               --  Over all inflection hits
               if Pdl (J).Ds.Part.Pron.Decl = Sl (M).IR.Qual.Pron.Decl then
                  Pa_Last := Pa_Last + 1;
                  Pa (Pa_Last) := (
                    Stem => Sl (M).Stem,
                    IR => (
                     Qual => (
                      Pofs => Pron,
                      Pron => (
                       Pdl (J).Ds.Part.Pron.Decl,
                       Sl (M).IR.Qual.Pron.Of_Case,
                       Sl (M).IR.Qual.Pron.Number,
                       Sl (M).IR.Qual.Pron.Gender)),
                     Key => Sl (M).IR.Key,
                     Ending => Sl (M).IR.Ending,
                     Age => Sl (M).IR.Age,
                     Freq => Sl (M).IR.Freq),
                    D_K => Pdl (J).D_K,
                    MNPC => Pdl (J).Ds.MNPC);
               end if;
               M := M + 1;

            end loop Sl_Loop;
            -- PDL:= PDL.SUCC;
         end loop Pdl_Loop;

      end Process_Qu_Pronouns;

      procedure Try_Tackons (Input_Word : String) is
         Tackon_Hit : Boolean := False;
         Tackon_On  : Boolean := False;
         J : Integer := 0;
         De : Dictionary_Entry := Null_Dictionary_Entry;
         Entering_Pa_Last : constant Integer := Pa_Last;
         Start_Of_Loop : constant Integer := 5;
         --  4 enclitics     --  Hard number  !!!!!!!!!!!!!!!
         End_Of_Loop : constant Integer := Number_Of_Tackons;
      begin
         Loop_Over_Tackons :
         for I in Start_Of_Loop .. End_Of_Loop  loop

            Remove_A_Tackon :
            declare
               Less : constant String :=
                 Subtract_Tackon (Input_Word, Tackons (I));
            begin
               --TEXT_IO.PUT_LINE ("LESS = " & LESS);
               if Less  /= Input_Word  then       --  LESS is less
                  Word (Less, Pa, Pa_Last);

                  if Pa_Last > Entering_Pa_Last  then
                     --  we have a possible word
                     if Tackons (I).Entr.Base.Pofs = X  then
                        Tackon_Hit := True;
                        Tackon_On  := False;
                     else
                        J := Pa_Last;

                        while J >= Entering_Pa_Last + 1  loop
                           --  Sweep backwards over PA
                           --  Sweeping up inapplicable fixes,
                           --  although we only have TACKONs for X
                           --    or PRON or ADJ - so far
                           --  and there are no fixes for PRON - so far

                           if Pa (J).IR.Qual.Pofs = Prefix
                             and then Tackon_On
                           then
                              null;          --  check PART
                              Tackon_On  := False;
                           elsif Pa (J).IR.Qual.Pofs = Suffix
                             and then Tackon_On
                           then
                              --  check PART
                              null;
                              Tackon_On  := False;
                           elsif Pa (J).IR.Qual.Pofs =
                             Tackons (I).Entr.Base.Pofs
                           then
                              Dict_IO.Set_Index
                                (Dict_File (Pa (J).D_K), Pa (J).MNPC);
                              Dict_IO.Read (Dict_File (Pa (J).D_K), De);

                              --  check PART
                              case Tackons (I).Entr.Base.Pofs is
                                 when N       =>
                                    if Pa (J).IR.Qual.Noun.Decl <=
                                      Tackons (I).Entr.Base.N.Decl
                                    then
                                       --  Ignore GEN and KIND
                                       Tackon_Hit := True;
                                       Tackon_On  := True;
                                    end if;
                                 when Pron    =>
                                    --  Only one we have other than X
                                    if Pa (J).IR.Qual.Pron.Decl <=
                                      Tackons (I).Entr.Base.Pron.Decl
                                    then
                                       Tackon_Hit := True;
                                       Tackon_On  := True;
                                    else
                                       Pa (J .. Pa_Last - 1) :=
                                         Pa (J + 1 .. Pa_Last);
                                       Pa_Last := Pa_Last - 1;

                                    end if;
                                 when Adj     =>
                                    --  Forego all checks, even on DECL of ADJ
                                    --  -cumque is the only one I have now
                                    --  if  . .. .. ..
                                    Tackon_Hit := True;
                                    Tackon_On  := True;
                                    --  else
                                    --    PA (J .. PA_LAST - 1) :=
                                    --       PA (J + 1 .. PA_LAST);
                                    --    PA_LAST := PA_LAST - 1;
                                    --  end if;

                                    --when ADV     =>
                                    --when V       =>
                                 when others  =>
                                    Pa (J .. Pa_Last - 1) :=
                                      Pa (J + 1 .. Pa_Last);
                                    Pa_Last := Pa_Last - 1;
                              end case;
                           else --  check PART
                              Pa (J .. Pa_Last - 1) := Pa (J + 1 .. Pa_Last);
                              Pa_Last := Pa_Last - 1;
                           end if; --  check PART
                           J := J - 1;
                        end loop; --  loop sweep over PA
                     end if; --  on PART (= X?)

                     -----------------------------------------
                     if Tackon_Hit  then
                        Pa_Last := Pa_Last + 1;
                        Pa (Entering_Pa_Last + 2 .. Pa_Last) :=
                          Pa (Entering_Pa_Last + 1 .. Pa_Last - 1);
                        Pa (Entering_Pa_Last + 1) := (Tackons (I).Tack,
                          ((Tackon, Null_Tackon_Record), 0,
                          Null_Ending_Record, X, X),
                          Addons,
                          Dict_IO.Count ((Tackons (I).MNPC)));
                        return;                 --  Be happy with one ???????
                     else
                        null;
                     end if;   --  TACKON_HIT
                  end if;                       --  we have a possible word
               end if;                                     --  LESS is less
            end Remove_A_Tackon;
         end loop Loop_Over_Tackons;
      end Try_Tackons;

   begin                           --  WORD
      if Trim (Input_Word) = ""  then
         return;
      end if;

      Run_Uniques (Input_Word, Pa, Pa_Last);

      Qu :
      declare
         Pa_Qstart : constant Integer := Pa_Last;
         Pa_Start : constant Integer := Pa_Last;
         Saved_Mode_Array : constant Mode_Array := Words_Mode;
         Qkey : Stem_Key_Type := 0;

      begin       --  QU
         Tickons (Number_Of_Tickons + 1) := Null_Prefix_Item;
         Words_Mode  := (others => False);

         for I in 1 .. Number_Of_Tickons + 1  loop
            declare
               Q_Word : constant String :=
                 Trim (Subtract_Tickon (Input_Word, Tickons (I)));
            begin
               Pa_Last := Pa_Qstart;
               Pa (Pa_Last + 1) := Null_Parse_Record;
               if (I = Number_Of_Tickons + 1)   or else
                 --  The prefix is a TICKON
                 (Q_Word /= Input_Word)
               --  and it matches the start of INPUT_WORD
               then

                  if I <= Number_Of_Tickons  then        --  Add to PA if
                     Pa_Last := Pa_Last + 1;
                     --  So add prefix line to parse array
                     Pa (Pa_Last).Stem := Head (Tickons (I).Fix, Max_Stem_Size);
                     Pa (Pa_Last).IR := ((Prefix, Null_Prefix_Record),
                       0, Null_Ending_Record, X, X);
                     Pa (Pa_Last).D_K  := Addons;
                     Pa (Pa_Last).MNPC := Dict_IO.Count (Tickons (I).MNPC);
                  end if;

                  if Q_Word'Length >= 3   and then   --  qui is shortest QU_PRON
                    ((Q_Word (Q_Word'First .. Q_Word'First + 1) = "qu")  or
                    (Q_Word (Q_Word'First .. Q_Word'First + 1) = "cu"))
                  then
                     if Q_Word (Q_Word'First .. Q_Word'First + 1) = "qu"  then
                        Qkey := 1;
                        Process_Qu_Pronouns (Q_Word, Qkey);
                     elsif Q_Word
                       (Q_Word'First .. Q_Word'First + 1) = "cu"
                     then
                        Qkey := 2;
                        Process_Qu_Pronouns (Q_Word, Qkey);
                     end if;
                     if Pa_Last <= Pa_Qstart + 1 and then Qkey > 0 then
                        --  If did not find a PACKON
                        if Q_Word
                          (Q_Word'First .. Q_Word'First + 1) = "qu"
                        then
                           Process_Packons (Q_Word);
                        elsif Q_Word
                          (Q_Word'First .. Q_Word'First + 1) = "cu"
                        then
                           Process_Packons (Q_Word);
                        end if;
                     else
                        exit;
                     end if;
                     if Pa_Last > Pa_Qstart + 1  then
                        exit;
                     end if;

                  elsif Input_Word'Length >= 6  then   --  aliqui as aliQU_PRON
                     if Input_Word
                       (Input_Word'First .. Input_Word'First + 4) = "aliqu"
                     then
                        Process_Qu_Pronouns (Input_Word, 1);
                     elsif Input_Word
                       (Input_Word'First .. Input_Word'First + 4) = "alicu"
                     then
                        Process_Qu_Pronouns (Input_Word, 2);
                     end if;
                  end if;

                  if Pa_Last = Pa_Start + 1  then    --  Nothing found
                     Pa_Last := Pa_Start;             --  Reset PA_LAST
                  else
                     exit;
                  end if;
               end if;
            end;
         end loop;

         Words_Mode := Saved_Mode_Array;
      exception
         when others =>
            Words_Mode := Saved_Mode_Array;
      end Qu;

      --==========================================================
      declare
         Sss : Sal := (others => Null_Parse_Record);
         Ss  : Sal := (others => Null_Parse_Record);
      begin
         Run_Inflections (Input_Word, Ss);
         Prune_Stems (Input_Word, Ss, Sss);
         if Sss (1) /= Null_Parse_Record   then
            Order_Stems (Sss);
            Array_Stems (Sss, Pa, Pa_Last);
            Sss (1) := Null_Parse_Record;
         end if;
      end;
      --==========================================================

      if Pa_Last = Pa_Save  then
         Try_Tackons (Input_Word);
      end if;
   exception
      when Storage_Error =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output,
           "STORAGE_ERROR exception in WORD while processing =>"
           & Raw_Word);
         Pa_Last := Pa_Save;
         if Words_Mode (Write_Unknowns_To_File)  then
            Ada.Text_IO.Put (Unknowns, Raw_Word);
            Ada.Text_IO.Set_Col (Unknowns, 21);
            Ada.Text_IO.Put_Line (Unknowns, "========   STORAGE_ERROR  ");
         end if;
      when others =>
         if Words_Mode (Write_Unknowns_To_File)  then
            Ada.Text_IO.Put (Unknowns, Raw_Word);
            Ada.Text_IO.Set_Col (Unknowns, 21);
            Ada.Text_IO.Put_Line (Unknowns, "========   ERROR  ");
         end if;
         Pa_Last := Pa_Save;
   end Word;

   procedure Initialize_Word_Package is
   begin                                  --  Initializing WORD_PACKAGE

      Establish_Inflections_Section;

      Lel_Section_Io.Open (Inflections_Sections_File, Lel_Section_Io.In_File,
        Inflections_Sections_Name);

      Try_To_Load_Dictionary (General);

      Try_To_Load_Dictionary (Special);

      Load_Local :
      begin
         --  First check if there is a LOC dictionary
         Check_For_Local_Dictionary :
         declare
            Dummy : Ada.Text_IO.File_Type;
         begin
            Ada.Text_IO.Open (Dummy, Ada.Text_IO.In_File,
              Add_File_Name_Extension (Dictionary_File_Name,
              "LOCAL"));
            --  Failure to OPEN will raise an exception, to be handled below
            Ada.Text_IO.Close (Dummy);
         end Check_For_Local_Dictionary;
         --  If the above does not exception out, we can load LOC
         Preface.Put ("LOCAL ");
         Dict_Loc := Null_Dictionary;
         Load_Dictionary (Dict_Loc,
           Add_File_Name_Extension (Dictionary_File_Name, "LOCAL"));
         --  Need to carry LOC through consistently on LOAD_D and LOAD_D_FILE
         Load_Stem_File (Local);
         Dictionary_Available (Local) := True;
      exception
         when others  =>
            Dictionary_Available (Local) := False;
      end Load_Local;

      Load_Uniques (Unq, Uniques_Full_Name);

      Load_Addons (Addons_Full_Name);

      Load_Bdl_From_Disk;

      if not (Dictionary_Available (General)  or
        Dictionary_Available (Special)  or
        Dictionary_Available (Local))
      then
         Preface.Put_Line
           ("There are no main dictionaries - program will not do much");
         Preface.Put_Line
           ("Check that there are dictionary files in this subdirectory");
         Preface.Put_Line
           ("Except DICT.LOC that means DICTFILE, INDXFILE, STEMFILE");
      end if;

      Try_To_Load_English_Words :
      begin
         English_Dictionary_Available (General) := False;
         Ewds_Direct_Io.Open
           (Ewds_File, Ewds_Direct_Io.In_File, "EWDSFILE.GEN");

         English_Dictionary_Available (General) := True;
      exception
         when others  =>
            Preface.Put_Line ("No English available");
            English_Dictionary_Available (General) := False;
      end Try_To_Load_English_Words;

   end Initialize_Word_Package;

end Words_Engine.Word_Package;
