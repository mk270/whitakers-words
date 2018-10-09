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

with Ada.Integer_Text_IO;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Preface;
package body Latin_Utils.Inflections_Package is

   ---------------------------------------------------------------------------

   function "<" (Left, Right : Decn_Record) return Boolean is
   begin
      if Left.Which < Right.Which  or else
        (Left.Which = Right.Which  and then
        Left.Var < Right.Var)
      then
         return True;
      else
         return False;
      end if;
   end "<";

   ---------------------------------------------------------------------------
   -- FIXME: should the function below, comparing two Quality_Records
   -- not call the function above, comparing two Decn_Records?
   function "<" (Left, Right : Quality_Record) return Boolean is
   begin
      if Left.Pofs = Right.Pofs  then
         case Left.Pofs is
            when N =>
               if Left.Noun.Decl.Which < Right.Noun.Decl.Which  or else
                 (Left.Noun.Decl.Which = Right.Noun.Decl.Which  and then
                 Left.Noun.Decl.Var < Right.Noun.Decl.Var)  or else
                 (Left.Noun.Decl.Which = Right.Noun.Decl.Which  and then
                 Left.Noun.Decl.Var = Right.Noun.Decl.Var  and then
                 Left.Noun.Number < Right.Noun.Number) or else
                 (Left.Noun.Decl.Which = Right.Noun.Decl.Which  and then
                 Left.Noun.Decl.Var = Right.Noun.Decl.Var  and then
                 Left.Noun.Number = Right.Noun.Number and then
                 Left.Noun.Of_Case < Right.Noun.Of_Case) or else
                 (Left.Noun.Decl.Which = Right.Noun.Decl.Which  and then
                 Left.Noun.Decl.Var = Right.Noun.Decl.Var  and then
                 Left.Noun.Number = Right.Noun.Number and then
                 Left.Noun.Of_Case = Right.Noun.Of_Case and then
                 Left.Noun.Gender < Right.Noun.Gender)
               then
                  return True;
               end if;
            when Pron =>
               if Left.Pron.Decl.Which < Right.Pron.Decl.Which  or else
                 (Left.Pron.Decl.Which = Right.Pron.Decl.Which  and then
                 Left.Pron.Decl.Var < Right.Pron.Decl.Var)  or else
                 (Left.Pron.Decl.Which = Right.Pron.Decl.Which  and then
                 Left.Pron.Decl.Var = Right.Pron.Decl.Var  and then
                 Left.Pron.Number < Right.Pron.Number) or else
                 (Left.Pron.Decl.Which = Right.Pron.Decl.Which  and then
                 Left.Pron.Decl.Var = Right.Pron.Decl.Var  and then
                 Left.Pron.Number = Right.Pron.Number and then
                 Left.Pron.Of_Case < Right.Pron.Of_Case) or else
                 (Left.Pron.Decl.Which = Right.Pron.Decl.Which  and then
                 Left.Pron.Decl.Var = Right.Pron.Decl.Var  and then
                 Left.Pron.Number = Right.Pron.Number and then
                 Left.Pron.Of_Case = Right.Pron.Of_Case and then
                 Left.Pron.Gender < Right.Pron.Gender)
               then
                  return True;
               end if;
            when Pack =>
               if Left.Pack.Decl.Which < Right.Pack.Decl.Which  or else
                 (Left.Pack.Decl.Which = Right.Pack.Decl.Which  and then
                 Left.Pack.Decl.Var < Right.Pack.Decl.Var)  or else
                 (Left.Pack.Decl.Which = Right.Pack.Decl.Which  and then
                 Left.Pack.Decl.Var = Right.Pack.Decl.Var  and then
                 Left.Pack.Number < Right.Pack.Number) or else
                 (Left.Pack.Decl.Which = Right.Pack.Decl.Which  and then
                 Left.Pack.Decl.Var = Right.Pack.Decl.Var  and then
                 Left.Pack.Number = Right.Pack.Number and then
                 Left.Pack.Of_Case < Right.Pack.Of_Case) or else
                 (Left.Pack.Decl.Which = Right.Pack.Decl.Which  and then
                 Left.Pack.Decl.Var = Right.Pack.Decl.Var  and then
                 Left.Pack.Number = Right.Pack.Number and then
                 Left.Pack.Of_Case = Right.Pack.Of_Case and then
                 Left.Pack.Gender < Right.Pack.Gender)
               then
                  return True;
               end if;
            when Adj =>
               if Left.Adj.Decl.Which < Right.Adj.Decl.Which  or else
                 (Left.Adj.Decl.Which = Right.Adj.Decl.Which  and then
                 Left.Adj.Decl.Var < Right.Adj.Decl.Var)  or else
                 (Left.Adj.Decl.Which = Right.Adj.Decl.Which  and then
                 Left.Adj.Decl.Var = Right.Adj.Decl.Var  and then
                 Left.Adj.Number < Right.Adj.Number) or else
                 (Left.Adj.Decl.Which = Right.Adj.Decl.Which  and then
                 Left.Adj.Decl.Var = Right.Adj.Decl.Var  and then
                 Left.Adj.Number = Right.Adj.Number and then
                 Left.Adj.Of_Case < Right.Adj.Of_Case) or else
                 (Left.Adj.Decl.Which = Right.Adj.Decl.Which  and then
                 Left.Adj.Decl.Var = Right.Adj.Decl.Var  and then
                 Left.Adj.Number = Right.Adj.Number and then
                 Left.Adj.Of_Case = Right.Adj.Of_Case and then
                 Left.Adj.Gender < Right.Adj.Gender)  or else
                 (Left.Adj.Decl.Which = Right.Adj.Decl.Which  and then
                 Left.Adj.Decl.Var = Right.Adj.Decl.Var  and then
                 Left.Adj.Number = Right.Adj.Number and then
                 Left.Adj.Of_Case = Right.Adj.Of_Case and then
                 Left.Adj.Gender = Right.Adj.Gender  and then
                 Left.Adj.Comparison < Right.Adj.Comparison)
               then
                  return True;
               end if;
            when Adv =>
               return Left.Adv.Comparison < Right.Adv.Comparison;
            when V =>
               if (Left.Verb.Con.Which < Right.Verb.Con.Which)  or else
                 (Left.Verb.Con.Which = Right.Verb.Con.Which  and then
                 Left.Verb.Con.Var < Right.Verb.Con.Var)  or else
                 (Left.Verb.Con.Which = Right.Verb.Con.Which  and then
                 Left.Verb.Con.Var = Right.Verb.Con.Var  and then
                 Left.Verb.Number < Right.Verb.Number) or else
                 (Left.Verb.Con.Which = Right.Verb.Con.Which  and then
                    Left.Verb.Con.Var = Right.Verb.Con.Var  and then
                    Left.Verb.Number = Right.Verb.Number and then
                    Left.Verb.Tense_Voice_Mood.Tense <
                    Right.Verb.Tense_Voice_Mood.Tense) or else
                 (Left.Verb.Con.Which = Right.Verb.Con.Which  and then
                    Left.Verb.Con.Var = Right.Verb.Con.Var  and then
                    Left.Verb.Number = Right.Verb.Number and then
                    Left.Verb.Tense_Voice_Mood.Tense =
                    Right.Verb.Tense_Voice_Mood.Tense and then
                    Left.Verb.Tense_Voice_Mood.Voice <
                    Right.Verb.Tense_Voice_Mood.Voice) or else
                 (Left.Verb.Con.Which = Right.Verb.Con.Which  and then
                    Left.Verb.Con.Var = Right.Verb.Con.Var  and then
                    Left.Verb.Number = Right.Verb.Number and then
                    Left.Verb.Tense_Voice_Mood.Tense =
                    Right.Verb.Tense_Voice_Mood.Tense and then
                    Left.Verb.Tense_Voice_Mood.Voice =
                    Right.Verb.Tense_Voice_Mood.Voice and then
                    Left.Verb.Tense_Voice_Mood.Mood <
                    Right.Verb.Tense_Voice_Mood.Mood)  or else
                 (Left.Verb.Con.Which = Right.Verb.Con.Which and then
                    Left.Verb.Con.Var = Right.Verb.Con.Var and then
                    Left.Verb.Number = Right.Verb.Number and then
                    Left.Verb.Tense_Voice_Mood.Tense =
                    Right.Verb.Tense_Voice_Mood.Tense and then
                    Left.Verb.Tense_Voice_Mood.Voice =
                    Right.Verb.Tense_Voice_Mood.Voice and then
                    Left.Verb.Tense_Voice_Mood.Mood  =
                    Right.Verb.Tense_Voice_Mood.Mood  and then
                    Left.Verb.Person < Right.Verb.Person)
               then
                  return True;
               end if;
            when Vpar =>
               if Left.Vpar.Con.Which < Right.Vpar.Con.Which  or else
                 (Left.Vpar.Con.Which = Right.Vpar.Con.Which  and then
                 Left.Vpar.Con.Var < Right.Vpar.Con.Var)  or else
                 (Left.Vpar.Con.Which = Right.Vpar.Con.Which  and then
                 Left.Vpar.Con.Var = Right.Vpar.Con.Var  and then
                 Left.Vpar.Number < Right.Vpar.Number) or else
                 (Left.Vpar.Con.Which = Right.Vpar.Con.Which  and then
                 Left.Vpar.Con.Var = Right.Vpar.Con.Var  and then
                 Left.Vpar.Number = Right.Vpar.Number and then
                 Left.Vpar.Of_Case < Right.Vpar.Of_Case) or else
                 (Left.Vpar.Con.Which = Right.Vpar.Con.Which  and then
                 Left.Vpar.Con.Var = Right.Vpar.Con.Var  and then
                 Left.Vpar.Number = Right.Vpar.Number and then
                 Left.Vpar.Of_Case = Right.Vpar.Of_Case and then
                 Left.Vpar.Gender < Right.Vpar.Gender)
               then
                  return True;
               end if;
            when Supine =>
               if Left.Supine.Con.Which < Right.Supine.Con.Which  or else
                 (Left.Supine.Con.Which = Right.Supine.Con.Which  and then
                 Left.Supine.Con.Var < Right.Supine.Con.Var)  or else
                 (Left.Supine.Con.Which = Right.Supine.Con.Which  and then
                 Left.Supine.Con.Var = Right.Supine.Con.Var  and then
                 Left.Supine.Number < Right.Supine.Number) or else
                 (Left.Supine.Con.Which = Right.Supine.Con.Which  and then
                 Left.Supine.Con.Var = Right.Supine.Con.Var  and then
                 Left.Supine.Number = Right.Supine.Number and then
                 Left.Supine.Of_Case < Right.Supine.Of_Case) or else
                 (Left.Supine.Con.Which = Right.Supine.Con.Which  and then
                 Left.Supine.Con.Var = Right.Supine.Con.Var  and then
                 Left.Supine.Number = Right.Supine.Number and then
                 Left.Supine.Of_Case = Right.Supine.Of_Case and then
                 Left.Supine.Gender < Right.Supine.Gender)
               then
                  return True;
               end if;
            when Prep =>
               return Left.Prep.Of_Case < Right.Prep.Of_Case;
            when Conj =>
               null;
            when Interj =>
               null;
            when Num =>
               if Left.Num.Decl.Which < Right.Num.Decl.Which  or else
                 (Left.Num.Decl.Which = Right.Num.Decl.Which  and then
                 Left.Num.Decl.Var < Right.Num.Decl.Var)  or else
                 (Left.Num.Decl.Which = Right.Num.Decl.Which  and then
                 Left.Num.Decl.Var = Right.Num.Decl.Var  and then
                 Left.Num.Number < Right.Num.Number) or else
                 (Left.Num.Decl.Which = Right.Num.Decl.Which  and then
                 Left.Num.Decl.Var = Right.Num.Decl.Var  and then
                 Left.Num.Number = Right.Num.Number and then
                 Left.Num.Of_Case < Right.Num.Of_Case) or else
                 (Left.Num.Decl.Which = Right.Num.Decl.Which  and then
                 Left.Num.Decl.Var = Right.Num.Decl.Var  and then
                 Left.Num.Number = Right.Num.Number and then
                 Left.Num.Of_Case = Right.Num.Of_Case and then
                 Left.Num.Gender < Right.Num.Gender)  or else
                 (Left.Num.Decl.Which = Right.Num.Decl.Which  and then
                 Left.Num.Decl.Var = Right.Num.Decl.Var  and then
                 Left.Num.Number = Right.Num.Number and then
                 Left.Num.Of_Case = Right.Num.Of_Case and then
                 Left.Num.Gender = Right.Num.Gender  and then
                 Left.Num.Sort < Right.Num.Sort)
               then
                  return True;
               end if;
            when Tackon .. Suffix =>
               null;
            when X =>
               null;
         end case;
      else
         return Left.Pofs < Right.Pofs;
      end if;
      return False;
   exception
      when Constraint_Error  =>
         return Left.Pofs < Right.Pofs;
   end "<";

   overriding function "<="
     (Left, Right : Part_Of_Speech_Type)
     return Boolean is
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

   function "<="
     (Left, Right : Decn_Record)
     return Boolean is
   begin
      if Right = Left  or else
        (Right = Decn_Record'(0, 0)  and Left.Which /= 9)  or else
        Right = Decn_Record'(Left.Which, 0)
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<="
     (Left, Right : Gender_Type)
     return Boolean is
   begin
      if Right = Left  or else
        Right = X     or else
        (Right = C  and then (Left = M or Left = F))
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<="
     (Left, Right : Case_Type)
     return Boolean is
   begin
      if Right = Left or else Right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<="
     (Left, Right : Number_Type)
     return Boolean is
   begin
      if Right = Left or else Right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (Left, Right : Person_Type) return Boolean is
   begin
      if Right = Left or else Right = 0 then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (Left, Right : Comparison_Type) return Boolean is
   begin
      if Right = Left or else Right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   function "<=" (Left, Right : Tense_Voice_Mood_Record)  return Boolean is
   begin
      if (Right.Tense = Left.Tense or else Right.Tense = X) and then
         (Right.Voice = Left.Voice or else Right.Voice = X) and then
         (Right.Mood = Left.Mood or else Right.Mood = X)
      then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (Left, Right : Noun_Kind_Type)   return Boolean is
   begin
      if Right = Left or else Right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<="
     (Left, Right : Pronoun_Kind_Type)
     return Boolean is
   begin
      if Right = Left or else Right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (Left, Right : Stem_Key_Type)   return Boolean is
   begin            --  Only works for 2 stem parts, not verbs
      if Right = Left or else Right = 0 then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (Left, Right : Age_Type) return Boolean is
   begin
      if Right = Left or else Right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   overriding function "<=" (Left, Right : Frequency_Type) return Boolean is
   begin
      if Right = Left or else Right = X then
         return True;
      else
         return False;
      end if;
   end "<=";

   ---------------------------------------------------------------------------

   package body Stem_Type_IO is separate;
   package body Decn_Record_IO is separate;
   package body Tense_Voice_Mood_Record_IO is separate;
   package body Noun_Record_IO is separate;
   package body Pronoun_Record_IO is separate;
   package body Propack_Record_IO is separate;
   package body Adjective_Record_IO is separate;
   package body Numeral_Record_IO is separate;
   package body Adverb_Record_IO is separate;
   package body Verb_Record_IO is separate;
   package body Vpar_Record_IO is separate;
   package body Supine_Record_IO is separate;
   package body Preposition_Record_IO is separate;
   package body Conjunction_Record_IO is separate;
   package body Interjection_Record_IO is separate;
   package body Tackon_Record_IO is separate;
   package body Prefix_Record_IO is separate;
   package body Suffix_Record_IO is separate;
   package body Quality_Record_IO is separate;
   package body Ending_Record_IO is separate;
   package body Inflection_Record_IO is separate;

   ---------------------------------------------------------------------------

   procedure Establish_Inflections_Section  is
      --  Loads the inflection array from the file prepared in
      --  FILE_INFLECTIONS_SECTION
      --  If N = 0 (an artificial flag for the section for blank
      --  inflections = 5)
      --  computes the LELL .. LELF indices for use in WORD
      use Lel_Section_Io;

      procedure Load_Lel_Indexes is
         --  Load arrays from file
         I  : Integer := 0;
         N, Xn : Integer := 0;
         Ch, Xch : Character := ' ';
         Inflections_Sections_File : Lel_Section_Io.File_Type;

         -- FIXME: This enumeration and its values should be changed to
         -- something more meaningful.
         type Paradigm is (P1, P2, P3, P4, P5);

         procedure Read_Inflections (P : Paradigm)
         is
            Count : constant Integer := Paradigm'Pos (P) + 1;
         begin
            if P /= P5 then
               Lel_Section_Io.Read (Inflections_Sections_File,
                 Lel,
                 Lel_Section_Io.Positive_Count (Count));
               I := 1;
            end if;

            N := Lel (I).Ending.Size;
            Ch := Lel (I).Ending.Suf (N);

            Xn := N;
            Xch := Ch;
            if P /= P5 then
               Lelf (N, Ch) := I;
            else
               Pelf (N, Ch) := I;
               Pell (N, Ch) := 0;
            end if;

            C1_Loop :
            loop
               N1_Loop :
               loop
                  case P is
                     when P1 | P2 | P3 | P5 =>
                        exit C1_Loop when Lel (I) = Null_Inflection_Record;
                     when P4 =>
                        exit C1_Loop when  Lel (I).Qual.Pofs = Pron  and then
                          (Lel (I).Qual.Pron.Decl.Which = 1  or
                          Lel (I).Qual.Pron.Decl.Which = 2);
                  end case;

                  N := Lel (I).Ending.Size;
                  Ch := Lel (I).Ending.Suf (N);

                  case P is
                     when P1 | P4 | P5 =>
                        null;
                     when P2 =>
                        exit N1_Loop when Ch > 'r';
                     when P3 =>
                        exit N1_Loop when Ch > 's';
                  end case;

                  if P /= P5 then
                     if Ch /= Xch  then
                        Lell (Xn, Xch) := I - 1;
                        Lelf (N, Ch) := I;
                        Lell (N, Ch) := 0;
                        Xch := Ch;
                        Xn := N;
                     elsif N /= Xn  then
                        Lell (Xn, Ch) := I - 1;
                        Lelf (N, Ch) := I;
                        Lell (N, Ch) := 0;
                        Xn := N;
                        exit N1_Loop;
                     end if;
                  else
                     if Ch /= Xch  then
                        Pell (Xn, Xch) := I - 1;
                        Pelf (N, Ch) := I;
                        Pell (N, Ch) := 0;
                        Xch := Ch;
                        Xn := N;
                     elsif N /= Xn  then
                        Pell (Xn, Ch) := I - 1;
                        Pelf (N, Ch) := I;
                        Pell (N, Ch) := 0;
                        Xn := N;
                        exit N1_Loop;
                     end if;
                  end if;

                  I := I + 1;

               end loop N1_Loop;
            end loop C1_Loop;

            if P /= P5 then
               Lell (Xn, Xch) := I - 1;
            end if;
         end Read_Inflections;

      begin
         Open (Inflections_Sections_File, In_File, Inflections_Sections_Name);
         Number_Of_Inflections := 0;

         Lel_Section_Io.Read (Inflections_Sections_File,
           Lel,
           Lel_Section_Io.Positive_Count (5));

         I := 1;
         Belf (0, ' ') := I;
         Bell (0, ' ') := 0;
         loop
            exit when Lel (I) = Null_Inflection_Record;
            Bel (I) := Lel (I);

            Bell (0, ' ') := I;
            I := I + 1;
         end loop;

         for K in Paradigm'Range loop
            if K /= P5 then
               Number_Of_Inflections := Number_Of_Inflections + I - 1;
            end if;
            Read_Inflections (K);
         end loop;

         Pell (Xn, Xch) := I - 1;
         Number_Of_Inflections := Number_Of_Inflections + I - 1;
         Close (Inflections_Sections_File);
      end Load_Lel_Indexes;

   begin
      Preface.Put ("INFLECTION_ARRAY being loaded");
      Preface.Set_Col (33);
      Preface.Put ("--  ");
      Load_Lel_Indexes;                    --  Makes indexes from array
      Preface.Put (Number_Of_Inflections, 6);
      Preface.Put (" entries");
      Preface.Set_Col (55); Preface.Put_Line ("--  Loaded correctly");
   exception
      when Ada.Text_IO.Name_Error  =>
         New_Line;
         Put_Line ("There is no " & Inflections_Sections_Name & " file.");
         Put_Line ("The program cannot work without one.");
         Put_Line ("Make sure you are in the"
           & " subdirectory containing the files");
         Put_Line ("for inflections, dictionary, addons and uniques.");
         raise Give_Up;
   end Establish_Inflections_Section;

begin
   --  initialization of body of INFLECTIONS_PACKAGE

   Part_Of_Speech_Type_IO.Default_Width := Part_Of_Speech_Type'Width;
   Gender_Type_IO.Default_Width := Gender_Type'Width;
   Case_Type_IO.Default_Width := Case_Type'Width;
   Number_Type_IO.Default_Width := Number_Type'Width;
   Person_Type_IO.Default_Width := 1;
   Comparison_Type_IO.Default_Width := Comparison_Type'Width;

   Tense_Type_IO.Default_Width := Tense_Type'Width;
   Voice_Type_IO.Default_Width := Voice_Type'Width;
   Mood_Type_IO.Default_Width := Mood_Type'Width;

   Numeral_Sort_Type_IO.Default_Width := Numeral_Sort_Type'Width;

end Latin_Utils.Inflections_Package;
