-- WORDS, a Latin dictionary, by Colonel William Whitaker (USAF, Retired)
--
-- Copyright William A. Whitaker (1936-2010)
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
with Ada.Text_IO;
with Latin_Utils.Config;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Words_Engine.English_Support_Package;
use Words_Engine.English_Support_Package;
with Weed;
with Weed_All;
with Support_Utils.Dictionary_Form;
with Latin_Utils.General;
use Latin_Utils;
procedure Makeewds is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Part_Of_Speech_Type_IO;
   use Age_Type_IO;
   use Area_Type_IO;
   use Geo_Type_IO;
   use Frequency_Type_IO;
   use Source_Type_IO;
   use Ewds_Record_Io;

   Porting  : constant Boolean := False;
   Checking : constant Boolean := True;

   D_K : Dictionary_Kind := Xxx;       --  ######################

   Start_Stem_1  : constant := 1;
   Start_Stem_2  : constant := Start_Stem_1 + Max_Stem_Size + 1;
   Start_Stem_3  : constant := Start_Stem_2 + Max_Stem_Size + 1;
   Start_Stem_4  : constant := Start_Stem_3 + Max_Stem_Size + 1;
   Start_Part    : constant := Start_Stem_4 + Max_Stem_Size + 1;

   Line_Number : Integer := 0;

   subtype Line_Type is String (1 .. 400);

   N : Integer := 0;

   Input, Output, Check : Ada.Text_IO.File_Type;
   De : Dictionary_Entry;

   S, Line : Line_Type := (others => ' ');
   Blank_Line : constant Line_Type := (others => ' ');
   L, Last : Integer := 0;

   Ewa : Ewds_Array (1 .. 40) := (others => Null_Ewds_Record);

   Ewr : Ewds_Record := Null_Ewds_Record;

   --  First we supplement MEAN with singles of any hyphenated words
   --  In principle this could be done in the main EXTRACT, much same logic/code
   --  However this is difficult code for an old man, EXTRACT was hard
   --  when I was a bit younger, and I cannot remember anything about it.
   --  Separating them out makes it much easier to test

   function Add_Hyphenated (S : String) return String is

      --------  I tried to do something with hyphenated but so far it
      --------  does not work

      --  Find hyphenated words and add them to MEAN with a / connector,
      --  right before the parse so one has both the individual words (may
      --  be more than two) and a single combined word
      --  counting-board -> counting board/countingboard

      --  Cannot be bigger:
      T : String (1 .. Max_Meaning_Size * 2 + 20) := (others => ' ');
      Word_Start : Integer := 1;
      Word_End   : Integer := 0;
      I, J, Jmax : Integer := 0;
      Hyphenated : Boolean := False;

   begin
      --PUT_LINE (S);
      while I < S'Last  loop
         I := I + 1;
         J := J + 1;
         Word_End := 0;
         --PUT (INTEGER'IMAGE (I) & "-");

         --  First clear away or ignore all the non-words stuff
         if S (I) = '|'   then     --  Skip continuation |'s
            Word_Start := I + 1;
            T (J) := S (I);
            J := J + 1;
            Jmax := Jmax + 1;
            null;
            I := I + 1;
         elsif S (I) = '"'   then     --  Skip "'S
            Word_Start := I + 1;
            T (J) := S (I);
            J := J + 1;
            Jmax := Jmax + 1;
            null;
            I := I + 1;
         else
            if S (I) = '('  then    --  ( .. .) not to be parsed
               T (J) := S (I);
               J := J + 1;
               Jmax := Jmax + 1;
               I := I + 1;
               while S (I) /= ')'  loop
                  T (J) := S (I);
                  J := J + 1;
                  Jmax := Jmax + 1;
                  I := I + 1;
               end loop;
               Word_Start := I + 2;   --  Skip };
               Word_End   := 0;
            elsif S (I) = '['  then    --  ( .. .) not to be parsed
               T (J) := S (I);
               J := J + 1;
               Jmax := Jmax + 1;
               I := I + 1;
               while S (I - 1 .. I) /= "=>"  loop
                  T (J) := S (I);
                  J := J + 1;
                  Jmax := Jmax + 1;
                  I := I + 1;
               end loop;
               Word_Start := I + 2;
               Word_End   := 0;

            end if;
            --  Finished with the non-word stuff

            if S (I) = '-' then
               Word_End := I - 1;

               --              if  (I /= S'FIRST)  and then    --  Not -word
               --               ( (S (I-1) /= ' ') and
               --                 (S (I-1) /= '/')  )  then
               --                 HYPHENATED := TRUE;
               --              end if;
            end if;

            if
              S (I) = ' '  or
              S (I) = '/'  or
              S (I) = ','  or
              S (I) = ';'  or
              S (I) = '!'  or
              S (I) = '?'  or
              S (I) = '+'  or
              S (I) = '*'  or
              S (I) = '"'  or
              S (I) = '('
            then
               Word_End := I - 1;

               if Hyphenated  then
                  T (J) := '/';
                  J := J + 1;
                  Jmax := Jmax + 1;
                  for K in Word_Start .. Word_End loop
                     if S (K) /= '-'  then
                        T (J) := S (K);
                        J := J + 1;
                        Jmax := Jmax + 1;
                     end if;
                  end loop;
                  Hyphenated := False;
               end if;

            end if;

            if  --WORD_END /= 0  and then
              S (I) = ' ' or S (I) = '/'
            then
               Word_Start := I + 1;
               Word_End   := 0;
            end if;
         end if;  --  On '|'

         --  Set up the Output to return
         --PUT ('|' & INTEGER'IMAGE (J) & '/' & INTEGER'IMAGE (I));
         T (J) := S (I);
         Jmax := Jmax + 1;

      end loop;  --  Over S'RANGE

      return T (1 .. Jmax);

   exception
      when others =>
         Put_Line ("ADD_HYPHENATED  Exception    LINE = " &
           Integer'Image (Line_Number));
         Put_Line (S);
         Put (De); New_Line;
         return T (1 .. Jmax);
   end Add_Hyphenated;

   procedure Extract_Words (S : in String;
                            Pofs : in Part_Of_Speech_Type;
                            N : out Integer;
                            Ewa : out Ewds_Array) is
      -- i, j, js, k, l, m, im, ic : Integer := 0;
      J, K, L, M, Im, Ic : Integer := 0;
      End_Semi : constant Integer := 1;
      --  Have to expand type to take care of hyphenated
      subtype X_Meaning_Type is String (1 .. Max_Meaning_Size * 2 + 20);
      Null_X_Meaning_Type : constant X_Meaning_Type := (others => ' ');
      Semi, Comma : X_Meaning_Type := Null_X_Meaning_Type;

      Ww : Integer := 0;    --  For debug
   begin
      -- i := 1;    --  Element Position in line, per SEMI
      J := 1;    --  Position in word
      K := 0;    --  SEMI - Division in line
      L := 1;    --  Position in MEAN, for EXTRACTing SEMI
      M := 1;    --  COMMA in SEMI
      N := 1;    --  Word number
      Im := 0;   --  Position in SEMI
      Ic := 0;   --  Position in COMMA

      Ewa (N) := Null_Ewds_Record;

      -- Slightly disparage extension
      if S (S'First) = '|' then
         K := 3;
      end if;

      while L <= S'Last loop  --  loop over MEAN
         if S (L) = ' ' then  --  Clear initial blanks
            L := L + 1;
         end if;

         Semi := Null_X_Meaning_Type;
         Im := 1;
         Extract_Semi : loop
            if S (L) = '|' then
               null;          --  Ignore continuation flag | as word
            elsif S (L) in '0' .. '9'  then
               null;         --  Ignore numbers
            elsif S (L) = ';'  then     --  Division Terminator
               K := K + 1;
               --PUT ('+');
               L := L + 1;  --  Clear ;
               exit Extract_Semi;
            elsif S (L) = '('  then  --  Skip ( .. .)  !
               while S (L) /= ')'   loop
                  --PUT ('+');
                  --PUT (INTEGER'IMAGE (L));
                  --PUT (S (L));
                  exit when L = S'Last;  -- Run out
                  L := L + 1;
               end loop;
               --              L := L + 1;    --  Clear the ')'
               --PUT ('^');
               --PUT (INTEGER'IMAGE (L));
               --PUT (S (L));
               if L > S'Last then
                  L := S'Last;
               else
                  if  S (L) = ';'  then  --  );
                     exit Extract_Semi;
                  end if;
               end if;
               --PUT (']');

               if L >= S'Last then  --  Ends in )
                                    --  PUT ('!');
                  exit Extract_Semi;
               end if;
               --PUT ('+');
               --L := L + 1;    --  Clear the ')'
            elsif L = S'Last then
               --PUT ('|');
               L := L + 1;     --  To end the loop
               exit Extract_Semi;

            else
               Semi (Im) := S (L);
               Im := Im + 1;
            end if;
            --PUT ('+');
            --IM := IM + 1;  --  To next Character
            L := L + 1;  --  To next Character
         end loop Extract_Semi;

         Ww := 10;

         Process_Semi : declare
            St : constant String := Trim (Semi);
            Sm : constant String (St'First .. St'Last) := St;
         begin
            if St'Length > 0  then
               Comma := Null_X_Meaning_Type;
               Im := Sm'First;
               M := 0;

               --I := SM'FIRST;
               --while  I <= ST'LAST  loop

               --PUT (S (I));
               --PUT ('*');

               --COMMA := NULL_X_MEANING_TYPE;

               Ic := 1;
               Loop_Over_Semi :
               while Im <= Sm'Last loop
                  Comma := Null_X_Meaning_Type;
                  Ww := 20;
                  Find_Comma :
                  loop
                     --PUT (INTEGER'IMAGE (IM) & " ( " & SM (IM));
                     if Sm (Im) = '('  then  --  Skip ( .. .)  !
                        while Sm (Im) /= ')'   loop
                           Im := Im + 1;
                        end loop;
                        Im := Im + 1;
                        --  Clear the ')'
                        --        IM := IM + 1;    --  Go to next Character
                        if Im >= End_Semi  then
                           exit Find_Comma;
                        end if;
                        if  (Sm (Im) = ';') or (Sm (Im) = ',')   then
                           --  Foumd COMMA
                           M := M + 1;
                           Ic := 1;
                           Im := Im + 1;       --  Clear ;,
                           exit Find_Comma;
                        elsif Sm (Im) = ' '  then
                           Im := Im + 1;
                        end if;
                        --PUT_LINE ("------------------------");
                     end if;

                     if Sm (Im) = '['  then  --  Take end of [=>]
                        while Sm (Im) /= '>'  loop
                           exit when Sm (Im) = ']'; --  If no >
                           Im := Im + 1;
                        end loop;
                        Im := Im + 1;    --  Clear the '>' or ']'
                        if  Sm (Im) = ';'  then
                           --  Foumd COMMA
                           M := M + 1;
                           Ic := 1;
                           Im := Im + 1;       --  Clear ;
                           exit Find_Comma;
                        elsif Sm (Im) = ' '  then
                           Im := Im + 1;
                        end if;
                     end if;
                     --  But could be 2 =>!
                     --PUT_LINE ("Through ()[] I = " & INTEGER'IMAGE (I));
                     exit Find_Comma when Im > Sm'Last;

                     --PUT (INTEGER'IMAGE (IM) & " ) " & SM (IM));
                     if Sm (Im) = ','  then
                        --  Foumd COMMA
                        M := M + 1;
                        Ic := 1;
                        Im := Im + 1;       --  Clear ,
                        exit Find_Comma;
                     elsif Im >= Sm'Last or Im = S'Last then
                        --  Foumd COMMA
                        Comma (Ic) := Sm (Im);
                        M := M + 1;
                        Ic := 1;
                        exit Find_Comma;
                     else
                        Comma (Ic) := Sm (Im);
                        Im := Im + 1;
                        Ic := Ic + 1;

                     end if;
                     --PUT (INTEGER'IMAGE (IM) & " ! " & SM (IM));

                  end loop Find_Comma;
                  Im := Im + 1;

                  Ww := 30;

                  Process_Comma :
                  declare
                     Ct : constant String := Trim (Comma);
                     Cs : String (Ct'First .. Ct'Last) := Ct;
                     Pure : Boolean := True;
                     W_Start, W_End : Integer := 0;
                  begin
                     Ww := 31;
                     if Ct'Length > 0 then
                        --  Is COMMA non empty
                        --  Are there any blanks?
                        --  If not then it is a pure word
                        --  Or words with /
                        for Ip in Cs'Range loop
                           if Cs (Ip) = ' '  then
                              Pure := False;
                           end if;
                        end loop;

                        Ww := 32;

                        --  Check for WEED words and eliminate them
                        W_Start := Cs'First;
                        W_End   := Cs'Last;
                        for Iw in Cs'Range loop
                           --PUT ('-');
                           --PUT (CS (IW));
                           if (Cs (Iw) = '(')  or
                             (Cs (Iw) = '[')
                           then
                              Ww := 33;
                              W_Start := Iw + 1;
                           else
                              Ww := 34;
                              if (Cs (Iw) = ' ')  or
                                (Cs (Iw) = '_')  or
                                (Cs (Iw) = '-')  or
                                (Cs (Iw) = ''')  or
                                (Cs (Iw) = '!')  or
                                (Cs (Iw) = '/')  or
                                (Cs (Iw) = ':')  or
                                (Cs (Iw) = '.')  or
                                (Cs (Iw) = '!')  or
                                (Cs (Iw) = ')')  or
                                (Cs (Iw) = ']')  or
                                (Iw = Cs'Last)
                              then
                                 Ww := 35;
                                 if Iw = Cs'Last then
                                    W_End := Iw;
                                 elsif Iw /= Cs'First then
                                    W_End := Iw - 1;
                                 end if;

                                 Ww := 36;
                                 --  KLUDGE
                                 if Cs (W_Start) = '"' then
                                    Ww := 361;
                                    W_Start := W_Start + 1;
                                    Ww := 362;
                                 elsif Cs (W_End) = '"' then
                                    Ww := 364;
                                    W_End := W_End - 1;
                                    Ww := 365;
                                 end if;

                                 Ww := 37;

                                 --& "  " & CS (W_START .. W_END)
                                 --);
                                 Weed_All (Cs (W_Start .. W_End));

                                 if not Pure then
                                    Weed (Cs (W_Start .. W_End), Pofs);
                                 end if;
                                 W_Start := Iw + 1;
                              end if;
                              Ww := 38;
                           end if;
                           Ww := 39;
                        end loop;          --  On CS'RANGE

                        --PUT_LINE (INTEGER'IMAGE (LINE_NUMBER) & "WEED done");

                        Ww := 40;
                        --  Main process of COMMA
                        Ic := 1;
                        J := 1;
                        while  Ic <= Cs'Last  loop
                           --PUT (CS (IC));

                           if Cs (Ic) = '"'  or      --  Skip all "
                             Cs (Ic) = '('  or      --  Skip initial (
                             Cs (Ic) = '?'  or      --  Ignore ?
                             Cs (Ic) = '~'  or      --  Ignore about ~
                             Cs (Ic) = '*'  or
                             Cs (Ic) = '%'  or      --  Ignore % unless word
                             Cs (Ic) = '.'  or      --  Ignore . ..
                             Cs (Ic) = '\'  or      --  Ignore weed
                             (Cs (Ic) in '0' .. '9')
                           then --  Skip numbers
                              Ic := Ic + 1;
                              Ww := 50;
                              ----PUT ('-');
                           else
                              if
                                Cs (Ic) = '/'  or
                                Cs (Ic) = ' '  or
                                Cs (Ic) = '''  or --  Ignore all ' incl 's  ???
                                Cs (Ic) = '-'  or --  Hyphen causes 2 words  XXX
                                Cs (Ic) = '+'  or --  Plus causes 2 words
                                Cs (Ic) = '_'  or --  Underscore causes 2 words
                                Cs (Ic) = '='  or --  = space/terminates
                                Cs (Ic) = '>'  or
                                Cs (Ic) = ')'  or
                                Cs (Ic) = ']'  or
                                Cs (Ic) = '!'  or
                                Cs (Ic) = '?'  or
                                Cs (Ic) = '+'  or
                                Cs (Ic) = ':'  or
                                Cs (Ic) = ']'
                              then  --  Found word
                                 Ww := 60;
                                 --PUT ('/');
                                 Ewa (N).Semi := K;
                                 if Pure  then
                                    if K = 1  then
                                       Ewa (N).Kind := 15;
                                    else
                                       Ewa (N).Kind := 10;
                                    end if;
                                 else
                                    Ewa (N).Kind := 0;
                                 end if;
                                 Ww := 70;
                                 N := N + 1;       --  Start new word in COMMA
                                 Ic := Ic + 1;
                                 J := 1;
                                 Ewa (N) := Null_Ewds_Record;

                              elsif Ic = Cs'Last then --  Order of if important
                                                      --  End, Found word
                                                      --PUT ('!');
                                 Ewa (N).W (J) := Cs (Ic);
                                 Ewa (N).Semi := K;
                                 if Pure  then
                                    if K = 1  then
                                       Ewa (N).Kind := 15;
                                    else
                                       Ewa (N).Kind := 10;
                                    end if;
                                 else
                                    Ewa (N).Kind := 0;
                                 end if;
                                 N := N + 1;       --  Start new word/COMMA

                                 Ewa (N) := Null_Ewds_Record;
                                 exit;
                              else
                                 Ww := 80;
                                 --PUT ('+');
                                 Ewa (N).W (J) := Cs (Ic);
                                 J := J + 1;
                                 Ic := Ic + 1;
                              end if;
                           end if;
                           Ww := 90;
                        end loop;

                     end if;  -- On COMMA being empty
                  end Process_Comma;
                  --PUT_LINE ("COMMA Processed ");

               end loop Loop_Over_Semi;

               --PUT_LINE ("LOOP OVER SEMI Processed ");

            end if;
            -- On ST'LENGTH > 0
            --PUT_LINE ("LOOP OVER SEMI after ST'LENGTH  0 ");
         end Process_Semi;

         --PUT_LINE ("SEMI Processed ");
         -- I = "  & INTEGER'IMAGE (I)
         --& "  S (I) = " & S (I)
         --);
         if (L < S'Last) and then (S (L) = ';') then
            --  ??????
            --PUT_LINE ("Clear  L = " & INTEGER'IMAGE (L));
            L := L + 1;
         end if;

         -- investigate this:
         -- js := l;    --  Odd but necessary    ?????
         for J in L .. S'Last loop
            exit when J = S'Last;
            if S (J) = ' ' then
               L := L + 1;
            else
               exit;
            end if;
         end loop;

         exit when L >= S'Last;
      end loop;   --  loop over MEAN

      --PUT_LINE ("SEMI loop Processed");

                  DROP_DUPES :
      for Z in Ewa'Range loop
         if Ewa (Z).W /= Null_Eword then

               INNER_LOOP :
               for ZZ in Z + 1 .. Ewa'Last loop
                  if Ewa (Z).W = Ewa (ZZ).W then
                     Ewa (ZZ).W := Null_Eword;
                  end if;
               end loop INNER_LOOP;

         end if;
      end loop DROP_DUPES;

      if Ewa (N) = Null_Ewds_Record  then
         N := N - 1;   --  Clean up danglers
      end if;
      if Ewa (N) = Null_Ewds_Record  then   --  AGAIN!!!!!!
         N := N - 1;   --  Clean up danglers
      end if;
   exception
      when others =>
         if (S (S'Last) /= ')') or  (S (S'Last) /= ']') then    --  KLUDGE
            New_Line;
            Put_Line ("Extract Exception    WW = "
              & Integer'Image (Ww) & "    LINE = " &
              Integer'Image (Line_Number));
            Put_Line (S);
            Put (De); New_Line;
         end if;
   end Extract_Words;

begin
   Put_Line ("Takes a DICTLINE.D_K and produces a EWDSLIST.D_K ");
   Latin_Utils.General.Load_Dictionary (Line, Last, D_K);

   Open (Input, In_File,
         Latin_Utils.Config.Path (Dict_Line_Name & '.' & Ext (D_K)));
   --PUT_LINE ("OPEN");

   if not Porting  then
      --PUT_LINE ("CREATING");

      Create (Output, Out_File, "EWDSLIST." & Ext (D_K));

      if Checking  then
         Create (Check, Out_File, "CHECKEWD.");
      end if;

      --PUT_LINE ("CREATED");
   end if;

   --  Now do the rest
   Over_Lines :
      while not End_Of_File (Input) loop
         S := Blank_Line;
         Get_Line (Input, S, Last);
         if Trim (S (1 .. Last)) /= ""  then       --  If non-blank line
            L := 0;

            Form_De : begin
               De.Stems (1) := S (Start_Stem_1 .. Max_Stem_Size);
               --NEW_LINE; PUT (DE.STEMS (1));
               De.Stems (2) := S (Start_Stem_2 .. Start_Stem_2
                 + Max_Stem_Size - 1);
               De.Stems (3) := S (Start_Stem_3 .. Start_Stem_3
                 + Max_Stem_Size - 1);
               De.Stems (4) := S (Start_Stem_4 .. Start_Stem_4
                 + Max_Stem_Size - 1);
               --PUT ('#'); PUT (INTEGER'IMAGE (L)); PUT (INTEGER'IMAGE (LAST));
               --PUT ('@');
               Get (S (Start_Part .. Last), De.Part, L);
               --PUT ('%'); PUT (INTEGER'IMAGE (L)); PUT (INTEGER'IMAGE (LAST));
               --PUT ('&'); PUT (S (L+1 .. LAST)); PUT ('3');
               --GET (S (L+1 .. LAST), DE.PART.POFS, DE.KIND, L);
               -- FIXME: Why not Translation_Record_IO.Put ?
               Get (S (L + 1 .. Last), De.Tran.Age, L);
               Get (S (L + 1 .. Last), De.Tran.Area, L);
               Get (S (L + 1 .. Last), De.Tran.Geo, L);
               Get (S (L + 1 .. Last), De.Tran.Freq, L);
               Get (S (L + 1 .. Last), De.Tran.Source, L);
               De.Mean := Head (S (L + 2 .. Last), Max_Meaning_Size);
               --  Note that this allows initial blanks
               --  L+2 skips over the SPACER, required because
               --  this is STRING, not ENUM

            exception
               when others =>
                  New_Line;
                  Put_Line ("GET Exception  LAST = " & Integer'Image (Last));
                  Put_Line (S (1 .. Last));
                  Put (Line_Number); New_Line;
                  Put (De); New_Line;
            end Form_De;

            Line_Number := Line_Number + 1;

            if De.Part.Pofs = V and then De.Part.V.Con.Which = 8 then
               --  V 8 is a kludge for variant forms of verbs
               --  that have regular forms elsewhere
               null;
            else
               --  Extract words
               Extract_Words (Add_Hyphenated (Trim (De.Mean)),
                 De.Part.Pofs, N, Ewa);

               --      EWORD_SIZE    : constant := 38;
               --      AUX_WORD_SIZE : constant := 9;
               --      LINE_NUMBER_WIDTH : constant := 10;
               --
               --      type EWDS_RECORD is
               --        record
               --          POFS : PART_OF_SPEECH_TYPE := X;
               --          W    : STRING (1 .. EWORD_SIZE);
               --          AUX  : STRING (1 .. AUX_WORD_SIZE);
               --          N    : INTEGER;
               --        end record;

               for I in 1 .. N  loop
                  if Trim (Ewa (I).W)'Length /= 0 then
                     Ewr.W := Head (Trim (Ewa (I).W), Eword_Size);
                     Ewr.Aux := Head ("",  Aux_Word_Size);
                     Ewr.N := Line_Number;
                     Ewr.Pofs := De.Part.Pofs;
                     Ewr.Freq := De.Tran.Freq;
                     Ewr.Semi := Ewa (I).Semi;
                     Ewr.Kind := Ewa (I).Kind;
                     Ewr.Rank := 80 - Frequency_Type'Pos (Ewr.Freq) * 10
                       + Ewr.Kind + (Ewr.Semi - 1) * (-3);
                     if Ewr.Freq = Inflections_Package.N  then
                        Ewr.Rank := Ewr.Rank + 25;
                     end if;

                     --PUT (EWA (I)); NEW_LINE;
                     --PUT (EWR); NEW_LINE;
                     Put (Output, Ewr);

                     -- SET_COL (OUTPUT, 71);
                     -- INTEGER_IO.PUT (OUTPUT, I, 2);

                     New_Line (Output);

                     if Checking  then
                        --  Now make the CHECK file

                        Put (Check, Ewr.W);
                        Set_Col (Check, 25);
                        declare
                           Df : constant String :=
                             Support_Utils.Dictionary_Form (De);
                           Ii : Integer := 1;
                        begin
                           if Df'Length > 0  then
                              while Df (Ii) /= ' '  and
                                Df (Ii) /= '.'     and
                                Df (Ii) /= ','
                              loop
                                 Put (Check, Df (Ii));
                                 Ii := Ii + 1;
                                 exit when Ii = 19;
                              end loop;
                           end if;
                        end;

                        Set_Col (Check, 44);
                        Put (Check, Ewr.N, 6);
                        Put (Check, ' ');
                        Put (Check, Ewr.Pofs);
                        Put (Check, ' ');
                        Put (Check, Ewr.Freq);
                        Put (Check, ' ');
                        Put (Check, Ewr.Semi, 5);
                        Put (Check, ' ');
                        Put (Check, Ewr.Kind, 5);
                        Put (Check, ' ');
                        Put (Check, Ewr.Rank, 5);
                        Put (Check, ' ');
                        Put (Check, De.Mean);

                        New_Line (Check);
                     end if;
                  end if;
               end loop;
            end if;  --  If non-blank line
         end if;
      end loop Over_Lines;

      Put_Line ("NUMBER_OF_LINES = " & Integer'Image (Line_Number));

      if not Porting  then
         Close (Output);
         if Checking  then
            Close (Check);
         end if;
      end if;

exception
   when Ada.Text_IO.Data_Error  =>
      null;
   when others =>
      Put_Line (S (1 .. Last));
      Put (Line_Number); New_Line;
      Close (Output);
      if Checking  then
         Close (Check);
      end if;
end Makeewds;
