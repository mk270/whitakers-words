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

-------------------------------------------------------------------------

-- This file contains the key routine `List_Stems`; this currently uses too
-- much state; ideally it should be a pure function, and making it so is
-- an important step in simplifying WORDS and exposing its engine to other
-- interfaces.
--
-- `List_Stems` contains a *lot* of duplicated code that could be factored
-- out, to be marked with "FACTOR OUT".

with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Latin_File_Names; use Latin_Utils.Latin_File_Names;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
with Support_Utils.Addons_Package; use Support_Utils.Addons_Package;
with Support_Utils.Uniques_Package; use Support_Utils.Uniques_Package;
with Support_Utils.Word_Support_Package; use Support_Utils.Word_Support_Package;
with Support_Utils.Developer_Parameters; use Support_Utils.Developer_Parameters;
with Word_Package; use Word_Package;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Support_Utils.Char_Utils;
with Support_Utils.Dictionary_Form;
with Put_Example_Line;
with List_Sweep;
with Put_Stat;
use Latin_Utils;
package body List_Package is

   subtype Xons is Part_Of_Speech_Type range Tackon .. Suffix;

   type Dictionary_MNPC_Record is record
      D_K  : Dictionary_Kind := Default_Dictionary_Kind;
      MNPC : MNPC_Type := Null_MNPC;
      De   : Dictionary_Entry := Null_Dictionary_Entry;
   end record;
   Null_Dictionary_MNPC_Record : constant Dictionary_MNPC_Record
     := (X, Null_MNPC, Null_Dictionary_Entry);

   Stem_Inflection_Array_Size       : constant := 10;
   Stem_Inflection_Array_Array_Size : constant := 40;

   type Stem_Inflection_Record is
      record
         Stem : Stem_Type          := Null_Stem_Type;
         Ir   : Inflection_Record  := Null_Inflection_Record;
      end record;

   Null_Stem_Inflection_Record      : constant Stem_Inflection_Record :=
     (Stem => Null_Stem_Type,
     Ir => Null_Inflection_Record);

   type Stem_Inflection_Array is
     array (Integer range <>) of Stem_Inflection_Record;
   type Stem_Inflection_Array_Array is array (Integer range <>)
     of Stem_Inflection_Array (1 .. Stem_Inflection_Array_Size);

   Null_Sra :
     constant Stem_Inflection_Array (1 .. Stem_Inflection_Array_Size)
     := (others => (Null_Stem_Type, Null_Inflection_Record));

   Dictionary_MNPC_Array_Size : constant := 40;

   type Dictionary_MNPC_Array is array (1 .. Dictionary_MNPC_Array_Size)
     of Dictionary_MNPC_Record;

   Max_Meaning_Print_Size : constant := 79;

   Inflection_Frequency : constant array (Frequency_Type) of String (1 .. 8) :=
     ("        ",  --  X
     "mostfreq",  --  A
     "sometime",  --  B
     "uncommon",  --  C
     "infreq  ",  --  D
     "rare    ",  --  E
     "veryrare",  --  F
     "inscript",  --  I
     "        ",  --  Not used
     "        ");
   Inflection_Age : constant array (Age_Type) of String (1 .. 8) :=
     ("Always  ",   --  X
     "Archaic ",   --  A
     "Early   ",   --  B
     "Classic ",   --  C
     "Late    ",   --  D
     "Later   ",   --  E
     "Medieval",   --  F
     "Scholar ",   --  G
     "Modern  "); --  H

   Dictionary_Frequency : constant array (Frequency_Type) of String (1 .. 8) :=
     ("        ",  --  X
     "veryfreq",  --  A
     "frequent",  --  B
     "common  ",  --  C
     "lesser  ",  --  D
     "uncommon",  --  E
     "veryrare",  --  F
     "inscript",  --  I
     "graffiti",  --  J
     "Pliny   "); --  N

   Dictionary_Age : constant array (Age_Type) of String (1 .. 8) :=
     ("        ",   --  X
     "Archaic ",   --  A
     "Early   ",   --  B
     "Classic ",   --  C
     "Late    ",   --  D
     "Later   ",   --  E
     "Medieval",   --  F
     "NeoLatin",   --  G
     "Modern  "); --  H

   function Get_Max_Meaning_Size (Output : Ada.Text_IO.File_Type)
     return Integer
   is
   begin
      if Ada.Text_IO.Name (Output) =
        Ada.Text_IO.Name (Ada.Text_IO.Standard_Output)
      then
         --  to keep from overflowing screen line or even adding blank line
         return Max_Meaning_Print_Size;
      else
         return Max_Meaning_Size;
      end if;
   end Get_Max_Meaning_Size;

   procedure Put_Pearse_Code (Output : Ada.Text_IO.File_Type;
                              Code   : String) is
   begin
      if Words_Mdev (Do_Pearse_Codes) then
         Ada.Text_IO.Put (Output, Code);
      end if;
   end Put_Pearse_Code;

   procedure Put_Dictionary_Flags (Output : Ada.Text_IO.File_Type;
                                   De     : Dictionary_Entry;
                                   Hit    : out Boolean) is
   begin

      if Words_Mode (Show_Age)   or
        (Trim (Dictionary_Age (De.Tran.Age))'Length /= 0)  --  Not X
      then
         Ada.Text_IO.Put (Output, "  " & Trim (Dictionary_Age (De.Tran.Age)));
         Hit := True;
      end if;
      if (Words_Mode (Show_Frequency) or
        (De.Tran.Freq >= D))  and
        (Trim (Dictionary_Frequency (De.Tran.Freq))'Length /= 0)
      then
         Ada.Text_IO.Put (Output, "  " &
           Trim (Dictionary_Frequency (De.Tran.Freq)));
         Hit := True;
      end if;
   end Put_Dictionary_Flags;

   procedure Put_Dictionary_Form (Output : Ada.Text_IO.File_Type;
                                  D_K    : Dictionary_Kind;
                                  MNPC   : Dict_IO.Count;
                                  De     : Dictionary_Entry)
   is
      Chit, Dhit, Ehit, Fhit, Lhit : Boolean := False; --  Things on this line?
      Dictionary_Line_Number : constant Integer := Integer (MNPC);
   begin                               --  PUT_DICTIONARY_FORM
      if Words_Mode (Do_Dictionary_Forms)  then
         Put_Pearse_Code (Output, "02 ");
         if Words_Mdev (Do_Pearse_Codes) then
            Dhit := True;
         end if;
         if Support_Utils.Dictionary_Form (De)'Length /= 0  then
            Ada.Text_IO.Put (Output, Support_Utils.Dictionary_Form (De) & "  ");
            Dhit := True;
         end if;
      end if;

      if Words_Mdev (Show_Dictionary_Codes) and then
        De.Part.Pofs not in Xons
      then
         Ada.Text_IO.Put (Output, " [");
         -- FIXME: Why noy Translation_Record_IO.Put ?
         Age_Type_IO.Put (Output, De.Tran.Age);
         Area_Type_IO.Put (Output, De.Tran.Area);
         Geo_Type_IO.Put (Output, De.Tran.Geo);
         Frequency_Type_IO.Put (Output, De.Tran.Freq);
         Source_Type_IO.Put (Output, De.Tran.Source);
         Ada.Text_IO.Put (Output, "]  ");
         Chit := True;
      end if;

      if Words_Mdev (Show_Dictionary) then
         Ada.Text_IO.Put (Output, Ext (D_K) & ">");
         Ehit := True;
      end if;

      if Words_Mdev (Show_Dictionary_Line)  then
         if Dictionary_Line_Number > 0  then
            Ada.Text_IO.Put (Output, "("
              & Trim (Integer'Image (Dictionary_Line_Number)) & ")");
            Lhit := True;
         end if;
      end if;

      Put_Dictionary_Flags (Output, De, Fhit);

      if Chit or Dhit or Ehit or Fhit or Lhit then
         Ada.Text_IO.New_Line (Output);
      end if;
      --end if;

   end Put_Dictionary_Form;

   function Constructed_Meaning
     (Sr  : Stem_Inflection_Record;
      Dm  : Dictionary_MNPC_Record)
     return String
   is
      --  Constructs the meaning for NUM from NUM.SORT and NUM_VALUE
      S : String (1 .. Max_Meaning_Size) := Null_Meaning_Type;
      N : Integer := 0;
   begin
      if Dm.De.Part.Pofs = Num  then
         N := Dm.De.Part.Num.Value;
         if Sr.Ir.Qual.Pofs = Num  then    --  Normal parse
            case Sr.Ir.Qual.Num.Sort is
               when Card  =>
                  S := Head (Integer'Image (N) &
                    " - (CARD answers 'how many');", Max_Meaning_Size);
               when Ord   =>
                  S := Head (Integer'Image (N) &
                    "th - (ORD, 'in series'); (a/the)" & Integer'Image (N) &
                    "th (part) (fract w/pars?);", Max_Meaning_Size);
               when Dist  =>
                  S := Head (Integer'Image (N) &
                    " each/apiece/times/fold/together/at a time" &
                    " - 'how many each'; by " &
                    Integer'Image (N) & "s; ", Max_Meaning_Size);
               when Adverb =>
                  S := Head (Integer'Image (N) &
                    " times, on" & Integer'Image (N) &
                    " occasions - (ADVERB answers 'how often');",
                    Max_Meaning_Size);
               when others =>
                  null;
            end case;
         else  -- there is fix so POFS is not NUM
            S := Head ("Number " & Integer'Image (N), Max_Meaning_Size);
         end if;
      end if;

      return S;
   end Constructed_Meaning;

   function Trim_Bar (S : String) return String is
      --  Takes vertical bars from begining of MEAN and TRIMs
   begin
      if S'Length > 3  and then S (S'First .. S'First + 3) = "||||"  then
         return Trim (S (S'First + 4 .. S'Last));
      elsif S'Length > 2  and then S (S'First .. S'First + 2) = "|||"  then
         return Trim (S (S'First + 3 .. S'Last));
      elsif S'Length > 1  and then  S (S'First .. S'First + 1) = "||"  then
         return Trim (S (S'First + 2 .. S'Last));
      elsif S (S'First) = '|'  then
         return Trim (S (S'First + 1 .. S'Last));
      else
         return Trim (S);
      end if;
   end Trim_Bar;

   procedure Put_Meaning_Line
     (Output : Ada.Text_IO.File_Type;
      Sr     : Stem_Inflection_Record;
      Dm     : Dictionary_MNPC_Record;
      Mm     : Integer)
   is
      use Dict_IO;

      procedure Put_Meaning (Output      : Ada.Text_IO.File_Type;
                             Raw_Meaning : String) is
         --  Handles the MM screen line limit and TRIM_BAR, then TRIMs
      begin
         Ada.Text_IO.Put (Output, Trim (Head (Trim_Bar (Raw_Meaning), Mm)));
      end Put_Meaning;

      procedure Put_Word_Meaning
        (Meaning : in out Meaning_Type;
         Code    : in     String)
      is
      begin
         if Meaning /= Null_Meaning_Type then
            Put_Pearse_Code (Output, Code);
            Put_Meaning (Output, Meaning);
            Meaning := Null_Meaning_Type;
            Ada.Text_IO.New_Line (Output);
         end if;
      end Put_Word_Meaning;
   begin
      case Dm.D_K is
         when Rrr => Put_Word_Meaning (Rrr_Meaning, "03 "); --  Roman Numeral
         when Nnn => Put_Word_Meaning (Nnn_Meaning, "06 "); --  Unknown Name
         when Xxx => Put_Word_Meaning (Xxx_Meaning, "06 "); --  TRICKS
         when Yyy => Put_Word_Meaning (Yyy_Meaning, "06 "); --  Syncope
         when Ppp => Put_Word_Meaning (Ppp_Meaning, "06 "); --  Compounds
         when Addons =>
            Put_Pearse_Code (Output, "06 ");
            Put_Meaning (Output, Means (Integer (Dm.MNPC)));
            Ada.Text_IO.New_Line (Output);
         when others =>
            Put_Pearse_Code (Output, "03 ");
            if Dm.De.Part.Pofs = Num  and then Dm.De.Part.Num.Value > 0  then
               Ada.Text_IO.Put_Line (Output, Constructed_Meaning (Sr, Dm));
               --  Constructed MEANING
            elsif Dm.D_K = Unique  then
               Put_Meaning (Output, Uniques_De (Dm.MNPC).Mean);
               Ada.Text_IO.New_Line (Output);
            else
               Put_Meaning (Output, Trim_Bar (Dm.De.Mean));
               Ada.Text_IO.New_Line (Output);
            end if;
      end case;
   end Put_Meaning_Line;

   --  Convert from PARSE_RECORDs to DICTIONARY_MNPC_RECORD
   --    and STEM_INFLECTION_RECORD
   procedure Cycle_Over_Pa
     (Pa            :  in Parse_Array;
      Pa_Last       :  in Integer;
      Sraa          : out Stem_Inflection_Array_Array;
      Dma           : out Dictionary_MNPC_Array;
      I_Is_Pa_Last  : out Boolean;
      Raw_Word, W   :  in String)
   is
      use Ada.Text_IO;
      use Dict_IO;
      I   : Integer := 1;
      J   : Integer := 0;
      K   : Integer := 0;
      Dm  : Dictionary_MNPC_Record := Null_Dictionary_MNPC_Record;
      Odm : Dictionary_MNPC_Record := Null_Dictionary_MNPC_Record;
      Dea : Dictionary_Entry := Null_Dictionary_Entry;
   begin

      while I <= Pa_Last  loop
         --  I cycles over full PA array
         Odm := Null_Dictionary_MNPC_Record;

         if Pa (I).D_K = Unique  then
            J := J + 1;
            Sraa (J)(1) := (Pa (I).Stem, Pa (I).IR);

            Dm := Null_Dictionary_MNPC_Record;
            Dm.D_K := Unique;
            Dm.MNPC := Pa (I).MNPC;
            Dm.De := Uniques_De (Pa (I).MNPC);
            Dma (J) := Dm;
            I := I + 1;
         else
            declare
               procedure Handle_Parse_Record is
               begin
                  if Pa (I).MNPC /= Odm.MNPC then
                     -- Encountering new MNPC
                     K := 1;
                     -- K indexes within the MNPCA array -- Initialise
                     J := J + 1;
                     -- J indexes the number of MNPCA arrays - Next MNPCA
                     Sraa (J)(K) := (Pa (I).Stem, Pa (I).IR);
                     Dict_IO.Set_Index (Dict_File (Pa (I).D_K), Pa (I).MNPC);
                     Dict_IO.Read (Dict_File (Pa (I).D_K), Dea);
                     Dm := (Pa (I).D_K, Pa (I).MNPC, Dea);
                     Dma (J) := Dm;
                     Odm := Dm;
                  else
                     K := K + 1;
                     -- K indexes within the MNPCA array -- Next MNPC
                     Sraa (J)(K) := (Pa (I).Stem, Pa (I).IR);
                  end if;
               end Handle_Parse_Record;
            begin
               case Pa (I).IR.Qual.Pofs  is
                  -- TODO: FACTOR OUT
                  --
                  -- the first four branches are completely regular;
                  -- work out how to reduce the remaining branches
                  when N =>
                     while Pa (I).IR.Qual.Pofs = N and I <= Pa_Last loop
                        Handle_Parse_Record;
                        I := I + 1;           --  I cycles over full PA array
                     end loop;

                  when Pron =>
                     while Pa (I).IR.Qual.Pofs = Pron and I <= Pa_Last loop
                        Handle_Parse_Record;
                        I := I + 1;           --  I cycles over full PA array
                     end loop;

                  when Pack =>
                     while Pa (I).IR.Qual.Pofs = Pack and I <= Pa_Last loop
                        Handle_Parse_Record;
                        I := I + 1;           --  I cycles over full PA array
                     end loop;

                  when Adj =>
                     while Pa (I).IR.Qual.Pofs = Adj and I <= Pa_Last loop
                        Handle_Parse_Record;
                        I := I + 1;         --  I cycles over full PA array
                     end loop;

                  when Num  =>
                     while Pa (I).IR.Qual.Pofs = Num   and
                       I <= Pa_Last                   loop
                        if Pa (I).D_K = Rrr then        --  Roman numeral
                           K := 1;
                           --  K indexes within the MNPCA array -- Initialize
                           J := J + 1;
                           --  J indexes the number of MNPCA arrays
                           Sraa (J)(K) := (Pa (I).Stem, Pa (I).IR);

                           Dea := Null_Dictionary_Entry;
                           Dm := (Pa (I).D_K, Pa (I).MNPC, Dea);
                           Dma (J) := Dm;
                           Odm := Dm;
                        else
                           Handle_Parse_Record;
                        end if;

                        I := I + 1;           --  I cycles over full PA array
                     end loop;

                  when V | Vpar | Supine  =>
                     while (Pa (I).IR.Qual.Pofs = V      or
                       Pa (I).IR.Qual.Pofs = Vpar   or
                       Pa (I).IR.Qual.Pofs = Supine)   and
                       I <= Pa_Last                   loop
                        if (Pa (I).MNPC  /= Odm.MNPC) and
                          (Pa (I).D_K /= Ppp)
                        then   --  Encountering new MNPC
                           K := 1;
                           --  K indexes within the MNPCA array -- Initialize

                           J := J + 1;
                           --  J indexes the number of MNPCA arrays
                           Sraa (J)(K) := (Pa (I).Stem, Pa (I).IR);
                           if Pa (I).D_K /= Ppp  then
                              Dict_IO.Set_Index
                                (Dict_File (Pa (I).D_K), Pa (I).MNPC);
                              Dict_IO.Read (Dict_File (Pa (I).D_K), Dea);
                           end if;     --  use previous DEA
                           Dm := (Pa (I).D_K, Pa (I).MNPC, Dea);
                           Dma (J) := Dm;
                           Odm := Dm;
                        else
                           K := K + 1;
                           --  K indexes within the MNPCA array  - Next MNPC
                           Sraa (J)(K) := (Pa (I).Stem, Pa (I).IR);
                        end if;

                        I := I + 1;           --  I cycles over full PA array
                     end loop;

                  when others  =>
                     --TEXT_IO.PUT_LINE ("Others");
                     while I <= Pa_Last                   loop
                        if (Odm.D_K  /= Pa (I).D_K)  or
                          (Odm.MNPC /= Pa (I).MNPC)
                        then   --  Encountering new single (K only 1)
                           K := 1;
                           --  K indexes within the MNPCA array -- Initialize

                           J := J + 1;
                           --  J indexes the number of MNPCA array

                           Sraa (J)(K) := (Pa (I).Stem, Pa (I).IR);
                           if Pa (I).MNPC /= Null_MNPC  then
                              if Pa (I).D_K = Addons  then
                                 Dea :=  Null_Dictionary_Entry;
                                 --  Fix for ADDONS in MEANS, not DICT_IO
                              else
                                 Dict_IO.Set_Index (Dict_File (Pa (I).D_K),
                                   Pa (I).MNPC);
                                 Dict_IO.Read (Dict_File (Pa (I).D_K), Dea);
                              end if;
                           else                 --  Has no dictionary to read
                              Dea := Null_Dictionary_Entry;
                           end if;
                           Dm := (Pa (I).D_K, Pa (I).MNPC, Dea);
                           Dma (J) := Dm;
                           Odm := Dm;
                           --else
                           --  K := K + 1;
                           --  K indexes within the MNPCA array  - Next MNPC
                           --  SRAA (J)(K) := (PA (I).STEM, PA (I).IR);
                        end if;

                        I := I + 1;           --  I cycles over full PA array
                        exit;
                        --  Since Other is only one, don't loop
                     end loop;
               end case;
            end;
         end if;

      end loop;

      if I = Pa_Last then
         I_Is_Pa_Last := True;
      else
         I_Is_Pa_Last := False;
      end if;

   exception
      when others  =>
         Ada.Text_IO.Put_Line
           ("Unexpected exception in CYCLE_OVER_PA processing " & Raw_Word);
         Put_Stat ("EXCEPTION LS at "
           & Head (Integer'Image (Line_Number), 8) &
           Head (Integer'Image (Word_Number), 4)
           & "   " & Head (W, 20) & "   "  & Pa (I).Stem);
   end Cycle_Over_Pa;

   procedure Put_Inflection
     (Configuration : Configuration_Type;
      Output        : Ada.Text_IO.File_Type;
      Sr            : Stem_Inflection_Record;
      Dm            : Dictionary_MNPC_Record)
   is
      --  Handles Putting ONLY_MEAN, PEARSE_CODES, CAPS, QUAL, V_KIND, FLAGS
      procedure Put_Inflection_Flags is
      begin
         if (Words_Mode (Show_Age)   or
           (Sr.Ir.Age /= X))  and     --  Warn even if not to show AGE
           Trim (Inflection_Age (Sr.Ir.Age))'Length /= 0
         then
            Ada.Text_IO.Put (Output, "  " & Inflection_Age (Sr.Ir.Age));
         end if;
         if (Words_Mode (Show_Frequency)  or
           (Sr.Ir.Freq >= C))  and    --  Warn regardless
           Trim (Inflection_Frequency (Sr.Ir.Freq))'Length /= 0
         then
            Ada.Text_IO.Put (Output, "  " &
              Inflection_Frequency (Sr.Ir.Freq));
         end if;
      end Put_Inflection_Flags;

   begin
      --TEXT_IO.PUT_LINE ("PUT_INFLECTION ");
      if not Words_Mode (Do_Only_Meanings) and
        not (Configuration = Only_Meanings)
      then
         Ada.Text_IO.Set_Col (Output, 1);

         if Dm.D_K = Addons then
            Put_Pearse_Code (Output, "05 ");
         elsif Dm.D_K in Xxx .. Yyy then
            Put_Pearse_Code (Output, "06 ");
         else
            Put_Pearse_Code (Output, "01 ");
         end if;

         --TEXT_IO.PUT (OUTPUT, CAP_STEM (TRIM (SR.STEM)));
         Ada.Text_IO.Put (Output, (Trim (Sr.Stem)));
         if Sr.Ir.Ending.Size > 0  then
            Ada.Text_IO.Put (Output, ".");
            --TEXT_IO.PUT (OUTPUT, TRIM (CAP_ENDING (SR.IR.ENDING.SUF)));
            Ada.Text_IO.Put (Output, Trim ((Sr.Ir.Ending.Suf)));
         end if;

         if Words_Mdev (Do_Pearse_Codes) then
            Ada.Text_IO.Set_Col (Output, 25);
         else
            Ada.Text_IO.Set_Col (Output, 22);
         end if;

         if Sr.Ir /= Null_Inflection_Record  then

            Print_Modified_Qual :
            declare
               Out_String : String (1 .. Quality_Record_IO.Default_Width);
               Passive_Start  : constant Integer :=
                 Part_Of_Speech_Type_IO.Default_Width + 1 +
                 Decn_Record_IO.Default_Width + 1 +
                 Tense_Type_IO.Default_Width + 1;
               Passive_Finish : constant Integer :=
                 Passive_Start +
                 Voice_Type_IO.Default_Width;
               Ppl_Start      : constant Integer :=
                 Part_Of_Speech_Type_IO.Default_Width + 1 +
                 Decn_Record_IO.Default_Width + 1 +
                 Case_Type_IO.Default_Width + 1 +
                 Number_Type_IO.Default_Width + 1 +
                 Gender_Type_IO.Default_Width + 1 +
                 Tense_Type_IO.Default_Width + 1;
               Ppl_Finish : constant Integer :=
                 Ppl_Start +
                 Voice_Type_IO.Default_Width;
               Passive_Blank :
                 constant String (1 .. Voice_Type_IO.Default_Width) :=
                 (others => ' ');
            begin

               Quality_Record_IO.Put (Out_String, Sr.Ir.Qual);
               if Dm.D_K in General .. Local then  --  UNIQUES has no DE

                  if (Sr.Ir.Qual.Pofs = V)    and then
                    (Dm.De.Part.V.Kind = Dep)       and then
                    (Sr.Ir.Qual.Verb.Tense_Voice_Mood.Mood in Ind .. Inf)
                  then
                     --TEXT_IO.PUT_LINE ("START PRINT MODIFIED QUAL   V");
                     Out_String (Passive_Start + 1 .. Passive_Finish) :=
                       Passive_Blank;
                  elsif (Sr.Ir.Qual.Pofs = Vpar)    and then
                    (Dm.De.Part.V.Kind = Dep)    and then
                    (Sr.Ir.Qual.Vpar.Tense_Voice_Mood.Mood = Ppl)
                  then
                     --TEXT_IO.PUT_LINE ("START PRINT MODIFIED QUAL   VPAR");
                     Out_String (Ppl_Start + 1 .. Ppl_Finish) :=
                       Passive_Blank;
                  end if;
               end if;

               Ada.Text_IO.Put (Output, Out_String);
               --TEXT_IO.PUT_LINE ("PRINT MODIFIED QUAL 4");
            end Print_Modified_Qual;

            --               if ((SR.IR.QUAL.POFS = NUM)  and
            --                          -- Don't want on inflection
            --                   (DM.D_K in GENERAL .. UNIQUE))  and then
            --                   (DM.DE.KIND.NUM_VALUE > 0)  then
            --                 TEXT_IO.PUT (OUTPUT, "  ");
            --                 INFLECTIONS_PACKAGE.INTEGER_IO.PUT
            --                    (OUTPUT, DM.DE.KIND.NUM_VALUE);
            --               end if;
            Put_Inflection_Flags;
            Ada.Text_IO.New_Line (Output);
            Put_Example_Line (Configuration, Output, Sr.Ir, Dm.De);
            --  Only full when DO_EXAMPLES
         else
            Ada.Text_IO.New_Line (Output);
         end if;
      end if;
   end Put_Inflection;

   --  Handles PEARSE_CODES and DICTIONARY_FORM (which has FLAGS) and D_K
   --  The Pearse 02 is handled in PUT_DICTIONARY_FORM
   procedure Put_Form
     (Output : Ada.Text_IO.File_Type;
      Sr     : Stem_Inflection_Record;
      Dm     : Dictionary_MNPC_Record)
   is
   begin
      if (Sr.Ir.Qual.Pofs not in Xons)  and
        (Dm.D_K in General .. Unique)
      then
         --DICTIONARY_ENTRY_IO.PUT (DM.DE);
         Put_Dictionary_Form (Output, Dm.D_K, Dm.MNPC, Dm.De);
      end if;
   end Put_Form;

   procedure Put_Parse_Details
     (Configuration : Configuration_Type;
      Output        : Ada.Text_IO.File_Type;
      Dma           : Dictionary_MNPC_Array;
      Sraa          : Stem_Inflection_Array_Array;
      I_Is_Pa_Last  : Boolean)
   is
      Mm            : constant Integer := Get_Max_Meaning_Size (Output);
   begin
      --TEXT_IO.PUT_LINE ("PUTting INFLECTIONS");
      declare
         J : Integer := 1;
         Osra : Stem_Inflection_Array (1 .. Stem_Inflection_Array_Size)
           := Null_Sra;
      begin

         Output_Loop :
         while  Dma (J) /= Null_Dictionary_MNPC_Record  loop
            --  Skips one identical SRA no matter what comes next
            if Sraa (J) /= Osra  then

               Put_Inflection_Array_J :
               for K in Sraa (J)'Range loop
                  exit Put_Inflection_Array_J when Sraa (J)(K) =
                    Null_Stem_Inflection_Record;

                  Put_Inflection (Configuration, Output, Sraa (J)(K), Dma (J));
                  if Sraa (J)(K).Stem (1 .. 3) = "PPL"  then
                     Ada.Text_IO.Put_Line (Output, Head (Ppp_Meaning, Mm));
                  end if;
               end loop Put_Inflection_Array_J;
               Osra := Sraa (J);
            end if;

            Putting_Form :
            begin
               if J = 1  or else
                 Support_Utils.Dictionary_Form (Dma (J).De) /=
                 Support_Utils.Dictionary_Form (Dma (J - 1).De)
               then
                  --  Put at first chance, skip duplicates
                  Put_Form (Output, Sraa (J)(1), Dma (J));
               end if;
            end Putting_Form;

            Putting_Meaning :
            begin
               if Dma (J).D_K in General .. Unique then
                  if Dma (J).De.Mean /= Dma (J + 1).De.Mean then
                     --  Hhandle simple multiple MEAN with same IR and FORM
                     --  by anticipating duplicates and waiting until change
                     Put_Meaning_Line (Output, Sraa (J)(1), Dma (J), Mm);
                  end if;
               else
                  Put_Meaning_Line (Output, Sraa (J)(1), Dma (J), Mm);
               end if;
            end Putting_Meaning;

            Do_Pause :
            begin
               if I_Is_Pa_Last  then
                  Ada.Text_IO.New_Line (Output);
               elsif Integer (Ada.Text_IO.Line (Output)) >
                 Scroll_Line_Number + Output_Screen_Size
               then
                  Pause (Output);
                  Scroll_Line_Number := Integer (Ada.Text_IO.Line (Output));
               end if;
            end Do_Pause;

            J := J + 1;
         end loop Output_Loop;
         --TEXT_IO.PUT_LINE ("Finished OUTPUT_LOOP");
      end;
   end Put_Parse_Details;

   procedure List_Stems
     (Configuration : Configuration_Type;
      Output        : Ada.Text_IO.File_Type;
      Raw_Word      : String;
      Input_Line    : String;
      Pa            : in out Parse_Array;
      Pa_Last       : in out Integer)
   is
      use Ada.Text_IO;
      use Dict_IO;

      --  The main WORD processing has been to produce an array of PARSE_RECORD
      --      type PARSE_RECORD is
      --        record
      --          STEM  : STEM_TYPE := NULL_STEM_TYPE;
      --          IR    : INFLECTION_RECORD := NULL_INFLECTION_RECORD;
      --          D_K   : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
      --          MNPC  : DICT_IO.COUNT := NULL_MNPC;
      --        end record;
      --  This has involved STEMFILE and INFLECTS, no DICTFILE

      --  PARSE_RECORD is Put through the LIST_SWEEP procedure that does TRIMing
      --  Then, for processing for Output, the data is converted to arrays of
      --      type STEM_INFLECTION_RECORD is
      --        record
      --          STEM : STEM_TYPE          := NULL_STEM_TYPE;
      --          IR   : INFLECTION_RECORD  := NULL_INFLECTION_RECORD;
      --        end record;
      --  and
      --      type DICTIONARY_MNPC_RECORD is
      --        record
      --          D_K  : DICTIONARY_KIND;
      --          MNPC : MNPC_TYPE;
      --          DE   : DICTIONARY_ENTRY;
      --        end record;
      --  containing the same data plus the DICTFILE data DICTIONARY_ENTRY
      --  but breaking it into two arrays allows different manipulation
      --  These are only within this routine, used to clean up the Output

      Null_Sraa : constant Stem_Inflection_Array_Array
        (1 .. Stem_Inflection_Array_Array_Size)
        := (others => Null_Sra);

      Sraa : Stem_Inflection_Array_Array
        (1 .. Stem_Inflection_Array_Array_Size) := Null_Sraa;

      Null_Dma : constant Dictionary_MNPC_Array :=
        (others => Null_Dictionary_MNPC_Record);
      Dma : Dictionary_MNPC_Array := Null_Dma;

      --MEANING_ARRAY_SIZE : constant := 5;
      --MEANING_ARRAY : array (1 .. MEANING_ARRAY_SIZE) of MEANING_TYPE;

      W : constant String := Raw_Word;
      J1, J2 : Integer := 0;
      There_Is_An_Adverb : Boolean := False;

      I_Is_Pa_Last : Boolean := False;

   begin
      Trimmed := False;

      --  Since this procedure weeds out possible parses, if it weeds out all
      --  (or all of a class) it must fix up the rest of the parse array,
      --  e.g., it must clean out dangling prefixes and suffixes

      -------  The gimick of adding an ADV if there is only ADJ VOC  ----
      --TEXT_IO.PUT_LINE ("About to do the ADJ -> ADV kludge");
      for I in Pa'First .. Pa_Last  loop
         if Pa (I).IR.Qual.Pofs = Adv   then
            There_Is_An_Adverb := True;
            exit;
         end if;
      end loop;

      declare
         J : Integer := 0;
      begin
         if (not There_Is_An_Adverb) and (Words_Mode (Do_Fixes))  then
            --TEXT_IO.PUT_LINE ("In the ADJ -> ADV kludge  There is no ADV");
            for I in reverse Pa'First .. Pa_Last  loop
               if Pa (I).IR.Qual.Pofs = Adj and then
                 (Pa (I).IR.Qual.Adj = ((1, 1), Voc, S, M, Pos)    or
                 ((Pa (I).IR.Qual.Adj.Of_Case = Voc)   and
                 (Pa (I).IR.Qual.Adj.Number = S)   and
                 (Pa (I).IR.Qual.Adj.Gender = M)   and
                 (Pa (I).IR.Qual.Adj.Comparison = Super)))
               then
                  J := I;

                  while J >=  Pa'First  loop  --Back through other ADJ cases
                     if Pa (J).IR.Qual.Pofs /= Adj  then
                        J2 := J;
                        --  J2 is first (reverse) that is not ADJ
                        exit;
                     end if;
                     J := J - 1;
                  end loop;
                  while J >=  Pa'First  loop  --  Sweep up associated fixes
                     if Pa (J).IR.Qual.Pofs not in Xons  then
                        J1 := J;
                        --  J1 is first (reverse) that is not XONS
                        exit;
                     end if;
                     J := J - 1;
                  end loop;

                  for J in J1 + 1 .. J2  loop
                     Pa (Pa_Last + J - J1 + 1) := Pa (J);
                  end loop;

                  Pa_Last := Pa_Last + J2 - J1 + 1;
                  Pa (Pa_Last) := Pa (J2 + 1);

                  Pa (Pa_Last) := ("e                 ",
                    ((Suffix, Null_Suffix_Record), 0, Null_Ending_Record, X, B),
                    Ppp, Null_MNPC);
                  --PARSE_RECORD_IO.PUT (PA (PA_LAST)); TEXT_IO.NEW_LINE;
                  Pa_Last := Pa_Last + 1;

                  declare
                     procedure Handle_Degree
                       (E       : Ending_Record;
                        Caption : String) is
                     begin
                        Pa (Pa_Last) := (Pa (J2 + 1).Stem,
                          ((Pofs => Adv,
                          Adv => (Comparison =>
                          Pa (J2 + 1).IR.Qual.Adj.Comparison)),
                          Key => 0, Ending => E, Age => X, Freq => B),
                          Pa (J2 + 1).D_K,
                          Pa (J2 + 1).MNPC);

                        Ppp_Meaning := Head (Caption, Max_Meaning_Size);
                     end Handle_Degree;
                  begin
                     if Pa (J2 + 1).IR.Qual.Adj.Comparison = Pos then
                        Handle_Degree ((1, "e      "),
                          "-ly; -ily;  Converting ADJ to ADV");
                     elsif Pa (J2 + 1).IR.Qual.Adj.Comparison = Super then
                        Handle_Degree ((2, "me     "),
                          "-estly; -estily; most -ly, very -ly" &
                          "  Converting ADJ to ADV");
                     end if;
                  end;
               end if;           --  PA (I).IR.QUAL.POFS = ADJ
            end loop;
         end if;           --  not THERE_IS_AN_ADVERB
      end;

      List_Sweep (Pa (1 .. Pa_Last), Pa_Last);

      if  Words_Mdev (Write_Statistics_File)    then
         --  Omit rest of Output
         for I in 1 .. Pa_Last  loop                       --  Just to PUT_STAT
            if Pa (I).D_K = Addons then
               declare
                  procedure Put_Addon_Info (Caption : String) is
                  begin
                     Put_Stat ("ADDON " & Caption & " at "
                       & Head (Integer'Image (Line_Number), 8) &
                       Head (Integer'Image (Word_Number), 4)
                       & "   " & Head (W, 20) & "   "  & Pa (I).Stem &
                       "  " & Integer'Image (Integer (Pa (I).MNPC)));
                  end Put_Addon_Info;
               begin
                  case Pa (I).IR.Qual.Pofs is
                     when Prefix => Put_Addon_Info ("PREFIX");
                     when Suffix => Put_Addon_Info ("SUFFIX");
                     when Tackon => Put_Addon_Info ("TACKON");
                     when others => null;
                  end case;
               end;
            end if;
         end loop;
      end if;

      Cycle_Over_Pa (Pa, Pa_Last, Sraa, Dma, I_Is_Pa_Last,
                    Raw_Word, W);

      --  Sets + if capitalized
      --  Strangely enough, it may enter LIST_STEMS with PA_LAST /= 0
      --  but be weeded and end up with no parse after
      --                    LIST_SWEEP  -  PA_LAST = 0
      if Pa_Last = 0  then
         --  WORD failed
         declare
            procedure Do_Ignore_Unknown (Caption : String) is
            begin
               Nnn_Meaning := Head (
                 "Assume this is capitalized proper name/abbr," &
                 " under MODE " & Caption & " ",
                 Max_Meaning_Size);
               Pa (1) := (Head (Raw_Word, Max_Stem_Size),
                 ((N, ((0, 0), X, X, X)), 0, Null_Ending_Record, X, X),
                 Nnn, Null_MNPC);
               Pa_Last := 1;    --  So LIST_NEIGHBORHOOD will not be called
               Sraa := Null_Sraa;
               Dma := Null_Dma;
               Sraa (1)(1) := (Pa (1).Stem, Pa (1).IR);
               Dma (1) := (Nnn, 0, Null_Dictionary_Entry);
            end Do_Ignore_Unknown;
         begin
            if Words_Mode (Ignore_Unknown_Names) and Capitalized then
               Do_Ignore_Unknown ("IGNORE_UNKNOWN_NAME");
            elsif Words_Mode (Ignore_Unknown_Caps) and All_Caps then
               Do_Ignore_Unknown ("IGNORE_UNKNOWN_CAPS");
            end if;
         end;
      end if;

      if Pa_Last = 0   then
         if  Words_Mode (Write_Output_To_File)      then
            Put_Pearse_Code (Output, "04 ");
            Ada.Text_IO.Put (Output, Raw_Word);
            Ada.Text_IO.Set_Col (Output, 30);
            Inflections_Package.Integer_IO.Put (Output, Line_Number, 7);
            Inflections_Package.Integer_IO.Put (Output, Word_Number, 7);
            Ada.Text_IO.Put_Line (Output, "    ========   UNKNOWN    ");
         else              --  Just screen Output
            if Words_Mdev (Do_Pearse_Codes) then
               Ada.Text_IO.Put ("04 ");
            end if;
            Ada.Text_IO.Put (Raw_Word);
            Ada.Text_IO.Set_Col (30);
            Ada.Text_IO.Put_Line ("    ========   UNKNOWN    ");
         end if;

         if Words_Mode (Write_Unknowns_To_File)  then
            if Words_Mdev (Include_Unknown_Context) or
              Words_Mdev (Do_Only_Initial_Word)
            then
               Ada.Text_IO.Put_Line (Input_Line);
               Ada.Text_IO.Put_Line (Unknowns, Input_Line);
            end if;
            Put_Pearse_Code (Unknowns, "04 ");
            Ada.Text_IO.Put (Unknowns, Raw_Word);
            Ada.Text_IO.Set_Col (Unknowns, 30);
            Inflections_Package.Integer_IO.Put (Unknowns, Line_Number, 7);
            Inflections_Package.Integer_IO.Put (Unknowns, Word_Number, 7);
            Ada.Text_IO.Put_Line (Unknowns, "    ========   UNKNOWN    ");
         end if;
      end if;

      if Pa_Last = 0   then
         if Words_Mode (Do_Stems_For_Unknown)   then
            if  Words_Mode (Write_Output_To_File)  and then
              not Words_Mode (Write_Unknowns_To_File)
            then
               List_Neighborhood (Output, Raw_Word);
            elsif  Words_Mode (Write_Output_To_File)  and then
              Words_Mode (Write_Unknowns_To_File)
            then
               List_Neighborhood (Output, Raw_Word);
               List_Neighborhood (Unknowns, Raw_Word);
            elsif Name (Current_Input) = Name (Standard_Input) then
               List_Neighborhood (Output, Raw_Word);
            end if;
         end if;
      end if;

      if Pa_Last = 0 then
         if Words_Mdev (Update_Local_Dictionary)  and
           -- Don't if reading from file
           (Name (Current_Input) = Name (Standard_Input))
         then
            Update_Local_Dictionary_File;
            Word (Raw_Word, Pa, Pa_Last);
            --  Circular if you dont update!!!!!
         end if;
      end if;

      --  Exit if UNKNOWNS ONLY (but had to do STATS above)
      if  Words_Mode (Do_Unknowns_Only)    then      --  Omit rest of Output
         return;
      end if;

      Put_Parse_Details (Configuration, Output, Dma, Sraa, I_Is_Pa_Last);

      if Trimmed then
         Put (Output, '*');
      end if;
      Ada.Text_IO.New_Line (Output);

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("Unexpected exception in LIST_STEMS processing " & Raw_Word);
         Put_Stat ("EXCEPTION LS at "
           & Head (Integer'Image (Line_Number), 8) &
           Head (Integer'Image (Word_Number), 4)
           & "   " & Head (W, 20));
   end List_Stems;

   procedure List_Entry (Output   : Ada.Text_IO.File_Type;
                         D_K      : Dictionary_Kind;
                         Mn       : Dict_IO.Count;
                         Mm       : Integer) is
      De : Dictionary_Entry;
   begin
      Dict_IO.Read (Dict_File (D_K), De, Mn);
      Ada.Text_IO.Put (Output, "=>  ");
      --TEXT_IO.PUT_LINE (OUTPUT, DICTIONARY_FORM (DE));
      Put_Dictionary_Form (Output, D_K, Mn, De);
      Ada.Text_IO.Put_Line (Output,
        Trim (Head (De.Mean, Mm)));  --  so it wont line wrap/Put CR

   end List_Entry;

   procedure Unknown_Search (Unknown       :  in String;
                             Unknown_Count : out Dict_IO.Count) is

      use Stem_Io;

      D_K : constant Dictionary_Kind := General;
      J, J1, J2, Jj : Stem_Io.Count := 0;

      Index_On : constant String := Unknown;
      Index_First, Index_Last : Stem_Io.Count := 0;
      Ds : Dictionary_Stem;
      First_Try, Second_Try : Boolean := True;

      function First_Two (W : String) return String is
         --  'v' could be represented by 'u'
         --  like the new Oxford Latin Dictionary
         --  Fixes the first two letters of a word/stem which can be done right
         S : constant String := Lower_Case (W);
         Ss : String (W'Range) := W;

      begin
         if S'Length = 1  then
            Ss (S'First) :=
              Support_Utils.Char_Utils.V_To_U_And_J_To_I (W (S'First));
         else
            Ss (S'First) :=
              Support_Utils.Char_Utils.V_To_U_And_J_To_I (W (S'First));
            Ss (S'First + 1) :=
              Support_Utils.Char_Utils.V_To_U_And_J_To_I (W (S'First + 1));
         end if;
         return Ss;
      end First_Two;

   begin

      if Dictionary_Available (D_K)  then
         if not Is_Open (Stem_File (D_K))  then
            Open (Stem_File (D_K), Stem_Io.In_File,
              Add_File_Name_Extension (Stem_File_Name,
              Dictionary_Kind'Image (D_K)));
         end if;

         Index_First := First_Index (First_Two (Index_On), D_K);
         Index_Last  := Last_Index (First_Two (Index_On), D_K);

         if Index_First > 0  and then Index_First <= Index_Last then

            J1 := Index_First;    --######################
            J2 := Index_Last;

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

               if  Ltu (Lower_Case (Ds.Stem), Unknown)  then
                  J1 := J;
                  J := (J1 + J2) / 2;
               elsif  Gtu (Lower_Case (Ds.Stem), Unknown)  then
                  J2 := J;
                  J := (J1 + J2) / 2;
               else
                  for I in reverse J1 .. J  loop
                     Set_Index (Stem_File (D_K), Stem_Io.Count (I));
                     Read (Stem_File (D_K), Ds);

                     if Equ (Lower_Case (Ds.Stem), Unknown)  then
                        Jj := I;

                     else
                        exit;
                     end if;
                  end loop;

                  for I in J + 1 .. J2  loop
                     Set_Index (Stem_File (D_K), Stem_Io.Count (I));
                     Read (Stem_File (D_K), Ds);

                     if Equ (Lower_Case (Ds.Stem), Unknown)  then
                        Jj := I;

                     else
                        exit Binary_Search;
                     end if;
                  end loop;
                  exit Binary_Search;

               end if;
            end loop Binary_Search;
            J1 := Jj;
            J2 := Index_Last;

         end if;
         Unknown_Count := Ds.MNPC;

         Close (Stem_File (D_K));  --??????
      end if;
      --TEXT_IO.PUT_LINE ("Leaving LIST_NEIGHBORHOOD    UNKNOWN_SEARCH");
   end Unknown_Search;

   procedure List_Neighborhood (Output : Ada.Text_IO.File_Type;
                                Input_Word : String) is
      D_K : constant Dictionary_Kind := General;
      Unk_MNPC : Dict_IO.Count;
      Mm : constant Integer := Get_Max_Meaning_Size (Output);
   begin
      Unknown_Search (Head (Input_Word, Max_Stem_Size), Unk_MNPC);
      --TEXT_IO.PUT_LINE ("UNK_MNPC = " & INTEGER'IMAGE (INTEGER (UNK_MNPC)));
      if Integer (Unk_MNPC) > 0  then
         Ada.Text_IO.Put_Line (Output,
           "----------  " &
           "Entries in GENEAL Dictionary around the UNKNOWN" &
           "  ----------");
         Pause (Output);
         for Mn in Dict_IO.Count (Integer (Unk_MNPC) - 5) ..
           Dict_IO.Count (Integer (Unk_MNPC) + 3)  loop
            List_Entry (Output, D_K, Mn, Mm);

         end loop;
      end if;

      --TEXT_IO.PUT_LINE ("Leaving LIST_NEIGHBORHOOD");

   end List_Neighborhood;

end List_Package;
