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

with Ada.Text_IO;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
with Latin_Utils.Config; use Latin_Utils.Config;
with Support_Utils.Word_Parameters; use Support_Utils.Word_Parameters;
--with LATIN_DEBUG;
procedure Put_example_line
  (configuration : configuration_type;
   Output        : Ada.Text_IO.File_Type;
   ir            : in Inflection_Record;
   de            : in Dictionary_Entry)
is
   --      use LATIN_DEBUG;
   vk : Verb_Kind_Type;

   procedure Put_verb_example
     (Output : Ada.Text_IO.File_Type;
      ir     : in Inflection_Record;
      vk      : in Verb_Kind_Type)
   is
      person : constant Person_Type      := ir.qual.V.Person;
      number : constant Number_Type      := ir.qual.V.Number;
      tense  : constant Tense_Type       := ir.qual.V.Tense_Voice_Mood.Tense;
      mood   : constant Mood_Type        := ir.qual.V.Tense_Voice_Mood.Mood;
      voice  : Voice_Type                := ir.qual.V.Tense_Voice_Mood.Voice;
      kind   : constant Verb_Kind_Type   := vk;
      --  Nothing on  (part), gerund,

      function they return String is
      begin
         if kind = Impers  then
            return "it ";
         end if;

         if mood = Inf then
            return "to ";
         end if;

         if mood = Imp and tense = Pres  and number = P  then
            return "(you) ";
         end if;

         if mood = Sub and tense = Pres  and
           person = 1 and number = P
         then
            return "let us ";   --  G&L 263 1
         end if;

         if  number = S  then
            if person = 1  then
               return "I ";
            elsif  person = 2  then
               return "you ";
            elsif  person = 3  then
               return "he/it ";
            else
               return "";
            end if;
         elsif number = P  then
            if person = 1  then
               return "we ";
            elsif  person = 2  then
               return "you ";
            elsif  person = 3  then
               return "they ";
            else
               return "";
            end if;
         else
            return "";
         end if;
      end they;

      function shall return String is
      begin            --  ACTIVE only  !!!!!!!!!!!!!!!!
         if tense = Fut or tense = Futp then
            if (mood = Ind) or (mood = Sub)  then
               if person = 1  then
                  return "shall ";
               elsif  person = 2  then
                  return "will ";
               elsif  person = 3  then
                  return "will ";
               else
                  return "";
               end if;
            elsif mood = Imp  then
               if person = 1  then
                  return "will ";
               elsif  person = 2  then
                  return "(shall) ";
               elsif  person = 3  then
                  return "(shall) ";
               else
                  return "";
               end if;
            elsif mood = Inf  then
               if tense = Fut  then
                  return "be about to be ";
               else
                  return "";
               end if;
            else
               return "";
            end if;
         else
            return "";
         end if;
      end shall;

      function have return String is
      begin
         if tense in Pres .. Fut  then
            return "";
         elsif tense = Perf  then
            if (tense = Perf) and (person = 3) and (number = S)  then
               return "has ";
            else
               return "have ";    -- works for INF too
            end if;
         elsif tense = Plup  then
            if mood = Ind  then
               return "had";
            elsif mood = Sub  then
               return "have ";
            else
               return "";
            end if;
         elsif tense = Futp   then
            return "have ";
         else
            return "";
         end if;
      end have;

      function been return String is
      begin
         if voice = Passive  then
            if mood = Ind  then
               if tense = Pres  then
                  if (person = 1) and (number = S)  then
                     return "am/am being ";
                  elsif (person = 3) and (number = S)  then
                     return "is/is being ";
                  else
                     return "are/are being ";
                  end if;
               elsif tense = Impf   then
                  if (person = 1 or person = 3) and (number = S)  then
                     return "was/was being ";
                  else
                     return "were/were being ";
                  end if;
               elsif tense = Fut   then
                  return "be ";
               elsif tense = Perf   then
                  if (person = 1 or person = 3) and (number = S)  then
                     return "been/was ";
                  else
                     return "been/were ";
                  end if;
               elsif tense in Plup .. Futp   then
                  return "been ";
               else
                  return "";
               end if;
            elsif mood = Sub  then
               return "";              --????????
            elsif mood = Inf  then
               if tense = Pres  then
                  return "be ";
               elsif tense = Perf  then
                  return "been ";
               else
                  return "";
               end if;
            elsif mood = Imp  then
               return "be ";
            else
               return "";
            end if;
         else
            return "";
         end if;
      end been;

      function ed return String is
      begin
         if mood = Imp  then
            if voice = Active  then
               return "!";
            elsif voice = Passive  then
               return "ed!";
            else
               return "";
            end if;
         elsif mood = Inf  then
            if voice = Active  then
               return "";
            elsif voice = Passive  then
               return "ed";
            else
               return "";
            end if;
         elsif mood = Ind  then
            if voice = Active  then
               if tense = Pres  then
                  if (person = 3) and (number = S)  then
                     return "s";
                  else
                     return "";
                  end if;
               elsif tense = Impf   then
                  if (person = 1 or person = 3) and (number = S)  then
                     return "ed/was ~ing";
                  else
                     return "ed/were ~ing";
                  end if;
               elsif tense in Perf .. Futp   then
                  return "ed";
               else
                  return "";
               end if;
            elsif voice = Passive  then
               return "ed";
            else
               return "";
            end if;
         elsif mood = Sub  then
            if tense in Perf .. Plup  then
               return "ed";
            else
               return "";
            end if;
         else
            return "";
         end if;
      end ed;

      function sub return String is
      begin
         if mood = Sub  then
            return "may/must/should ";
         else
            return "";
         end if;
      end sub;

   begin   --  PUT_VERB_EXAMPLE
      if kind = Dep then
         voice := Active;    --  Should only have allowed PASSIVE at this point
      elsif kind = Semidep and then tense in Perf .. Futp   then
         voice := Active;    --  Should only have allowed PASSIVE at this point
      end if;

      Ada.Text_IO.Put (Output, they & sub & shall & have & been & "~" & ed);

   end Put_verb_example;

begin    --  PUT_EXAMPLE_LINE

   if words_mode (do_examples)
     and then (not (configuration = only_meanings))
   then

      case ir.qual.pofs is
         when N =>
            case ir.qual.N.Of_Case is
               when Gen =>
                  Ada.Text_IO.Put (Output, "~'s; of ~");
                  Ada.Text_IO.New_Line (Output);
               when Abl =>
                  Ada.Text_IO.New_Line (Output);
                  --  Info too much for same line
                  Ada.Text_IO.Set_Col (Output, 6);
                  Ada.Text_IO.Put (Output,
                    "from _ (separ); because of ~ (cause);"
                    & " than ~ (compar); of ~ (circumstance)");
                  Ada.Text_IO.New_Line (Output);
               when Dat =>
                  Ada.Text_IO.New_Line (Output);
                  --  Info too much for same line
                  Ada.Text_IO.Set_Col (Output, 6);
                  Ada.Text_IO.Put (Output,
                    "for _ (purpose, reference);"
                    & " to ~ (w/adjectives); to ~ (double dative)");
                  Ada.Text_IO.New_Line (Output);
               when Loc =>
                  Ada.Text_IO.Put (Output, "at ~ (place where)");
                  Ada.Text_IO.New_Line (Output);
               when others  =>
                  null;
                  --TEXT_IO.NEW_LINE (OUTPUT);
            end case;

         when Adj =>
            case ir.qual.Adj.Comparison is
               when Comp  =>
                  Ada.Text_IO.Put (Output, "~er; more/too _");
                  Ada.Text_IO.New_Line (Output);
               when Super =>
                  Ada.Text_IO.Put (Output, "~est; most/very");
                  Ada.Text_IO.New_Line (Output);
               when others  =>
                  null;
                  --TEXT_IO.NEW_LINE (OUTPUT);
            end case;

         when Adv =>
            case ir.qual.Adv.Comparison is
               when Comp  =>
                  Ada.Text_IO.Put (Output, "more/too ~(ly)");
                  Ada.Text_IO.New_Line (Output);
               when Super =>
                  Ada.Text_IO.Put (Output, "most/very ~(ly)");
                  Ada.Text_IO.New_Line (Output);
               when others  =>
                  null;
                  --TEXT_IO.NEW_LINE (OUTPUT);
            end case;

         when V =>
            --TEXT_IO.NEW_LINE (OUTPUT);
            --  Verb info too much for same line
            vk := de.Part.V.Kind;
            Ada.Text_IO.Set_Col (Output, 6);
            Put_verb_example (Output, ir, vk);
            Ada.Text_IO.New_Line (Output);

         when Vpar =>
            --    TEXT_IO.NEW_LINE (OUTPUT);
            --  Verb info too much for same line
            case ir.qual.Vpar.Tense_Voice_Mood.Tense is
               when Perf  =>
                  Ada.Text_IO.Put (Output,
                    "~ed  PERF PASSIVE PPL often used as ADJ"
                    & " or N (amatus => belov.ed)");
                  Ada.Text_IO.New_Line (Output);
               when Pres  =>
                  Ada.Text_IO.Put (Output,
                    "~ing  PRES ACTIVE PPL often used as ADJ"
                    & " or N (lov.ing, curl.y)");
                  Ada.Text_IO.New_Line (Output);
               when Fut   =>
                  if ir.qual.Vpar.Tense_Voice_Mood.Voice = Active  then
                     Ada.Text_IO.Put (Output,
                       "about/going/intending/destined to ~"
                       & "  FUT ACTIVE PPL often used as ADJ or N ");
                     Ada.Text_IO.New_Line (Output);
                  else
                     case ir.qual.Vpar.Of_Case is
                        when Gen =>
                           Ada.Text_IO.Put (Output,
                             "to (/must) be ~ed  FUT PASSIVE PPL,"
                             & " often used as gerund or gerundive (of ~ing)");
                        when Dat =>
                           Ada.Text_IO.Put (Output,
                             "to (/must) be ~ed  FUT PASSIVE PPL,"
                             & " often used as gerund or gerundive "
                             & "(to/for ~ing)");
                        when Abl =>
                           Ada.Text_IO.Put (Output,
                             "to (/must) be ~ed  FUT PASSIVE PPL,"
                             & " often used as gerund or gerundive "
                             & "(by/in ~ing)");
                        when Acc =>
                           Ada.Text_IO.Put (Output,
                             "to (/must) be ~ed  FUT PASSIVE PPL,"
                             & " often used as gerund or gerundive "
                             & "(for ~ing/to ~)");
                        when others =>
                           Ada.Text_IO.Put (Output,
                             "to (/must) be ~ed  FUT PASSIVE PPL,"
                             & " often used as gerund or gerundive (~ing)");
                     end case;
                     Ada.Text_IO.New_Line (Output);
                  end if;
               when others  =>
                  null;
                  --TEXT_IO.NEW_LINE (OUTPUT);
            end case;      --  TENSE

         when Supine =>
            --TEXT_IO.NEW_LINE (OUTPUT);
            if ir.qual.Supine.Of_Case = Acc  then
               Ada.Text_IO.Put (Output,
                 "to ~  expresses purpose of verb of motion;"
                 & " may take a direct object");
               Ada.Text_IO.New_Line (Output);
            elsif ir.qual.Supine.Of_Case = Abl  then
               Ada.Text_IO.Put (Output,
                 "to ~  after ADJ indicating aspect/respect in"
                 & " which something is/is done");
               Ada.Text_IO.New_Line (Output);
            end if;

         when others  =>
            null;
            --TEXT_IO.NEW_LINE (OUTPUT);
      end case;        --  PART

   else
      null;
      --TEXT_IO.NEW_LINE (OUTPUT);
   end if;

end Put_example_line;
