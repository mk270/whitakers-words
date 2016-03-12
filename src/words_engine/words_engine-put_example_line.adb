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
procedure Words_Engine.Put_Example_Line
  (Configuration : Configuration_Type;
   Output        : Ada.Text_IO.File_Type;
   Ir            : in Inflection_Record;
   De            : in Dictionary_Entry)
is
   --      use LATIN_DEBUG;
   Vk : Verb_Kind_Type;

   procedure Put_Verb_Example
     (Output : Ada.Text_IO.File_Type;
      Ir     : in Inflection_Record;
      Vk      : in Verb_Kind_Type)
   is
      Person : constant Person_Type      := Ir.Qual.Verb.Person;
      Number : constant Number_Type      := Ir.Qual.Verb.Number;
      Tense  : constant Tense_Type       := Ir.Qual.Verb.Tense_Voice_Mood.Tense;
      Mood   : constant Mood_Type        := Ir.Qual.Verb.Tense_Voice_Mood.Mood;
      Voice  : Voice_Type                := Ir.Qual.Verb.Tense_Voice_Mood.Voice;
      Kind   : constant Verb_Kind_Type   := Vk;
      --  Nothing on  (part), gerund,

      function They return String is
      begin
         if Kind = Impers  then
            return "it ";
         end if;

         if Mood = Inf then
            return "to ";
         end if;

         if Mood = Imp and Tense = Pres  and Number = P  then
            return "(you) ";
         end if;

         if Mood = Sub and Tense = Pres  and
           Person = 1 and Number = P
         then
            return "let us ";   --  G&L 263 1
         end if;

         if  Number = S  then
            if Person = 1  then
               return "I ";
            elsif  Person = 2  then
               return "you ";
            elsif  Person = 3  then
               return "he/it ";
            else
               return "";
            end if;
         elsif Number = P  then
            if Person = 1  then
               return "we ";
            elsif  Person = 2  then
               return "you ";
            elsif  Person = 3  then
               return "they ";
            else
               return "";
            end if;
         else
            return "";
         end if;
      end They;

      function Shall return String is
      begin            --  ACTIVE only  !!!!!!!!!!!!!!!!
         if Tense = Fut or Tense = Futp then
            if (Mood = Ind) or (Mood = Sub)  then
               if Person = 1  then
                  return "shall ";
               elsif  Person = 2  then
                  return "will ";
               elsif  Person = 3  then
                  return "will ";
               else
                  return "";
               end if;
            elsif Mood = Imp  then
               if Person = 1  then
                  return "will ";
               elsif  Person = 2  then
                  return "(shall) ";
               elsif  Person = 3  then
                  return "(shall) ";
               else
                  return "";
               end if;
            elsif Mood = Inf  then
               if Tense = Fut  then
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
      end Shall;

      function Have return String is
      begin
         if Tense in Pres .. Fut  then
            return "";
         elsif Tense = Perf  then
            if (Tense = Perf) and (Person = 3) and (Number = S)  then
               return "has ";
            else
               return "have ";    -- works for INF too
            end if;
         elsif Tense = Plup  then
            if Mood = Ind  then
               return "had";
            elsif Mood = Sub  then
               return "have ";
            else
               return "";
            end if;
         elsif Tense = Futp   then
            return "have ";
         else
            return "";
         end if;
      end Have;

      function Been return String is
      begin
         if Voice = Passive  then
            if Mood = Ind  then
               if Tense = Pres  then
                  if (Person = 1) and (Number = S)  then
                     return "am/am being ";
                  elsif (Person = 3) and (Number = S)  then
                     return "is/is being ";
                  else
                     return "are/are being ";
                  end if;
               elsif Tense = Impf   then
                  if (Person = 1 or Person = 3) and (Number = S)  then
                     return "was/was being ";
                  else
                     return "were/were being ";
                  end if;
               elsif Tense = Fut   then
                  return "be ";
               elsif Tense = Perf   then
                  if (Person = 1 or Person = 3) and (Number = S)  then
                     return "been/was ";
                  else
                     return "been/were ";
                  end if;
               elsif Tense in Plup .. Futp   then
                  return "been ";
               else
                  return "";
               end if;
            elsif Mood = Sub  then
               return "";              --????????
            elsif Mood = Inf  then
               if Tense = Pres  then
                  return "be ";
               elsif Tense = Perf  then
                  return "been ";
               else
                  return "";
               end if;
            elsif Mood = Imp  then
               return "be ";
            else
               return "";
            end if;
         else
            return "";
         end if;
      end Been;

      function Ed return String is
      begin
         if Mood = Imp  then
            if Voice = Active  then
               return "!";
            elsif Voice = Passive  then
               return "ed!";
            else
               return "";
            end if;
         elsif Mood = Inf  then
            if Voice = Active  then
               return "";
            elsif Voice = Passive  then
               return "ed";
            else
               return "";
            end if;
         elsif Mood = Ind  then
            if Voice = Active  then
               if Tense = Pres  then
                  if (Person = 3) and (Number = S)  then
                     return "s";
                  else
                     return "";
                  end if;
               elsif Tense = Impf   then
                  if (Person = 1 or Person = 3) and (Number = S)  then
                     return "ed/was ~ing";
                  else
                     return "ed/were ~ing";
                  end if;
               elsif Tense in Perf .. Futp   then
                  return "ed";
               else
                  return "";
               end if;
            elsif Voice = Passive  then
               return "ed";
            else
               return "";
            end if;
         elsif Mood = Sub  then
            if Tense in Perf .. Plup  then
               return "ed";
            else
               return "";
            end if;
         else
            return "";
         end if;
      end Ed;

      function Sub return String is
      begin
         if Mood = Sub  then
            return "may/must/should ";
         else
            return "";
         end if;
      end Sub;

   begin   --  PUT_VERB_EXAMPLE
      if Kind = Dep then
         Voice := Active;    --  Should only have allowed PASSIVE at this point
      elsif Kind = Semidep and then Tense in Perf .. Futp   then
         Voice := Active;    --  Should only have allowed PASSIVE at this point
      end if;

      Ada.Text_IO.Put (Output, They & Sub & Shall & Have & Been & "~" & Ed);

   end Put_Verb_Example;

begin    --  PUT_EXAMPLE_LINE

   if Words_Mode (Do_Examples)
     and then (not (Configuration = Only_Meanings))
   then

      case Ir.Qual.Pofs is
         when N =>
            case Ir.Qual.Noun.Of_Case is
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
            case Ir.Qual.Adj.Comparison is
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
            case Ir.Qual.Adv.Comparison is
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
            Vk := De.Part.V.Kind;
            Ada.Text_IO.Set_Col (Output, 6);
            Put_Verb_Example (Output, Ir, Vk);
            Ada.Text_IO.New_Line (Output);

         when Vpar =>
            --    TEXT_IO.NEW_LINE (OUTPUT);
            --  Verb info too much for same line
            case Ir.Qual.Vpar.Tense_Voice_Mood.Tense is
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
                  if Ir.Qual.Vpar.Tense_Voice_Mood.Voice = Active  then
                     Ada.Text_IO.Put (Output,
                       "about/going/intending/destined to ~"
                       & "  FUT ACTIVE PPL often used as ADJ or N ");
                     Ada.Text_IO.New_Line (Output);
                  else
                     case Ir.Qual.Vpar.Of_Case is
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
            if Ir.Qual.Supine.Of_Case = Acc  then
               Ada.Text_IO.Put (Output,
                 "to ~  expresses purpose of verb of motion;"
                 & " may take a direct object");
               Ada.Text_IO.New_Line (Output);
            elsif Ir.Qual.Supine.Of_Case = Abl  then
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

end Words_Engine.Put_Example_Line;
