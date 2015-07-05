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

with Text_IO;
with Inflections_Package; use Inflections_Package;
with Dictionary_Package; use Dictionary_Package;
with config; use config;
with word_parameters; use word_parameters;
--with LATIN_DEBUG;
procedure Put_example_line(configuration : configuration_type;
                           Output : Text_IO.File_Type; ir : in Inflection_Record;
                                                       de : in dictionary_entry) is
   --      use LATIN_DEBUG;
   vk : Verb_Kind_Type;

   procedure Put_verb_example(Output : Text_IO.File_Type; ir : in Inflection_Record;
                                                          vk : in Verb_Kind_Type) is
      person : constant person_type      := ir.qual.v.person;
      number : constant number_type      := ir.qual.v.number;
      tense  : constant tense_type       := ir.qual.v.tense_voice_mood.tense;
      mood   : constant mood_type        := ir.qual.v.tense_voice_mood.mood;
      voice  : voice_type       := ir.qual.v.tense_voice_mood.voice;
      kind   : constant Verb_Kind_Type   := vk;
      --  Nothing on  (part), gerund,

      function they return String is
      begin
         if kind = impers  then
            return "it ";
         end if;

         if mood = inf then
            return "to ";
         end if;

         if mood = imp and tense = pres  and number = p  then
            return "(you) ";
         end if;

         if mood = sub and tense = pres  and
           person = 1 and number = p
         then
            return "let us ";   --  G&L 263 1
         end if;

         if  number = s  then
            if person = 1  then
               return "I ";
            elsif  person = 2  then
               return "you ";
            elsif  person = 3  then
               return "he/it ";
            else
               return "";
            end if;
         elsif number = p  then
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
         if tense = fut or tense = futp then
            if (mood = ind) or (mood = sub)  then
               if person = 1  then
                  return "shall ";
               elsif  person = 2  then
                  return "will ";
               elsif  person = 3  then
                  return "will ";
               else
                  return "";
               end if;
            elsif mood = imp  then
               if person = 1  then
                  return "will ";
               elsif  person = 2  then
                  return "(shall) ";
               elsif  person = 3  then
                  return "(shall) ";
               else
                  return "";
               end if;
            elsif mood = inf  then
               if tense = fut  then
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
         if tense in pres..fut  then
            return "";
         elsif tense = perf  then
            if (tense = perf) and (person = 3) and (number = s)  then
               return "has ";
            else
               return "have ";    -- works for INF too
            end if;
         elsif tense = plup  then
            if mood = ind  then
               return "had";
            elsif mood = sub  then
               return "have ";
            else
               return "";
            end if;
         elsif tense = futp   then
            return "have ";
         else
            return "";
         end if;
      end have;

      function been return String is
      begin
         if voice = passive  then
            if mood = ind  then
               if tense = pres  then
                  if (person = 1) and (number = s)  then
                     return "am/am being ";
                  elsif (person = 3) and (number = s)  then
                     return "is/is being ";
                  else
                     return "are/are being ";
                  end if;
               elsif tense = impf   then
                  if (person = 1 or person = 3) and (number = s)  then
                     return "was/was being ";
                  else
                     return "were/were being ";
                  end if;
               elsif tense = fut   then
                  return "be ";
               elsif tense = perf   then
                  if (person = 1 or person = 3) and (number = s)  then
                     return "been/was ";
                  else
                     return "been/were ";
                  end if;
               elsif tense in plup..futp   then
                  return "been ";
               else
                  return "";
               end if;
            elsif mood = sub  then
               return "";              --????????
            elsif mood = inf  then
               if tense = pres  then
                  return "be ";
               elsif tense = perf  then
                  return "been ";
               else
                  return "";
               end if;
            elsif mood = imp  then
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
         if mood = imp  then
            if voice = active  then
               return "!";
            elsif voice = passive  then
               return "ed!";
            else
               return "";
            end if;
         elsif mood = inf  then
            if voice = active  then
               return "";
            elsif voice = passive  then
               return "ed";
            else
               return "";
            end if;
         elsif mood = ind  then
            if voice = active  then
               if tense = pres  then
                  if (person = 3) and (number = s)  then
                     return "s";
                  else
                     return "";
                  end if;
               elsif tense = impf   then
                  if (person = 1 or person = 3) and (number = s)  then
                     return "ed/was ~ing";
                  else
                     return "ed/were ~ing";
                  end if;
               elsif tense in perf..futp   then
                  return "ed";
               else
                  return "";
               end if;
            elsif voice = passive  then
               return "ed";
            else
               return "";
            end if;
         elsif mood = sub  then
            if tense in perf..plup  then
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
         if mood = sub  then
            return "may/must/should ";
         else
            return "";
         end if;
      end sub;

   begin   --  PUT_VERB_EXAMPLE
      if kind = dep    then
         voice := active;    --  Should only have allowed PASSIVE at this point
      elsif kind = semidep    and then tense in perf..futp   then
         voice := active;    --  Should only have allowed PASSIVE at this point
      end if;

      Text_IO.Put(Output, they & sub & shall & have & been & "~" & ed);

   end Put_verb_example;

begin    --  PUT_EXAMPLE_LINE

   if words_mode(do_examples)  and then (not (configuration = only_meanings))   then

      case ir.qual.pofs is
         when n =>
            case ir.qual.n.cs is
               when gen =>
                  Text_IO.Put(Output, "~'s; of ~");
                  Text_IO.New_Line(Output);
               when abl =>
                  Text_IO.New_Line(Output);      --  Info too much for same line
                  Text_IO.Set_Col(Output, 6);
                  Text_IO.Put(Output,
                              "from _ (separ); because of ~ (cause); than ~ (compar); of ~ (circumstance)");
                  Text_IO.New_Line(Output);
               when dat =>
                  Text_IO.New_Line(Output);      --  Info too much for same line
                  Text_IO.Set_Col(Output, 6);
                  Text_IO.Put(Output,
                              "for _ (purpose, reference); to ~ (w/adjectives); to ~ (double dative)");
                  Text_IO.New_Line(Output);
               when loc =>
                  Text_IO.Put(Output, "at ~ (place where)");
                  Text_IO.New_Line(Output);
               when others  =>
                  null;
                  --TEXT_IO.NEW_LINE(OUTPUT);
            end case;

         when adj =>
            case ir.qual.adj.co is
               when comp  =>
                  Text_IO.Put(Output, "~er; more/too _");
                  Text_IO.New_Line(Output);
               when super =>
                  Text_IO.Put(Output, "~est; most/very");
                  Text_IO.New_Line(Output);
               when others  =>
                  null;
                  --TEXT_IO.NEW_LINE(OUTPUT);
            end case;

         when adv =>
            case ir.qual.adv.co is
               when comp  =>
                  Text_IO.Put(Output, "more/too ~(ly)");
                  Text_IO.New_Line(Output);
               when super =>
                  Text_IO.Put(Output, "most/very ~(ly)");
                  Text_IO.New_Line(Output);
               when others  =>
                  null;
                  --TEXT_IO.NEW_LINE(OUTPUT);
            end case;

         when v =>
            --TEXT_IO.NEW_LINE(OUTPUT);        --  Verb info too much for same line
            vk := de.part.v.Kind;
            Text_IO.Set_Col(Output, 6);
            Put_verb_example(Output, ir, vk);
            Text_IO.New_Line(Output);

         when vpar =>
            --    TEXT_IO.NEW_LINE(OUTPUT);        --  Verb info too much for same line
            case ir.qual.vpar.tense_voice_mood.tense is
               when perf  =>
                  Text_IO.Put(Output,
                              "~ed  PERF PASSIVE PPL often used as ADJ or N (amatus => belov.ed)");
                  Text_IO.New_Line(Output);
               when pres  =>
                  Text_IO.Put(Output,
                              "~ing  PRES ACTIVE PPL often used as ADJ or N (lov.ing, curl.y)");
                  Text_IO.New_Line(Output);
               when fut   =>
                  if ir.qual.vpar.tense_voice_mood.voice = active  then
                     Text_IO.Put(Output,
                                 "about/going/intending/destined to ~  FUT ACTIVE PPL often used as ADJ or N ");
                     Text_IO.New_Line(Output);
                  else
                     case ir.qual.vpar.cs is
                        when gen =>
                           Text_IO.Put(Output,
                                       "to(/must) be ~ed  FUT PASSIVE PPL, often used as gerund or gerundive (of ~ing)");
                        when dat =>
                           Text_IO.Put(Output,
                                       "to(/must) be ~ed  FUT PASSIVE PPL, often used as gerund or gerundive (to/for ~ing)");
                        when abl =>
                           Text_IO.Put(Output,
                                       "to(/must) be ~ed  FUT PASSIVE PPL, often used as gerund or gerundive (by/in ~ing)");
                        when acc =>
                           Text_IO.Put(Output,
                                       "to(/must) be ~ed  FUT PASSIVE PPL, often used as gerund or gerundive (for ~ing/to ~)");
                        when others =>
                           Text_IO.Put(Output,
                                       "to(/must) be ~ed  FUT PASSIVE PPL, often used as gerund or gerundive (~ing)");
                     end case;
                     Text_IO.New_Line(Output);
                  end if;
               when others  =>
                  null;
                  --TEXT_IO.NEW_LINE(OUTPUT);
            end case;      --  TENSE

         when supine =>
            --TEXT_IO.NEW_LINE(OUTPUT);
            if ir.qual.supine.cs = acc  then
               Text_IO.Put(Output,
                           "to ~  expresses purpose of verb of motion; may take a direct object");
               Text_IO.New_Line(Output);
            elsif ir.qual.supine.cs = abl  then
               Text_IO.Put(Output,
                           "to ~  after ADJ indicating aspect/respect in which something is/is done");
               Text_IO.New_Line(Output);
            end if;

         when others  =>
            null;
            --TEXT_IO.NEW_LINE(OUTPUT);
      end case;        --  PART

   else
      null;
      --TEXT_IO.NEW_LINE(OUTPUT);
   end if;

end Put_example_line;
