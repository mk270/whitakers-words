   with TEXT_IO; 
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with CONFIG; use CONFIG;
   with WORD_PARAMETERS; use WORD_PARAMETERS;
--with LATIN_DEBUG; 
   procedure PUT_EXAMPLE_LINE(OUTPUT : TEXT_IO.FILE_TYPE; IR : in INFLECTION_RECORD; 
                              DE : in DICTIONARY_ENTRY) is
   --      use LATIN_DEBUG;
      VK : VERB_KIND_TYPE;
      
      procedure PUT_VERB_EXAMPLE(OUTPUT : TEXT_IO.FILE_TYPE; IR : in INFLECTION_RECORD; 
                                 VK : in VERB_KIND_TYPE) is
         PERSON : constant PERSON_TYPE      := IR.QUAL.V.PERSON;
         NUMBER : constant NUMBER_TYPE      := IR.QUAL.V.NUMBER;
         TENSE  : constant TENSE_TYPE       := IR.QUAL.V.TENSE_VOICE_MOOD.TENSE;
         MOOD   : constant MOOD_TYPE        := IR.QUAL.V.TENSE_VOICE_MOOD.MOOD; 
         VOICE  : VOICE_TYPE       := IR.QUAL.V.TENSE_VOICE_MOOD.VOICE;
         KIND   : VERB_KIND_TYPE   := VK; 
      --  Nothing on  (part), gerund, 
      
         function THEY return STRING is
         begin
            if KIND = IMPERS  then
               return "it ";
            end if;
         
            if MOOD = INF then
               return "to ";
            end if;
         
            if MOOD = IMP and TENSE = PRES  and NUMBER = P  then
               return "(you) ";
            end if;
         
            if MOOD = SUB and TENSE = PRES  and 
            PERSON = 1 and NUMBER = P  then
               return "let us ";   --  G&L 263 1
            end if;
         
            if  NUMBER = S  then
               if PERSON = 1  then
                  return "I ";
               elsif  PERSON = 2  then
                  return "you ";
               elsif  PERSON = 3  then
                  return "he/it ";
               else
                  return "";
               end if;
            elsif NUMBER = P  then
               if PERSON = 1  then
                  return "we ";
               elsif  PERSON = 2  then
                  return "you ";
               elsif  PERSON = 3  then
                  return "they ";
               else
                  return "";
               end if;
            else
               return "";
            end if;
         end THEY;
      
         function SHALL return STRING is
         begin            --  ACTIVE only  !!!!!!!!!!!!!!!!
            if (TENSE = FUT or TENSE = FUTP )  then
               if (MOOD = IND) or (MOOD = SUB)  then
                  if PERSON = 1  then
                     return "shall ";
                  elsif  PERSON = 2  then
                     return "will ";
                  elsif  PERSON = 3  then
                     return "will ";
                  else 
                     return "";
                  end if;
               elsif MOOD = IMP  then
                  if PERSON = 1  then
                     return "will ";
                  elsif  PERSON = 2  then
                     return "(shall) ";
                  elsif  PERSON = 3  then
                     return "(shall) ";
                  else
                     return "";
                  end if;
               elsif MOOD = INF  then
                  if TENSE = FUT  then
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
         end SHALL;
      
         function HAVE return STRING is
         begin
            if TENSE in PRES..FUT  then
               return "";
            elsif TENSE = PERF  then
               if (TENSE = PERF) and (PERSON = 3) and (NUMBER = S)  then
                  return "has ";
               else
                  return "have ";    -- works for INF too
               end if;
            elsif TENSE = PLUP  then
               if MOOD = IND  then
                  return "had";
               elsif MOOD = SUB  then
                  return "have ";
               else
                  return "";
               end if;
            elsif TENSE = FUTP   then
               return "have ";
            else
               return "";
            end if;
         end HAVE;
      
         function BEEN return STRING is
         begin
            if VOICE = PASSIVE  then
               if MOOD = IND  then
                  if TENSE = PRES  then
                     if (PERSON = 1) and (NUMBER = S)  then
                        return "am/am being ";
                     elsif (PERSON = 3) and (NUMBER = S)  then
                        return "is/is being ";
                     else
                        return "are/are being ";
                     end if;
                  elsif TENSE = IMPF   then
                     if (PERSON = 1 or PERSON = 3) and (NUMBER = S)  then
                        return "was/was being ";
                     else
                        return "were/were being ";
                     end if;
                  elsif TENSE = FUT   then
                     return "be ";
                  elsif TENSE = PERF   then
                     if (PERSON = 1 or PERSON = 3) and (NUMBER = S)  then
                        return "been/was ";                
                     else
                        return "been/were ";              
                     end if;
                  elsif TENSE in PLUP..FUTP   then
                     return "been ";
                  else 
                     return "";
                  end if;
               elsif MOOD = SUB  then
                  return "";              --????????
               elsif MOOD = INF  then
                  if TENSE = PRES  then
                     return "be ";
                  elsif TENSE = PERF  then
                     return "been ";
                  else 
                     return "";
                  end if;
               elsif MOOD = IMP  then
                  return "be ";
               else
                  return "";
               end if;
            else
               return "";
            end if;
         end BEEN;
      
         function ED return STRING is
         begin
            if MOOD = IMP  then
               if VOICE = ACTIVE  then
                  return "!";
               elsif VOICE = PASSIVE  then
                  return "ed!";
               else
                  return "";
               end if;            
            elsif MOOD = INF  then
               if VOICE = ACTIVE  then
                  return "";
               elsif VOICE = PASSIVE  then
                  return "ed";
               else
                  return "";
               end if;
            elsif MOOD = IND  then
               if VOICE = ACTIVE  then
                  if TENSE = PRES  then
                     if (PERSON = 3) and (NUMBER = S)  then
                        return "s";
                     else
                        return "";
                     end if;
                  elsif TENSE = IMPF   then
                     if (PERSON = 1 or PERSON = 3) and (NUMBER = S)  then
                        return "ed/was ~ing";
                     else
                        return "ed/were ~ing";
                     end if;
                  elsif TENSE in PERF..FUTP   then
                     return "ed";
                  else 
                     return "";
                  end if;
               elsif VOICE = PASSIVE  then
                  return "ed";
               else 
                  return "";
               end if;
            elsif MOOD = SUB  then
               if TENSE in PERF..PLUP  then
                  return "ed";
               else
                  return "";
               end if;
            else 
               return "";
            end if;
         end ED;
      
         function SUB return STRING is 
         begin
            if MOOD = SUB  then
               return "may/must/should ";
            else 
               return "";
            end if;
         end SUB;
      
      
      begin   --  PUT_VERB_EXAMPLE
         if KIND = DEP    then   
            VOICE := ACTIVE;    --  Should only have allowed PASSIVE at this point
         elsif KIND = SEMIDEP    and then TENSE in PERF..FUTP   then
            VOICE := ACTIVE;    --  Should only have allowed PASSIVE at this point
         end if;
      
         TEXT_IO.PUT(OUTPUT, THEY & SUB & SHALL & HAVE & BEEN & "~" & ED);
      
      end PUT_VERB_EXAMPLE;  
   
   
   begin    --  PUT_EXAMPLE_LINE
   
   --TEXT_IO.PUT("In EXAMPLES  "); 
   --TEXT_IO.PUT("  LKM  "); BOOLEAN_IO.PUT(WORDS_MDEV(LOCK_MEANINGS));
   --TEXT_IO.PUT("   /LKM  "); BOOLEAN_IO.PUT((not WORDS_MDEV(LOCK_MEANINGS))  );
   
      if WORDS_MODE(DO_EXAMPLES)  and then (not (CONFIGURATION = ONLY_MEANINGS))   then
      
         case IR.QUAL.POFS is 
            when N => 
               case IR.QUAL.N.CS is
                  when GEN =>
                     TEXT_IO.PUT(OUTPUT, "~'s; of ~"); 
                     TEXT_IO.NEW_LINE(OUTPUT);
                  when ABL =>
                     TEXT_IO.NEW_LINE(OUTPUT);      --  Info too much for same line
                     TEXT_IO.SET_COL(OUTPUT, 6);
                     TEXT_IO.PUT(OUTPUT, 
                                 "from _ (separ); because of ~ (cause); than ~ (compar); of ~ (circumstance)");
                     TEXT_IO.NEW_LINE(OUTPUT);
                  when DAT =>
                     TEXT_IO.NEW_LINE(OUTPUT);      --  Info too much for same line
                     TEXT_IO.SET_COL(OUTPUT, 6);
                     TEXT_IO.PUT(OUTPUT, 
                                 "for _ (purpose, reference); to ~ (w/adjectives); to ~ (double dative)");
                     TEXT_IO.NEW_LINE(OUTPUT);
                  when LOC =>
                     TEXT_IO.PUT(OUTPUT, "at ~ (place where)");
                     TEXT_IO.NEW_LINE(OUTPUT);
                  when others  => 
                     null;
               --TEXT_IO.NEW_LINE(OUTPUT); 
               end case;
         
            when ADJ => 
               case IR.QUAL.ADJ.CO is
                  when COMP  => 
                     TEXT_IO.PUT(OUTPUT, "~er; more/too _");
                     TEXT_IO.NEW_LINE(OUTPUT);
                  when SUPER => 
                     TEXT_IO.PUT(OUTPUT, "~est; most/very");
                     TEXT_IO.NEW_LINE(OUTPUT);
                  when others  => 
                     null;
               --TEXT_IO.NEW_LINE(OUTPUT); 
               end case;
         
            when ADV => 
               case IR.QUAL.ADV.CO is
                  when COMP  => 
                     TEXT_IO.PUT(OUTPUT, "more/too ~(ly)");
                     TEXT_IO.NEW_LINE(OUTPUT);
                  when SUPER => 
                     TEXT_IO.PUT(OUTPUT, "most/very ~(ly)");
                     TEXT_IO.NEW_LINE(OUTPUT);
                  when others  => 
                     null;
               --TEXT_IO.NEW_LINE(OUTPUT); 
               end case;
         
            when V => 
            --TEXT_IO.NEW_LINE(OUTPUT);        --  Verb info too much for same line
               VK := DE.PART.V.KIND;
               TEXT_IO.SET_COL(OUTPUT, 6);
               PUT_VERB_EXAMPLE(OUTPUT, IR, VK);
               TEXT_IO.NEW_LINE(OUTPUT);
         
            when VPAR => 
            --    TEXT_IO.NEW_LINE(OUTPUT);        --  Verb info too much for same line
               case IR.QUAL.VPAR.TENSE_VOICE_MOOD.TENSE is
                  when PERF  => 
                     TEXT_IO.PUT(OUTPUT, 
                                 "~ed  PERF PASSIVE PPL often used as ADJ or N (amatus => belov.ed)");
                     TEXT_IO.NEW_LINE(OUTPUT);
                  when PRES  => 
                     TEXT_IO.PUT(OUTPUT, 
                                 "~ing  PRES ACTIVE PPL often used as ADJ or N (lov.ing, curl.y)");
                     TEXT_IO.NEW_LINE(OUTPUT);
                  when FUT   => 
                     if IR.QUAL.VPAR.TENSE_VOICE_MOOD.VOICE = ACTIVE  then
                        TEXT_IO.PUT(OUTPUT, 
                                    "about/going/intending/destined to ~  FUT ACTIVE PPL often used as ADJ or N ");
                        TEXT_IO.NEW_LINE(OUTPUT);
                     else
                        case IR.QUAL.VPAR.CS is
                           when GEN =>
                              TEXT_IO.PUT(OUTPUT, 
                                          "to(/must) be ~ed  FUT PASSIVE PPL, often used as gerund or gerundive (of ~ing)");
                           when DAT =>
                              TEXT_IO.PUT(OUTPUT, 
                                          "to(/must) be ~ed  FUT PASSIVE PPL, often used as gerund or gerundive (to/for ~ing)");
                           when ABL =>
                              TEXT_IO.PUT(OUTPUT, 
                                          "to(/must) be ~ed  FUT PASSIVE PPL, often used as gerund or gerundive (by/in ~ing)");
                           when ACC =>
                              TEXT_IO.PUT(OUTPUT, 
                                          "to(/must) be ~ed  FUT PASSIVE PPL, often used as gerund or gerundive (for ~ing/to ~)");
                           when others =>
                              TEXT_IO.PUT(OUTPUT, 
                                          "to(/must) be ~ed  FUT PASSIVE PPL, often used as gerund or gerundive (~ing)");
                        end case;
                        TEXT_IO.NEW_LINE(OUTPUT);
                     end if;
                  when others  => 
                     null;
               --TEXT_IO.NEW_LINE(OUTPUT); 
               end case;      --  TENSE
         
            when SUPINE => 
            --TEXT_IO.NEW_LINE(OUTPUT);
               if IR.QUAL.SUPINE.CS = ACC  then
                  TEXT_IO.PUT(OUTPUT, 
                              "to ~  expresses purpose of verb of motion; may take a direct object");
                  TEXT_IO.NEW_LINE(OUTPUT);
               elsif IR.QUAL.SUPINE.CS = ABL  then
                  TEXT_IO.PUT(OUTPUT, 
                              "to ~  after ADJ indicating aspect/respect in which something is/is done");
                  TEXT_IO.NEW_LINE(OUTPUT);
               end if;   
         
            when others  => 
               null;
         --TEXT_IO.NEW_LINE(OUTPUT); 
         end case;        --  PART
      
      else
         null;
      --TEXT_IO.NEW_LINE(OUTPUT); 
      end if;
   
   end PUT_EXAMPLE_LINE;  
