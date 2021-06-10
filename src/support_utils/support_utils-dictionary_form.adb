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
-- All Parts of the WORDS system, source code and data files, are made freely
-- available to anyone who wishes to use them, for whatever purpose.

with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
function Support_Utils.Dictionary_Form (De : Dictionary_Entry) return String is

   Null_Ox : constant String (1 .. 24) := (others => ' ');
   Ox : array (1 .. 4) of String (1 .. 24) := (others => Null_Ox);
   Form : String (1 .. 100) := (others => ' ');

   Fst : constant array (Which_Type range 1 .. 5) of String (1 .. 3) :=
     ("1st", "2nd", "3rd", "4th", "5th");

   Not_Found : exception;

   function Add (Stem, Infl : String) return String is
   begin
      return Head (Trim (Stem) & Trim (Infl), 24);
   end Add;

   procedure Add_Up (Factor : String) is
   begin
      Form := Head (Trim (Form) & Trim (Factor), 100);
   end Add_Up;

   procedure Add_To (Factor : String) is
   begin
      Form := Head (Trim (Form) & Factor, 100);
   end Add_To;

begin
   --DICTIONARY_ENTRY_IO.PUT (DE);
   --  So I can call with a NULL_DICTIONARY_ENTRY and not bomb
   if De = Null_Dictionary_Entry  then
      return "";
   end if;

   if De.Part.Pofs = Prep then
      return Trim (De.Stems (1)) & "  " &
        Part_Of_Speech_Type'Image (De.Part.Pofs) &
        "  " & Case_Type'Image (De.Part.Prep.Obj);
   end if;

   if De.Stems (2 .. 4) = (Null_Stem_Type, Null_Stem_Type, Null_Stem_Type)  and
     not (((De.Part.Pofs = N)  and then (De.Part.N.Decl.Which = 9))  or
     ((De.Part.Pofs = Adj)  and then
     ((De.Part.Adj.Decl.Which = 9) or
     (De.Part.Adj.Co = Comp or De.Part.Adj.Co = Super))) or
     ((De.Part.Pofs = V)  and then (De.Part.V.Con = (9, 8))) or
     ((De.Part.Pofs = V)  and then (De.Part.V.Con = (9, 9))))
   then
      return Trim (De.Stems (1)) & "  " &
        Part_Of_Speech_Type'Image (De.Part.Pofs);
      --  For UNIQUES, CONJ, INTERJ, . ..
   end if;

   if De.Part.Pofs = N    then
      case De.Part.N.Decl.Which is
         -- First declension noun
         when 1 =>
            case De.Part.N.Decl.Var is
               when 1 =>
                  Ox (1) := Add (De.Stems (1), "a");
                  Ox (2) := Add (De.Stems (2), "ae");
               -- Greek nouns
               when 6 =>
                  Ox (1) := Add (De.Stems (1), "e");
                  Ox (2) := Add (De.Stems (2), "es");
               when 7 =>
                  Ox (1) := Add (De.Stems (1), "es");
                  Ox (2) := Add (De.Stems (2), "ae");
               when 8 =>
                  Ox (1) := Add (De.Stems (1), "as");
                  Ox (2) := Add (De.Stems (2), "ae");
               when others => null;
            end case;

         -- Second declension noun
         when 2 =>
            case De.Part.N.Decl.Var is
               when 1 =>
                  Ox (1) := Add (De.Stems (1), "us");
                  Ox (2) := Add (De.Stems (2), "i");
               when 2 =>
                  Ox (1) := Add (De.Stems (1), "um");
                  Ox (2) := Add (De.Stems (2), "i");
               when 3 =>
                  Ox (1) := Add (De.Stems (1), "");
                  Ox (2) := Add (De.Stems (2), "i");
               when 4 =>
                  if De.Part.N.Gender = N  then
                     Ox (1) := Add (De.Stems (1), "um");
                  else
                     Ox (1) := Add (De.Stems (1), "us");
                  end if;
                  Ox (2) := Add (De.Stems (2), "(i)");
               when 5 =>
                  Ox (1) := Add (De.Stems (1), "us");
                  Ox (2) := Add (De.Stems (2), "");
               when 6 =>
                  Ox (1) := Add (De.Stems (1), "os");
                  Ox (2) := Add (De.Stems (2), "i");
               when 7 =>
                  Ox (1) := Add (De.Stems (1), "os");
                  Ox (2) := Add (De.Stems (2), "i");
               when 8 =>
                  Ox (1) := Add (De.Stems (1), "on");
                  Ox (2) := Add (De.Stems (2), "i");
               when 9 =>
                  Ox (1) := Add (De.Stems (1), "us");
                  Ox (2) := Add (De.Stems (2), "i");
               when others => null;
            end case;

         -- Third declension noun
         when 3 =>
            Ox (1) := Add (De.Stems (1), "");
            if (De.Part.N.Decl.Var = 7)  or
              (De.Part.N.Decl.Var = 9)
            then
               Ox (2) := Add (De.Stems (2), "os/is");
            else
               Ox (2) := Add (De.Stems (2), "is");
            end if;

         -- Fourth declension noun
         when 4 =>
            case De.Part.N.Decl.Var is
               when 1 =>
                  Ox (1) := Add (De.Stems (1), "us");
                  Ox (2) := Add (De.Stems (2), "us");
               when 2 =>
                  Ox (1) := Add (De.Stems (1), "u");
                  Ox (2) := Add (De.Stems (2), "us");
               when 3 =>
                  Ox (1) := Add (De.Stems (1), "us");
                  Ox (2) := Add (De.Stems (2), "u");
               when others => null;
            end case;

         -- Fifth decelnsion noun
         when 5 =>
            Ox (1) := Add (De.Stems (1), "es");
            Ox (2) := Add (De.Stems (2), "ei");

         when 9 =>
            case De.Part.N.Decl.Var is
               when 8 =>
                  Ox (1) := Add (De.Stems (1), ".");
                  Ox (2) := Add (Null_Ox, "abb.");
               when 9 =>
                  Ox (1) := Add (De.Stems (1), "");
                  Ox (2) := Add (Null_Ox, "undeclined");
               when others => null;
            end case;

         when others =>
            raise Not_Found;
      end case;

   elsif De.Part.Pofs = Pron    then
      case De.Part.Pron.Decl.Which is
         -- Proximal demonstrative pronoun (hic, haec, hoc)
         when 3 =>
            Ox (1) := Add (De.Stems (1), "ic");
            Ox (2) := Add (De.Stems (1), "aec");
            if De.Part.Pron.Decl.Var = 1  then
               Ox (3) := Add (De.Stems (1), "oc");
            elsif De.Part.Pron.Decl.Var = 2  then
               Ox (3) := Add (De.Stems (1), "uc");
            end if;

         when 4 =>
            if De.Part.Pron.Decl.Var = 1  then
               Ox (1) := Add (De.Stems (1), "s");
               Ox (2) := Add (De.Stems (2), "a");
               Ox (3) := Add (De.Stems (1), "d");
            elsif De.Part.Pron.Decl.Var = 2  then
               Ox (1) := Add (De.Stems (1), "dem");
               Ox (2) := Add (De.Stems (2), "adem");
               Ox (3) := Add (De.Stems (1), "dem");
            end if;

         -- Distal (ille, illa, illud) and medial (iste, ista, istud)
         -- demonstrative pronoun
         when 6 =>
            Ox (1) := Add (De.Stems (1), "e");
            Ox (2) := Add (De.Stems (1), "a");
            if De.Part.Pron.Decl.Var = 1  then
               Ox (3) := Add (De.Stems (1), "ud");
            elsif De.Part.Pron.Decl.Var = 2  then
               Ox (3) := Add (De.Stems (1), "um");
            end if;

         when 9 =>
            if De.Part.Pron.Decl.Var = 8  then
               Ox (1) := Add (De.Stems (1), ".");
               Ox (2) := Add (Null_Ox, "abb.");
            elsif De.Part.Pron.Decl.Var = 9  then
               Ox (1) := Add (De.Stems (1), "");
               Ox (2) := Add (Null_Ox, "undeclined");
            end if;

         when others =>
            raise Not_Found;
      end case;

   elsif De.Part.Pofs = Adj  then

      --TEXT_IO.NEW_LINE;
      --DICTIONARY_ENTRY_IO.PUT (DE);
      --TEXT_IO.NEW_LINE;

      case De.Part.Adj.Co is
         when Comp =>
            Ox (1) := Add (De.Stems (1), "or");
            Ox (2) := Add (De.Stems (1), "or");
            Ox (3) := Add (De.Stems (1), "us");
         when Super =>
            Ox (1) := Add (De.Stems (1), "mus");
            Ox (2) := Add (De.Stems (1), "ma");
            Ox (3) := Add (De.Stems (1), "mum");

         when Pos =>
            -- First declension adjective
            if De.Part.Adj.Decl.Which = 1  then
               case De.Part.Adj.Decl.Var is
                  when 1 =>
                     Ox (1) := Add (De.Stems (1), "us");
                     Ox (2) := Add (De.Stems (2), "a");
                     Ox (3) := Add (De.Stems (2), "um");
                  when 2 =>
                     Ox (1) := Add (De.Stems (1), "");
                     Ox (2) := Add (De.Stems (2), "a");
                     Ox (3) := Add (De.Stems (2), "um");
                  when 3 =>
                     Ox (1) := Add (De.Stems (1), "us");
                     Ox (2) := Add (De.Stems (2), "a");
                     Ox (3) := Add (De.Stems (2), "um (gen -ius)");
                  when 4 =>
                     Ox (1) := Add (De.Stems (1), "");
                     Ox (2) := Add (De.Stems (2), "a");
                     Ox (3) := Add (De.Stems (2), "um");
                  when 5 =>
                     Ox (1) := Add (De.Stems (1), "us");
                     Ox (2) := Add (De.Stems (2), "a");
                     Ox (3) := Add (De.Stems (2), "ud");
                  when others =>
                     raise Not_Found;
               end case;

            -- Second declension adjective
            elsif De.Part.Adj.Decl.Which = 2  then
               case De.Part.Adj.Decl.Var is
                  when 1 =>
                     Ox (1) := Add (Null_Ox, "-");
                     Ox (2) := Add (De.Stems (1), "e");
                     Ox (3) := Add (Null_Ox, "-");
                  when 2 =>
                     Ox (1) := Add (Null_Ox, "-");
                     Ox (2) := Add (Null_Ox, "a");
                     Ox (3) := Add (Null_Ox, "-");
                  when 3 =>
                     Ox (1) := Add (De.Stems (1), "es");
                     Ox (2) := Add (De.Stems (1), "es");
                     Ox (3) := Add (De.Stems (1), "es");
                  when 6 =>
                     Ox (1) := Add (De.Stems (1), "os");
                     Ox (2) := Add (De.Stems (1), "os");
                     Ox (3) := Add (Null_Ox, "-");
                  when 7 =>
                     Ox (1) := Add (De.Stems (1), "os");
                     Ox (2) := Add (Null_Ox, "-");
                     Ox (3) := Add (Null_Ox, "-");
                  when 8 =>
                     Ox (1) := Add (Null_Ox, "-");
                     Ox (2) := Add (Null_Ox, "-");
                     Ox (3) := Add (De.Stems (2), "on");
                  when others => null;
               end case;

            -- Third declension adjective
            elsif De.Part.Adj.Decl.Which = 3  then
               case De.Part.Adj.Decl.Var is
                  when 1 =>
                     Ox (1) := Add (De.Stems (1), "");
                     Ox (2) := Add (Null_Ox, "(gen.)");
                     Ox (3) := Add (De.Stems (2), "is");
                  when 2 =>
                     Ox (1) := Add (De.Stems (1), "is");
                     Ox (2) := Add (De.Stems (2), "is");
                     Ox (3) := Add (De.Stems (2), "e");
                  when 3 =>
                     Ox (1) := Add (De.Stems (1), "");
                     Ox (2) := Add (De.Stems (2), "is");
                     Ox (3) := Add (De.Stems (2), "e");
                  when 6 =>
                     Ox (1) := Add (De.Stems (1), "");
                     Ox (2) := Add (Null_Ox, "(gen.)");
                     Ox (3) := Add (De.Stems (2), "os");
                  when others => null;
               end case;

            elsif De.Part.Adj.Decl = (9, 8)  then
               Ox (1) := Add (De.Stems (1), ".");
               Ox (2) := Add (Null_Ox, "abb.");

            elsif De.Part.Adj.Decl = (9, 9)  then
               Ox (1) := Add (De.Stems (1), "");
               Ox (2) := Add (Null_Ox, "undeclined");

            else
               raise Not_Found;
            end if;

         when X =>
            case De.Part.Adj.Decl.Which is
               when 1 =>
                  if De.Part.Adj.Decl.Var = 1  then
                     Ox (1) := Add (De.Stems (1), "us");
                     Ox (2) := Add (De.Stems (2), "a -um");
                     Ox (3) := Add (De.Stems (3), "or -or -us");
                     Ox (4) := Add (De.Stems (4), "mus -a -um");
                  elsif De.Part.Adj.Decl.Var = 2  then
                     Ox (1) := Add (De.Stems (1), "");
                     Ox (2) := Add (De.Stems (2), "a -um");
                     Ox (3) := Add (De.Stems (3), "or -or -us");
                     Ox (4) := Add (De.Stems (4), "mus -a -um");
                  end if;

               when 3 =>
                  case De.Part.Adj.Decl.Var is
                     when 1 =>
                        Ox (1) := Add (De.Stems (1), "");
                        Ox (2) := Add (De.Stems (2), "is (gen.)");
                        Ox (3) := Add (De.Stems (3), "or -or -us");
                        Ox (4) := Add (De.Stems (4), "mus -a -um");
                     when 2 =>
                        Ox (1) := Add (De.Stems (1), "is");
                        Ox (2) := Add (De.Stems (2), "e");
                        Ox (3) := Add (De.Stems (3), "or -or -us");
                        Ox (4) := Add (De.Stems (4), "mus -a -um");
                     when 3 =>
                        Ox (1) := Add (De.Stems (1), "");
                        Ox (2) := Add (De.Stems (2), "is -e");
                        Ox (3) := Add (De.Stems (3), "or -or -us");
                        Ox (4) := Add (De.Stems (4), "mus -a -um");
                     when others => null;
                  end case;

               when 9 =>
                  Ox (1) := Add (De.Stems (1), "");
                  Ox (2) := Add (Null_Ox, "undeclined");
                  Ox (3) := Add (De.Stems (3), "or -or -us");
                  Ox (4) := Add (De.Stems (4), "mus -a -um");

               when others =>
                  raise Not_Found;
            end case;
      end case;

   elsif (De.Part.Pofs = Adv) and then (De.Part.Adv.Co = X)  then
      Ox (1) := Add (De.Stems (1), "");
      Ox (2) := Add (De.Stems (2), "");
      Ox (3) := Add (De.Stems (3), "");

   elsif De.Part.Pofs = V    then

      if De.Part.V.Kind = Dep  then    --  all DEP
         Ox (3) := Add (Null_Ox, "DEP");  --  Flag for later use
         Ox (4) := Add (De.Stems (4), "us sum");
         case De.Part.V.Con.Which is
            when 1 =>
               Ox (1) := Add (De.Stems (1), "or");
               Ox (2) := Add (De.Stems (2), "ari");
            when 2 =>
               Ox (1) := Add (De.Stems (1), "eor");
               Ox (2) := Add (De.Stems (2), "eri");
            when 3 =>
               Ox (1) := Add (De.Stems (1), "or");
               --  Would be wrong for 3 3, but no 3 3 DEP
               if De.Part.V.Con.Var = 4  then
                  Ox (2) := Add (De.Stems (2), "iri");
               else
                  Ox (2) := Add (De.Stems (2), "i");
               end if;
            when others =>
               raise Not_Found;
         end case;                      --  all DEP handled

      elsif De.Part.V.Kind = Perfdef  then   --  all PERFDEF handled
         Ox (1) := Add (De.Stems (3), "i");
         Ox (2) := Add (De.Stems (3), "isse");
         Ox (3) := Add (De.Stems (4), "us");
         Ox (4) := Null_Ox;  --  Flag for later use

      elsif De.Part.V.Kind = Impers  and then
        ((De.Stems (1)(1 .. 3) = "zzz")  and   -- Recognize as PERFDEF IMPERS
        (De.Stems (2)(1 .. 3) = "zzz"))
      then
         Ox (1) := Add (De.Stems (3), "it");
         Ox (2) := Add (De.Stems (3), "isse");
         Ox (3) := Add (De.Stems (4), "us est");
         --          OX (4) := ADD (NULL_OX, "PERFDEF");

      else                            --  Not DEP/PERFDEF/IMPERS

         if De.Part.V.Kind = Impers  then
            case De.Part.V.Con.Which is
               when 1 =>
                  Ox (1) := Add (De.Stems (1), "at");
               when 2 =>
                  Ox (1) := Add (De.Stems (1), "et");
               when 3 =>
                  if De.Part.V.Con.Var = 2  then
                     Ox (1) := Add (De.Stems (1), "t");
                  else
                     if De.Stems (1)(Trim (De.Stems (1))'Last) = 'i'  then
                        Ox (1) := Add (De.Stems (1), "t");
                     else
                        Ox (1) := Add (De.Stems (1), "it");
                     end if;
                  end if;
               when 5 =>
                  if De.Part.V.Con.Var = 1  then
                     Ox (1) := Add (De.Stems (1), "est");
                  end if;
               when 7 =>
                  if De.Part.V.Con.Var = 1  or
                    De.Part.V.Con.Var = 2
                  then
                     Ox (1) := Add (De.Stems (1), "t");
                  end if;
               when others => null;
            end case;

         else

            --  OX 1
            if De.Part.V.Con.Which = 2  then
               Ox (1) := Add (De.Stems (1), "eo");

            elsif De.Part.V.Con.Which = 5  then
               Ox (1) := Add (De.Stems (1), "um");
            elsif De.Part.V.Con = (7, 2)  then
               Ox (1) := Add (De.Stems (1), "am");
            else
               Ox (1) := Add (De.Stems (1), "o");
            end if;
         end if;

         --  OX 2
         case De.Part.V.Con.Which is
            when 1 =>
               Ox (2) := Add (De.Stems (2), "are");
            when 2 =>
               Ox (2) := Add (De.Stems (2), "ere");
            when 3 =>
               case De.Part.V.Con.Var is
                  when 2 =>
                     Ox (2) := Add (De.Stems (2), "re");
                  when 3 =>
                     -- Special case for fio, fieri: it follows the usual
                     -- conjugation everywhere except for present infinitive
                     if Trim (De.Stems (2)) = "f" then
                        Ox (2) := Add (De.Stems (2), "ieri");
                     else
                        Ox (2) := Add (De.Stems (2), "eri");
                     end if;
                  when 4 =>
                     Ox (2) := Add (De.Stems (2), "ire");
                  when others =>
                     Ox (2) := Add (De.Stems (2), "ere");
               end case;
               --            elsif DE.PART.V.CON.WHICH = 4  then
               --              OX (2) := ADD (DE.STEMS (2), "ire");
            when 5 =>
               if De.Part.V.Con.Var = 1  then
                  Ox (2) := Add (De.Stems (2), "esse");
               elsif De.Part.V.Con.Var = 2  then
                  Ox (2) := Add (De.Stems (1), "e");  --  tricky, but it is 1
               end if;
            when 6 =>
               if De.Part.V.Con.Var = 1  then
                  Ox (2) := Add (De.Stems (2), "re");
               elsif De.Part.V.Con.Var = 2  then
                  Ox (2) := Add (De.Stems (2), "le");
               end if;
            when 7 =>
               if De.Part.V.Con.Var = 3  then
                  Ox (2) := Add (De.Stems (2), "se");
               end if;
            when 8 =>
               case De.Part.V.Con.Var is
                  when 1 =>
                     Ox (2) := Add (De.Stems (2), "are");
                  when 2 =>
                     Ox (2) := Add (De.Stems (2), "ere");
                  when 3 =>
                     Ox (2) := Add (De.Stems (2), "ere");
                  when 4 =>
                     Ox (2) := Add (De.Stems (2), "ire");
                  when others =>
                     Ox (2) := Add (De.Stems (2), "ere");
               end case;
            when 9 =>
               if De.Part.V.Con.Var = 8  then
                  Ox (1) := Add (De.Stems (1), ".");
                  Ox (2) := Add (Null_Ox, "abb.");
               elsif De.Part.V.Con.Var = 9  then
                  Ox (1) := Add (De.Stems (1), "");
                  Ox (2) := Add (Null_Ox, "undeclined");
               end if;
            when others => null;

         end case;                        --  OX (2) handled

         --  OX 3 & 4
         if De.Part.V.Kind = Impers  then
            if Ox (3)(1 .. 7) /= "PERFDEF" then
               Ox (3) := Add (De.Stems (3), "it");
            end if;
            Ox (4) := Add (De.Stems (4), "us est");
         elsif De.Part.V.Kind  = Semidep  then    --  Finalization correction
            Ox (4) := Add (De.Stems (4), "us sum");
         elsif De.Part.V.Con = (5, 1)  then
            Ox (3) := Add (De.Stems (3), "i");
            Ox (4) := Add (De.Stems (4), "urus");
         elsif De.Part.V.Con.Which = 8  then
            Ox (3) := Add ("", "additional");
            Ox (4) := Add ("", "forms");
         elsif De.Part.V.Con.Which = 9  then
            Ox (3) := Add (Null_Ox, "BLANK");  --  Flag for later use
            Ox (4) := Add (Null_Ox, "BLANK");  --  Flag for later use
         else
            Ox (3) := Add (De.Stems (3), "i");
            Ox (4) := Add (De.Stems (4), "us");
         end if;                         --  OX (3 & 4) handled

      end if;                 --  On V KIND

      if De.Part.V.Con = (6, 1)  then      --  Finalization correction
         Ox (3) := Add (Ox (3), " (ii)");
      end if;

   elsif (De.Part.Pofs = Num) and then (De.Part.Num.Sort = X)  then
      if De.Part.Num.Decl.Which = 1  then
         case De.Part.Num.Decl.Var is
            when 1 =>
               Ox (1) := Add (De.Stems (1), "us -a -um");
               Ox (2) := Add (De.Stems (2), "us -a -um");
               Ox (3) := Add (De.Stems (3), "i -ae -a");
               Ox (4) := Add (De.Stems (4), "");
            when 2 =>
               Ox (1) := Add (De.Stems (1), "o -ae o");
               Ox (2) := Add (De.Stems (2), "us -a -um");
               Ox (3) := Add (De.Stems (3), "i -ae -a");
               Ox (4) := Add (De.Stems (4), "");
            when 3 =>
               Ox (1) := Add (De.Stems (1), "es -es -ia");
               Ox (2) := Add (De.Stems (2), "us -a -um");
               Ox (3) := Add (De.Stems (3), "i -ae -a");
               Ox (4) := Add (De.Stems (4), "");
            when 4 =>
               Ox (1) := Add (De.Stems (1), "i -ae -a");
               Ox (2) := Add (De.Stems (2), "us -a -um");
               Ox (3) := Add (De.Stems (3), "i -ae -a");
               Ox (4) := Add (De.Stems (4), "ie (n)s");
            when others => null;
         end case;

      elsif De.Part.Num.Decl.Which = 2  then
         Ox (1) := Add (De.Stems (1), "");
         Ox (2) := Add (De.Stems (2), "us -a -um");
         Ox (3) := Add (De.Stems (3), "i -ae -a");
         Ox (4) := Add (De.Stems (4), "ie (n)s");

      end if;

   elsif (De.Part.Pofs = Num) and then (De.Part.Num.Sort = Card)  then
      if De.Part.Num.Decl.Which = 1  then
         case De.Part.Num.Decl.Var is
            when 1 =>
               Ox (1) := Add (De.Stems (1), "us");
               Ox (2) := Add (De.Stems (1), "a");
               Ox (3) := Add (De.Stems (1), "um");
            when 2 =>
               Ox (1) := Add (De.Stems (1), "o");
               Ox (2) := Add (De.Stems (1), "ae");
               Ox (3) := Add (De.Stems (1), "o");
            when 3 =>
               Ox (1) := Add (De.Stems (1), "es");
               Ox (2) := Add (De.Stems (1), "es");
               Ox (3) := Add (De.Stems (1), "ia");
            when 4 =>
               Ox (1) := Add (De.Stems (1), "i");
               Ox (2) := Add (De.Stems (1), "ae");
               Ox (3) := Add (De.Stems (1), "a");
            when others => null;
         end case;

      elsif De.Part.Num.Decl.Which = 2  then
         Ox (1) := Add (De.Stems (1), "");

      end if;

   elsif (De.Part.Pofs = Num) and then (De.Part.Num.Sort = Ord)  then
      Ox (1) := Add (De.Stems (1), "us");
      Ox (2) := Add (De.Stems (1), "a");
      Ox (3) := Add (De.Stems (1), "um");

   elsif (De.Part.Pofs = Num) and then (De.Part.Num.Sort = Dist)  then
      Ox (1) := Add (De.Stems (1), "i");
      Ox (2) := Add (De.Stems (1), "ae");
      Ox (3) := Add (De.Stems (1), "a");

   else
      Ox (1) := Add (De.Stems (1), "");
   end if;     -- On PART

   --TEXT_IO.PUT_LINE (OX (1) & "+" & OX (2) & "+" & OX (3) & "+" & OX (4));

   --  Now clean up and Output
   --  Several flags have been set which modify OX's
   if Ox (1)(1 .. 3) = "zzz"  then
      Add_Up (" - ");
   elsif Ox (1) /= Null_Ox  then
      Add_Up (Trim (Ox (1)));
   end if;
   if Ox (2)(1 .. 3) = "zzz"  then
      Add_Up (", - ");
   elsif Ox (2) /= Null_Ox  then
      Add_Up (", " & Trim (Ox (2)));
   end if;
   if Ox (3)(1 .. 3) = "zzz"  then
      Add_Up (", - ");
   elsif Ox (3)(1 .. 3) = "DEP"  then
      null;
   elsif Ox (3)(1 .. 7) = "PERFDEF"  then
      null;
   elsif Ox (3)(1 .. 5) = "BLANK"  then
      null;
   elsif Ox (3) /= Null_Ox  then
      Add_Up (", " & Trim (Ox (3)));
   end if;
   if Ox (4)(1 .. 3) = "zzz"  then
      Add_Up (", - ");
   elsif Ox (4)(1 .. 5) = "BLANK"  then
      null;
   elsif Ox (4) /= Null_Ox  then
      Add_Up (", " & Trim (Ox (4)));
   end if;

   Add_To ("  " & Part_Of_Speech_Type'Image (De.Part.Pofs) & "  ");

   if De.Part.Pofs = N  then
      --  For DICTPAGE
      if De.Part.N.Decl.Which in 1 .. 5 and
        De.Part.N.Decl.Var  in 1 .. 5
      then
         Add_To (" (" & Fst (De.Part.N.Decl.Which) & ")");
      end if;

      Add_To (" " & Gender_Type'Image (De.Part.N.Gender) & "  ");
   end if;

   if De.Part.Pofs = V then

      --  For DICTPAGE
      if De.Part.V.Con.Which in 1 .. 3 then
         if De.Part.V.Con.Var = 1 then
            Add_To (" (" & Fst (De.Part.V.Con.Which) & ")");
         elsif  De.Part.V.Con = (3, 4)  then
            Add_To (" (" & Fst (4) & ")");
         end if;
      end if;

      if De.Part.V.Kind in Gen .. Perfdef then
         Add_To (" " & Verb_Kind_Type'Image (De.Part.V.Kind) & "  ");
      end if;

   end if;

   --TEXT_IO.PUT_LINE (">>>>" & TRIM (FORM));

   return Trim (Form);

exception
   when Not_Found  =>
      return "";
   when others     =>
      return "";
end Support_Utils.Dictionary_Form;
