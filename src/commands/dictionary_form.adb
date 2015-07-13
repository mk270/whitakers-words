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
function dictionary_form (de : Dictionary_Entry) return String is

   null_ox : constant String (1 .. 24) := (others => ' ');
   ox : array (1 .. 4) of String (1 .. 24) := (others => null_ox);
   form : String (1 .. 100) := (others => ' ');

   fst : constant array (Which_Type range 1 .. 5) of String (1 .. 3) :=
     ("1st", "2nd", "3rd", "4th", "5th");

   not_found : exception;

   function add (stem, infl : String) return String is
   begin
      return Head (Trim (stem) & Trim (infl), 24);
   end add;

   procedure add_up (factor : String) is
   begin
      form := Head (Trim (form) & Trim (factor), 100);
   end add_up;

   procedure add_to (factor : String) is
   begin
      form := Head (Trim (form) & factor, 100);
   end add_to;

begin
   --DICTIONARY_ENTRY_IO.PUT (DE);
   --  So I can call with a NULL_DICTIONARY_ENTRY and not bomb
   if de = Null_Dictionary_Entry  then
      return "";
   end if;

   if de.Part.pofs = Prep then
      return Trim (de.Stems (1)) & "  " &
        Part_Of_Speech_Type'Image (de.Part.pofs) &
        "  " & Case_Type'Image (de.Part.Prep.Obj);
   end if;

   if de.Stems (2) = Null_Stem_Type  and
     de.Stems (3) = Null_Stem_Type  and
     de.Stems (4) = Null_Stem_Type       and not
     (((de.Part.pofs = N)  and then (de.Part.N.Decl.Which = 9))  or
     ((de.Part.pofs = Adj)  and then
     ((de.Part.Adj.Decl.Which = 9) or
     (de.Part.Adj.Co = comp or de.Part.Adj.Co = super))) or
     ((de.Part.pofs = V)  and then (de.Part.V.Con = (9, 8))) or
     ((de.Part.pofs = V)  and then (de.Part.V.Con = (9, 9))))
   then
      return Trim (de.Stems (1)) & "  " &
        Part_Of_Speech_Type'Image (de.Part.pofs);
      --  For UNIQUES, CONJ, INTERJ, . ..
   end if;

   if de.Part.pofs = N    then
      if de.Part.N.Decl.Which = 1  then
         if de.Part.N.Decl.Var = 1  then
            ox (1) := add (de.Stems (1), "a");
            ox (2) := add (de.Stems (2), "ae");
         elsif de.Part.N.Decl.Var = 6  then
            ox (1) := add (de.Stems (1), "e");
            ox (2) := add (de.Stems (2), "es");
         elsif de.Part.N.Decl.Var = 7  then
            ox (1) := add (de.Stems (1), "es");
            ox (2) := add (de.Stems (2), "ae");
         elsif de.Part.N.Decl.Var = 8  then
            ox (1) := add (de.Stems (1), "as");
            ox (2) := add (de.Stems (2), "ae");
         end if;

      elsif de.Part.N.Decl.Which = 2  then
         if de.Part.N.Decl.Var = 1  then
            ox (1) := add (de.Stems (1), "us");
            ox (2) := add (de.Stems (2), "i");
         elsif de.Part.N.Decl.Var = 2  then
            ox (1) := add (de.Stems (1), "um");
            ox (2) := add (de.Stems (2), "i");
         elsif de.Part.N.Decl.Var = 3  then
            ox (1) := add (de.Stems (1), "");
            ox (2) := add (de.Stems (2), "i");
         elsif de.Part.N.Decl.Var = 4  then
            if de.Part.N.Gender = n  then
               ox (1) := add (de.Stems (1), "um");
            else
               ox (1) := add (de.Stems (1), "us");
            end if;
            ox (2) := add (de.Stems (2), "(i)");
         elsif de.Part.N.Decl.Var = 5  then
            ox (1) := add (de.Stems (1), "us");
            ox (2) := add (de.Stems (2), "");
         elsif de.Part.N.Decl.Var = 6  then
            ox (1) := add (de.Stems (1), "os");
            ox (2) := add (de.Stems (2), "i");
         elsif de.Part.N.Decl.Var = 7  then
            ox (1) := add (de.Stems (1), "os");
            ox (2) := add (de.Stems (2), "i");
         elsif de.Part.N.Decl.Var = 8  then
            ox (1) := add (de.Stems (1), "on");
            ox (2) := add (de.Stems (2), "i");
         elsif de.Part.N.Decl.Var = 9  then
            ox (1) := add (de.Stems (1), "us");
            ox (2) := add (de.Stems (2), "i");
         end if;

      elsif de.Part.N.Decl.Which = 3  then
         ox (1) := add (de.Stems (1), "");
         if (de.Part.N.Decl.Var = 7)  or
           (de.Part.N.Decl.Var = 9)
         then
            ox (2) := add (de.Stems (2), "os/is");
         else
            ox (2) := add (de.Stems (2), "is");
         end if;

      elsif de.Part.N.Decl.Which = 4  then
         if de.Part.N.Decl.Var = 1  then
            ox (1) := add (de.Stems (1), "us");
            ox (2) := add (de.Stems (2), "us");
         elsif de.Part.N.Decl.Var = 2  then
            ox (1) := add (de.Stems (1), "u");
            ox (2) := add (de.Stems (2), "us");
         elsif de.Part.N.Decl.Var = 3  then
            ox (1) := add (de.Stems (1), "us");
            ox (2) := add (de.Stems (2), "u");
         end if;

      elsif de.Part.N.Decl.Which = 5  then
         ox (1) := add (de.Stems (1), "es");
         ox (2) := add (de.Stems (2), "ei");

      elsif de.Part.N.Decl = (9, 8)  then
         ox (1) := add (de.Stems (1), ".");
         ox (2) := add (null_ox, "abb.");

      elsif de.Part.N.Decl = (9, 9)  then
         ox (1) := add (de.Stems (1), "");
         ox (2) := add (null_ox, "undeclined");

      else
         raise not_found;
      end if;     --  N

   elsif de.Part.pofs = Pron    then
      if de.Part.Pron.Decl.Which = 1  then
         raise not_found;

      elsif de.Part.Pron.Decl.Which = 3  then
         ox (1) := add (de.Stems (1), "ic");
         ox (2) := add (de.Stems (1), "aec");
         if de.Part.Pron.Decl.Var = 1  then
            ox (3) := add (de.Stems (1), "oc");
         elsif de.Part.Pron.Decl.Var = 2  then
            ox (3) := add (de.Stems (1), "uc");
         end if;

      elsif de.Part.Pron.Decl.Which = 4  then
         if de.Part.Pron.Decl.Var = 1  then
            ox (1) := add (de.Stems (1), "s");
            ox (2) := add (de.Stems (2), "a");
            ox (3) := add (de.Stems (1), "d");
         elsif de.Part.Pron.Decl.Var = 2  then
            ox (1) := add (de.Stems (1), "dem");
            ox (2) := add (de.Stems (2), "adem");
            ox (3) := add (de.Stems (1), "dem");
         end if;

      elsif de.Part.Pron.Decl.Which = 6  then
         ox (1) := add (de.Stems (1), "e");
         ox (2) := add (de.Stems (1), "a");
         if de.Part.Pron.Decl.Var = 1  then
            ox (3) := add (de.Stems (1), "ud");
         elsif de.Part.Pron.Decl.Var = 2  then
            ox (3) := add (de.Stems (1), "um");
         end if;

      elsif de.Part.Adj.Decl = (9, 8)  then
         ox (1) := add (de.Stems (1), ".");
         ox (2) := add (null_ox, "abb.");

      elsif de.Part.Pron.Decl = (9, 9)  then
         ox (1) := add (de.Stems (1), "");
         ox (2) := add (null_ox, "undeclined");

      else
         raise not_found;
      end if;      --  PRON

   elsif de.Part.pofs = Adj  then

      --TEXT_IO.NEW_LINE;
      --DICTIONARY_ENTRY_IO.PUT (DE);
      --TEXT_IO.NEW_LINE;

      if de.Part.Adj.Co = comp  then
         ox (1) := add (de.Stems (1), "or");
         ox (2) := add (de.Stems (1), "or");
         ox (3) := add (de.Stems (1), "us");
      elsif de.Part.Adj.Co = super  then
         ox (1) := add (de.Stems (1), "mus");
         ox (2) := add (de.Stems (1), "ma");
         ox (3) := add (de.Stems (1), "mum");

      elsif de.Part.Adj.Co = pos  then
         if de.Part.Adj.Decl.Which = 1  then
            if de.Part.Adj.Decl.Var = 1  then
               ox (1) := add (de.Stems (1), "us");
               ox (2) := add (de.Stems (2), "a");
               ox (3) := add (de.Stems (2), "um");
            elsif de.Part.Adj.Decl.Var = 2  then
               ox (1) := add (de.Stems (1), "");
               ox (2) := add (de.Stems (2), "a");
               ox (3) := add (de.Stems (2), "um");
            elsif de.Part.Adj.Decl.Var = 3  then
               ox (1) := add (de.Stems (1), "us");
               ox (2) := add (de.Stems (2), "a");
               ox (3) := add (de.Stems (2), "um (gen -ius)");
            elsif de.Part.Adj.Decl.Var = 4  then
               ox (1) := add (de.Stems (1), "");
               ox (2) := add (de.Stems (2), "a");
               ox (3) := add (de.Stems (2), "um");
            elsif de.Part.Adj.Decl.Var = 5  then
               ox (1) := add (de.Stems (1), "us");
               ox (2) := add (de.Stems (2), "a");
               ox (3) := add (de.Stems (2), "ud");
            else
               raise not_found;
            end if;

         elsif de.Part.Adj.Decl.Which = 2  then
            if de.Part.Adj.Decl.Var = 1  then
               ox (1) := add (null_ox, "-");
               ox (2) := add (de.Stems (1), "e");
               ox (3) := add (null_ox, "-");
            elsif de.Part.Adj.Decl.Var = 2  then
               ox (1) := add (null_ox, "-");
               ox (2) := add (null_ox, "a");
               ox (3) := add (null_ox, "-");
            elsif de.Part.Adj.Decl.Var = 3  then
               ox (1) := add (de.Stems (1), "es");
               ox (2) := add (de.Stems (1), "es");
               ox (3) := add (de.Stems (1), "es");
            elsif de.Part.Adj.Decl.Var = 6  then
               ox (1) := add (de.Stems (1), "os");
               ox (2) := add (de.Stems (1), "os");
               ox (3) := add (null_ox, "-");
            elsif de.Part.Adj.Decl.Var = 7  then
               ox (1) := add (de.Stems (1), "os");
               ox (2) := add (null_ox, "-");
               ox (3) := add (null_ox, "-");
            elsif de.Part.Adj.Decl.Var = 8  then
               ox (1) := add (null_ox, "-");
               ox (2) := add (null_ox, "-");
               ox (3) := add (de.Stems (2), "on");
            end if;

         elsif de.Part.Adj.Decl.Which = 3  then
            if de.Part.Adj.Decl.Var = 1  then
               ox (1) := add (de.Stems (1), "");
               ox (2) := add (null_ox, "(gen.)");
               ox (3) := add (de.Stems (2), "is");
            elsif de.Part.Adj.Decl.Var = 2  then
               ox (1) := add (de.Stems (1), "is");
               ox (2) := add (de.Stems (2), "is");
               ox (3) := add (de.Stems (2), "e");
            elsif de.Part.Adj.Decl.Var = 3  then
               ox (1) := add (de.Stems (1), "");
               ox (2) := add (de.Stems (2), "is");
               ox (3) := add (de.Stems (2), "e");
            elsif de.Part.Adj.Decl.Var = 6  then
               ox (1) := add (de.Stems (1), "");
               ox (2) := add (null_ox, "(gen.)");
               ox (3) := add (de.Stems (2), "os");
            end if;

         elsif de.Part.Adj.Decl = (9, 8)  then
            ox (1) := add (de.Stems (1), ".");
            ox (2) := add (null_ox, "abb.");

         elsif de.Part.Adj.Decl = (9, 9)  then
            ox (1) := add (de.Stems (1), "");
            ox (2) := add (null_ox, "undeclined");

         else
            raise not_found;
         end if;

      elsif de.Part.Adj.Co = x    then
         if de.Part.Adj.Decl.Which = 1  then
            if de.Part.Adj.Decl.Var = 1  then
               ox (1) := add (de.Stems (1), "us");
               ox (2) := add (de.Stems (2), "a -um");
               ox (3) := add (de.Stems (3), "or -or -us");
               ox (4) := add (de.Stems (4), "mus -a -um");
            elsif de.Part.Adj.Decl.Var = 2  then
               ox (1) := add (de.Stems (1), "");
               ox (2) := add (de.Stems (2), "a -um");
               ox (3) := add (de.Stems (3), "or -or -us");
               ox (4) := add (de.Stems (4), "mus -a -um");
            end if;

         elsif de.Part.Adj.Decl.Which = 3  then
            if de.Part.Adj.Decl.Var = 1  then
               ox (1) := add (de.Stems (1), "");
               ox (2) := add (de.Stems (2), "is (gen.)");
               ox (3) := add (de.Stems (3), "or -or -us");
               ox (4) := add (de.Stems (4), "mus -a -um");
            elsif de.Part.Adj.Decl.Var = 2  then
               ox (1) := add (de.Stems (1), "is");
               ox (2) := add (de.Stems (2), "e");
               ox (3) := add (de.Stems (3), "or -or -us");
               ox (4) := add (de.Stems (4), "mus -a -um");
            elsif de.Part.Adj.Decl.Var = 3  then
               ox (1) := add (de.Stems (1), "");
               ox (2) := add (de.Stems (2), "is -e");
               ox (3) := add (de.Stems (3), "or -or -us");
               ox (4) := add (de.Stems (4), "mus -a -um");
            end if;

         elsif de.Part.Adj.Decl.Which = 9  then
            ox (1) := add (de.Stems (1), "");
            ox (2) := add (null_ox, "undeclined");
            ox (3) := add (de.Stems (3), "or -or -us");
            ox (4) := add (de.Stems (4), "mus -a -um");

         else
            raise not_found;
         end if;

      else
         raise not_found;
      end if;

   elsif (de.Part.pofs = Adv) and then (de.Part.Adv.Co = x)  then
      ox (1) := add (de.Stems (1), "");
      ox (2) := add (de.Stems (2), "");
      ox (3) := add (de.Stems (3), "");

   elsif de.Part.pofs = V    then

      if de.Part.V.Kind = dep  then    --  all DEP
         ox (3) := add (null_ox, "DEP");  --  Flag for later use
         ox (4) := add (de.Stems (4), "us sum");
         if de.Part.V.Con.Which = 1  then
            ox (1) := add (de.Stems (1), "or");
            ox (2) := add (de.Stems (2), "ari");
         elsif de.Part.V.Con.Which = 2  then
            ox (1) := add (de.Stems (1), "eor");
            ox (2) := add (de.Stems (2), "eri");
         elsif de.Part.V.Con.Which = 3  then
            ox (1) := add (de.Stems (1), "or");
            --  Would be wrong for 3 3, but no 3 3 DEP
            if de.Part.V.Con.Var = 4  then
               ox (2) := add (de.Stems (2), "iri");
            else
               ox (2) := add (de.Stems (2), "i");
            end if;
         else
            raise not_found;
         end if;                      --  all DEP handled

      elsif de.Part.V.Kind = perfdef  then   --  all PERFDEF handled
         ox (1) := add (de.Stems (3), "i");
         ox (2) := add (de.Stems (3), "isse");
         ox (3) := add (de.Stems (4), "us");
         ox (4) := null_ox;  --  Flag for later use

      elsif de.Part.V.Kind = impers  and then
        ((de.Stems (1)(1 .. 3) = "zzz")  and   -- Recognize as PERFDEF IMPERS
        (de.Stems (2)(1 .. 3) = "zzz"))
      then
         ox (1) := add (de.Stems (3), "it");
         ox (2) := add (de.Stems (3), "isse");
         ox (3) := add (de.Stems (4), "us est");
         --          OX (4) := ADD (NULL_OX, "PERFDEF");

      else                            --  Not DEP/PERFDEF/IMPERS

         if de.Part.V.Kind = impers  then
            if de.Part.V.Con.Which = 1  then
               ox (1) := add (de.Stems (1), "at");
            elsif de.Part.V.Con.Which = 2  then
               ox (1) := add (de.Stems (1), "et");
            elsif de.Part.V.Con.Which = 3  then
               if de.Part.V.Con.Var = 2  then
                  ox (1) := add (de.Stems (1), "t");
               else
                  if de.Stems (1)(Trim (de.Stems (1))'Last) = 'i'  then
                     ox (1) := add (de.Stems (1), "t");
                  else
                     ox (1) := add (de.Stems (1), "it");
                  end if;
               end if;
            elsif de.Part.V.Con.Which = 5  then
               if de.Part.V.Con.Var = 1  then
                  ox (1) := add (de.Stems (1), "est");
               end if;
            elsif de.Part.V.Con.Which = 7  then
               if de.Part.V.Con.Var = 1  or
                 de.Part.V.Con.Var = 2
               then
                  ox (1) := add (de.Stems (1), "t");
               end if;
            end if;

         else

            --  OX 1
            if de.Part.V.Con.Which = 2  then
               ox (1) := add (de.Stems (1), "eo");

            elsif de.Part.V.Con.Which = 5  then
               ox (1) := add (de.Stems (1), "um");
            elsif de.Part.V.Con = (7, 2)  then
               ox (1) := add (de.Stems (1), "am");
            else
               ox (1) := add (de.Stems (1), "o");
            end if;
         end if;

         --  OX 2
         if de.Part.V.Con.Which = 1  then
            ox (2) := add (de.Stems (2), "are");
         elsif de.Part.V.Con.Which = 2  then
            ox (2) := add (de.Stems (2), "ere");
         elsif de.Part.V.Con.Which = 3  then
            if de.Part.V.Con.Var = 2  then
               ox (2) := add (de.Stems (2), "re");
            elsif de.Part.V.Con.Var = 3  then
               -- Special case for fio, fieri: it follows the usual
               -- conjugation everywhere except for present infinitive
               if Trim (de.Stems (2)) = "f" then
                  ox (2) := add (de.Stems (2), "ieri");
               else
                  ox (2) := add (de.Stems (2), "eri");
               end if;
            elsif de.Part.V.Con.Var = 4  then
               ox (2) := add (de.Stems (2), "ire");
            else
               ox (2) := add (de.Stems (2), "ere");
            end if;
            --            elsif DE.PART.V.CON.WHICH = 4  then
            --              OX (2) := ADD (DE.STEMS (2), "ire");
         elsif de.Part.V.Con.Which = 5  then
            if de.Part.V.Con.Var = 1  then
               ox (2) := add (de.Stems (2), "esse");
            elsif de.Part.V.Con.Var = 2  then
               ox (2) := add (de.Stems (1), "e");  --  tricky, but it is 1
            end if;
         elsif de.Part.V.Con.Which = 6  then
            if de.Part.V.Con.Var = 1  then
               ox (2) := add (de.Stems (2), "re");
            elsif de.Part.V.Con.Var = 2  then
               ox (2) := add (de.Stems (2), "le");
            end if;
         elsif de.Part.V.Con.Which = 7  then
            if de.Part.V.Con.Var = 3  then
               ox (2) := add (de.Stems (2), "se");
            end if;
         elsif de.Part.V.Con.Which = 8  then
            if de.Part.V.Con.Var = 1  then
               ox (2) := add (de.Stems (2), "are");
            elsif de.Part.V.Con.Var = 2  then
               ox (2) := add (de.Stems (2), "ere");
            elsif de.Part.V.Con.Var = 3  then
               ox (2) := add (de.Stems (2), "ere");
            elsif de.Part.V.Con.Var = 4  then
               ox (2) := add (de.Stems (2), "ire");
            else
               ox (2) := add (de.Stems (2), "ere");
            end if;
         elsif de.Part.V.Con = (9, 8)  then
            ox (1) := add (de.Stems (1), ".");
            ox (2) := add (null_ox, "abb.");
         elsif de.Part.V.Con = (9, 9)  then
            ox (1) := add (de.Stems (1), "");
            ox (2) := add (null_ox, "undeclined");

         end if;                        --  OX (2) handled

         --  OX 3 & 4
         if de.Part.V.Kind = impers  then
            if ox (3)(1 .. 7) /= "PERFDEF" then
               ox (3) := add (de.Stems (3), "it");
            end if;
            ox (4) := add (de.Stems (4), "us est");
         elsif de.Part.V.Kind  = semidep  then    --  Finalization correction
            ox (4) := add (de.Stems (4), "us sum");
         elsif de.Part.V.Con = (5, 1)  then
            ox (3) := add (de.Stems (3), "i");
            ox (4) := add (de.Stems (4), "urus");
         elsif de.Part.V.Con.Which = 8  then
            ox (3) := add ("", "additional");
            ox (4) := add ("", "forms");
         elsif de.Part.V.Con.Which = 9  then
            ox (3) := add (null_ox, "BLANK");  --  Flag for later use
            ox (4) := add (null_ox, "BLANK");  --  Flag for later use
         else
            ox (3) := add (de.Stems (3), "i");
            ox (4) := add (de.Stems (4), "us");
         end if;                         --  OX (3 & 4) handled

      end if;                 --  On V KIND

      if de.Part.V.Con = (6, 1)  then      --  Finalization correction
         ox (3) := add (ox (3), " (ii)");
      end if;

   elsif (de.Part.pofs = Num) and then (de.Part.Num.Sort = x)  then
      if de.Part.Num.Decl.Which = 1  then
         if de.Part.Num.Decl.Var = 1  then
            ox (1) := add (de.Stems (1), "us -a -um");
            ox (2) := add (de.Stems (2), "us -a -um");
            ox (3) := add (de.Stems (3), "i -ae -a");
            ox (4) := add (de.Stems (4), "");
         elsif de.Part.Num.Decl.Var = 2  then
            ox (1) := add (de.Stems (1), "o -ae o");
            ox (2) := add (de.Stems (2), "us -a -um");
            ox (3) := add (de.Stems (3), "i -ae -a");
            ox (4) := add (de.Stems (4), "");
         elsif de.Part.Num.Decl.Var = 3  then
            ox (1) := add (de.Stems (1), "es -es -ia");
            ox (2) := add (de.Stems (2), "us -a -um");
            ox (3) := add (de.Stems (3), "i -ae -a");
            ox (4) := add (de.Stems (4), "");
         elsif de.Part.Num.Decl.Var = 4  then
            ox (1) := add (de.Stems (1), "i -ae -a");
            ox (2) := add (de.Stems (2), "us -a -um");
            ox (3) := add (de.Stems (3), "i -ae -a");
            ox (4) := add (de.Stems (4), "ie (n)s");
         end if;

      elsif de.Part.Num.Decl.Which = 2  then
         ox (1) := add (de.Stems (1), "");
         ox (2) := add (de.Stems (2), "us -a -um");
         ox (3) := add (de.Stems (3), "i -ae -a");
         ox (4) := add (de.Stems (4), "ie (n)s");

      end if;

   elsif (de.Part.pofs = Num) and then (de.Part.Num.Sort = card)  then
      if de.Part.Num.Decl.Which = 1  then
         if de.Part.Num.Decl.Var = 1  then
            ox (1) := add (de.Stems (1), "us");
            ox (2) := add (de.Stems (1), "a");
            ox (3) := add (de.Stems (1), "um");
         elsif de.Part.Num.Decl.Var = 2  then
            ox (1) := add (de.Stems (1), "o");
            ox (2) := add (de.Stems (1), "ae");
            ox (3) := add (de.Stems (1), "o");
         elsif de.Part.Num.Decl.Var = 3  then
            ox (1) := add (de.Stems (1), "es");
            ox (2) := add (de.Stems (1), "es");
            ox (3) := add (de.Stems (1), "ia");
         elsif de.Part.Num.Decl.Var = 4  then
            ox (1) := add (de.Stems (1), "i");
            ox (2) := add (de.Stems (1), "ae");
            ox (3) := add (de.Stems (1), "a");
         end if;

      elsif de.Part.Num.Decl.Which = 2  then
         ox (1) := add (de.Stems (1), "");

      end if;

   elsif (de.Part.pofs = Num) and then (de.Part.Num.Sort = ord)  then
      ox (1) := add (de.Stems (1), "us");
      ox (2) := add (de.Stems (1), "a");
      ox (3) := add (de.Stems (1), "um");

   elsif (de.Part.pofs = Num) and then (de.Part.Num.Sort = dist)  then
      ox (1) := add (de.Stems (1), "i");
      ox (2) := add (de.Stems (1), "ae");
      ox (3) := add (de.Stems (1), "a");

   else
      ox (1) := add (de.Stems (1), "");
   end if;     -- On PART

   --TEXT_IO.PUT_LINE (OX (1) & "+" & OX (2) & "+" & OX (3) & "+" & OX (4));

   --  Now clean up and Output
   --  Several flags have been set which modify OX's
   if ox (1)(1 .. 3) = "zzz"  then
      add_up (" - ");
   elsif ox (1) /= null_ox  then
      add_up (Trim (ox (1)));
   end if;
   if ox (2)(1 .. 3) = "zzz"  then
      add_up (", - ");
   elsif ox (2) /= null_ox  then
      add_up (", " & Trim (ox (2)));
   end if;
   if ox (3)(1 .. 3) = "zzz"  then
      add_up (", - ");
   elsif ox (3)(1 .. 3) = "DEP"  then
      null;
   elsif ox (3)(1 .. 7) = "PERFDEF"  then
      null;
   elsif ox (3)(1 .. 5) = "BLANK"  then
      null;
   elsif ox (3) /= null_ox  then
      add_up (", " & Trim (ox (3)));
   end if;
   if ox (4)(1 .. 3) = "zzz"  then
      add_up (", - ");
   elsif ox (4)(1 .. 5) = "BLANK"  then
      null;
   elsif ox (4) /= null_ox  then
      add_up (", " & Trim (ox (4)));
   end if;

   add_to ("  " & Part_Of_Speech_Type'Image (de.Part.pofs)& "  ");

   if de.Part.pofs = N  then
      --  For DICTPAGE
      if de.Part.N.Decl.Which in 1 .. 5 and
        de.Part.N.Decl.Var  in 1 .. 5
      then
         add_to (" (" & fst (de.Part.N.Decl.Which) & ")");
      end if;

      add_to (" " & Gender_Type'Image (de.Part.N.Gender) & "  ");
   end if;

   if de.Part.pofs = V then

      --  For DICTPAGE
      if de.Part.V.Con.Which in 1 .. 3 then
         if de.Part.V.Con.Var = 1 then
            add_to (" (" & fst (de.Part.V.Con.Which) & ")");
         elsif  de.Part.V.Con = (3, 4)  then
            add_to (" (" & fst (4) & ")");
         end if;
      end if;

      if de.Part.V.Kind in gen .. perfdef then
         add_to (" " & Verb_Kind_Type'Image (de.Part.V.Kind) & "  ");
      end if;

   end if;

   --TEXT_IO.PUT_LINE (">>>>" & TRIM (FORM));

   return Trim (form);

exception
   when not_found  =>
      return "";
   when others     =>
      return "";
end dictionary_form;
