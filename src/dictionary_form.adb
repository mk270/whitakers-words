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

with strings_package; use strings_package;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
function dictionary_form(de : dictionary_entry) return string is

   null_ox : constant string(1..24) := (others => ' ');
   ox : array (1..4) of string (1..24) := (others => null_ox);
   form : string(1..100) := (others => ' ');

   fst: constant array (which_type range 1..5) of string(1..3) :=
     ("1st", "2nd", "3rd", "4th", "5th");

   not_found : exception;

   function add(stem, infl : string) return string is
   begin
      return head(trim(stem) & trim(infl), 24);
   end add;

   procedure add_up(factor : string) is
   begin
      form := head(trim(form) & trim(factor), 100);
   end add_up;

   procedure add_to(factor : string) is
   begin
      form := head(trim(form) & factor, 100);
   end add_to;

begin
   --DICTIONARY_ENTRY_IO.PUT(DE);
   --  So I can call with a NULL_DICTIONARY_ENTRY and not bomb
   if de = null_dictionary_entry  then
      return "";
   end if;

   if (de.part.pofs = prep)   then
      return trim(de.stems(1)) & "  " & part_of_speech_type'image(de.part.pofs) &
        "  " & case_type'image(de.part.prep.obj);
   end if;

   if de.stems(2) = null_stem_type  and
     de.stems(3) = null_stem_type  and
     de.stems(4) = null_stem_type       and not
     (((de.part.pofs = n)  and then (de.part.n.decl.which = 9))  or
     ((de.part.pofs = adj)  and then
     ((de.part.adj.decl.which = 9) or
     (de.part.adj.co = comp or de.part.adj.co = super))   ) or
     ((de.part.pofs = v)  and then (de.part.v.con = (9, 8))) or
     ((de.part.pofs = v)  and then (de.part.v.con = (9, 9))))
   then
      return trim(de.stems(1)) & "  " & part_of_speech_type'image(de.part.pofs);
      --  For UNIQUES, CONJ, INTERJ, ...
   end if;

   if de.part.pofs = n    then
      if de.part.n.decl.which = 1  then
         if de.part.n.decl.var = 1  then
            ox(1) := add(de.stems(1), "a");
            ox(2) := add(de.stems(2), "ae");
         elsif de.part.n.decl.var = 6  then
            ox(1) := add(de.stems(1), "e");
            ox(2) := add(de.stems(2), "es");
         elsif de.part.n.decl.var = 7  then
            ox(1) := add(de.stems(1), "es");
            ox(2) := add(de.stems(2), "ae");
         elsif de.part.n.decl.var = 8  then
            ox(1) := add(de.stems(1), "as");
            ox(2) := add(de.stems(2), "ae");
         end if;

      elsif de.part.n.decl.which = 2  then
         if de.part.n.decl.var = 1  then
            ox(1) := add(de.stems(1), "us");
            ox(2) := add(de.stems(2), "i");
         elsif de.part.n.decl.var = 2  then
            ox(1) := add(de.stems(1), "um");
            ox(2) := add(de.stems(2), "i");
         elsif de.part.n.decl.var = 3  then
            ox(1) := add(de.stems(1), "");
            ox(2) := add(de.stems(2), "i");
         elsif de.part.n.decl.var = 4  then
            if de.part.n.gender = n  then
               ox(1) := add(de.stems(1), "um");
            else
               ox(1) := add(de.stems(1), "us");
            end if;
            ox(2) := add(de.stems(2), "(i)");
         elsif de.part.n.decl.var = 5  then
            ox(1) := add(de.stems(1), "us");
            ox(2) := add(de.stems(2), "");
         elsif de.part.n.decl.var = 6  then
            ox(1) := add(de.stems(1), "os");
            ox(2) := add(de.stems(2), "i");
         elsif de.part.n.decl.var = 7  then
            ox(1) := add(de.stems(1), "os");
            ox(2) := add(de.stems(2), "i");
         elsif de.part.n.decl.var = 8  then
            ox(1) := add(de.stems(1), "on");
            ox(2) := add(de.stems(2), "i");
         elsif de.part.n.decl.var = 9  then
            ox(1) := add(de.stems(1), "us");
            ox(2) := add(de.stems(2), "i");
         end if;

      elsif de.part.n.decl.which = 3  then
         ox(1) := add(de.stems(1), "");
         if (de.part.n.decl.var = 7)  or
           (de.part.n.decl.var = 9)  then
            ox(2) := add(de.stems(2), "os/is");
         else
            ox(2) := add(de.stems(2), "is");
         end if;

      elsif de.part.n.decl.which = 4  then
         if de.part.n.decl.var = 1  then
            ox(1) := add(de.stems(1), "us");
            ox(2) := add(de.stems(2), "us");
         elsif de.part.n.decl.var = 2  then
            ox(1) := add(de.stems(1), "u");
            ox(2) := add(de.stems(2), "us");
         elsif de.part.n.decl.var = 3  then
            ox(1) := add(de.stems(1), "us");
            ox(2) := add(de.stems(2), "u");
         end if;

      elsif de.part.n.decl.which = 5  then
         ox(1) := add(de.stems(1), "es");
         ox(2) := add(de.stems(2), "ei");

      elsif de.part.n.decl = (9, 8)  then
         ox(1) := add(de.stems(1), ".");
         ox(2) := add(null_ox, "abb.");

      elsif de.part.n.decl = (9, 9)  then
         ox(1) := add(de.stems(1), "");
         ox(2) := add(null_ox, "undeclined");

      else
         raise not_found;
      end if;     --  N

   elsif de.part.pofs = pron    then
      if de.part.pron.decl.which = 1  then
         raise not_found;

      elsif de.part.pron.decl.which = 3  then
         ox(1) := add(de.stems(1), "ic");
         ox(2) := add(de.stems(1), "aec");
         if de.part.pron.decl.var = 1  then
            ox(3) := add(de.stems(1), "oc");
         elsif de.part.pron.decl.var = 2  then
            ox(3) := add(de.stems(1), "uc");
         end if;

      elsif de.part.pron.decl.which = 4  then
         if de.part.pron.decl.var = 1  then
            ox(1) := add(de.stems(1), "s");
            ox(2) := add(de.stems(2), "a");
            ox(3) := add(de.stems(1), "d");
         elsif de.part.pron.decl.var = 2  then
            ox(1) := add(de.stems(1), "dem");
            ox(2) := add(de.stems(2), "adem");
            ox(3) := add(de.stems(1), "dem");
         end if;

      elsif de.part.pron.decl.which = 6  then
         ox(1) := add(de.stems(1), "e");
         ox(2) := add(de.stems(1), "a");
         if de.part.pron.decl.var = 1  then
            ox(3) := add(de.stems(1), "ud");
         elsif de.part.pron.decl.var = 2  then
            ox(3) := add(de.stems(1), "um");
         end if;

      elsif de.part.adj.decl = (9, 8)  then
         ox(1) := add(de.stems(1), ".");
         ox(2) := add(null_ox, "abb.");

      elsif de.part.pron.decl = (9, 9)  then
         ox(1) := add(de.stems(1), "");
         ox(2) := add(null_ox, "undeclined");

      else
         raise not_found;
      end if;      --  PRON

   elsif de.part.pofs = adj  then

      --TEXT_IO.NEW_LINE;
      --DICTIONARY_ENTRY_IO.PUT(DE);
      --TEXT_IO.NEW_LINE;

      if de.part.adj.co = comp  then
         ox(1) := add(de.stems(1), "or");
         ox(2) := add(de.stems(1), "or");
         ox(3) := add(de.stems(1), "us");
      elsif de.part.adj.co = super  then
         ox(1) := add(de.stems(1), "mus");
         ox(2) := add(de.stems(1), "ma");
         ox(3) := add(de.stems(1), "mum");

      elsif de.part.adj.co = pos  then
         if de.part.adj.decl.which = 1  then
            if de.part.adj.decl.var = 1  then
               ox(1) := add(de.stems(1), "us");
               ox(2) := add(de.stems(2), "a");
               ox(3) := add(de.stems(2), "um");
            elsif de.part.adj.decl.var = 2  then
               ox(1) := add(de.stems(1), "");
               ox(2) := add(de.stems(2), "a");
               ox(3) := add(de.stems(2), "um");
            elsif de.part.adj.decl.var = 3  then
               ox(1) := add(de.stems(1), "us");
               ox(2) := add(de.stems(2), "a");
               ox(3) := add(de.stems(2), "um (gen -ius)");
            elsif de.part.adj.decl.var = 4  then
               ox(1) := add(de.stems(1), "");
               ox(2) := add(de.stems(2), "a");
               ox(3) := add(de.stems(2), "um");
            elsif de.part.adj.decl.var = 5  then
               ox(1) := add(de.stems(1), "us");
               ox(2) := add(de.stems(2), "a");
               ox(3) := add(de.stems(2), "ud");
            else
               raise not_found;
            end if;

         elsif de.part.adj.decl.which = 2  then
            if de.part.adj.decl.var = 1  then
               ox(1) := add(null_ox, "-");
               ox(2) := add(de.stems(1), "e");
               ox(3) := add(null_ox, "-");
            elsif de.part.adj.decl.var = 2  then
               ox(1) := add(null_ox, "-");
               ox(2) := add(null_ox, "a");
               ox(3) := add(null_ox, "-");
            elsif de.part.adj.decl.var = 3  then
               ox(1) := add(de.stems(1), "es");
               ox(2) := add(de.stems(1), "es");
               ox(3) := add(de.stems(1), "es");
            elsif de.part.adj.decl.var = 6  then
               ox(1) := add(de.stems(1), "os");
               ox(2) := add(de.stems(1), "os");
               ox(3) := add(null_ox, "-");
            elsif de.part.adj.decl.var = 7  then
               ox(1) := add(de.stems(1), "os");
               ox(2) := add(null_ox, "-");
               ox(3) := add(null_ox, "-");
            elsif de.part.adj.decl.var = 8  then
               ox(1) := add(null_ox, "-");
               ox(2) := add(null_ox, "-");
               ox(3) := add(de.stems(2), "on");
            end if;

         elsif de.part.adj.decl.which = 3  then
            if de.part.adj.decl.var = 1  then
               ox(1) := add(de.stems(1), "");
               ox(2) := add(null_ox, "(gen.)");
               ox(3) := add(de.stems(2), "is");
            elsif de.part.adj.decl.var = 2  then
               ox(1) := add(de.stems(1), "is");
               ox(2) := add(de.stems(2), "is");
               ox(3) := add(de.stems(2), "e");
            elsif de.part.adj.decl.var = 3  then
               ox(1) := add(de.stems(1), "");
               ox(2) := add(de.stems(2), "is");
               ox(3) := add(de.stems(2), "e");
            elsif de.part.adj.decl.var = 6  then
               ox(1) := add(de.stems(1), "");
               ox(2) := add(null_ox, "(gen.)");
               ox(3) := add(de.stems(2), "os");
            end if;

         elsif de.part.adj.decl = (9, 8)  then
            ox(1) := add(de.stems(1), ".");
            ox(2) := add(null_ox, "abb.");

         elsif de.part.adj.decl = (9, 9)  then
            ox(1) := add(de.stems(1), "");
            ox(2) := add(null_ox, "undeclined");

         else
            raise not_found;
         end if;

      elsif de.part.adj.co = x    then
         if de.part.adj.decl.which = 1  then
            if de.part.adj.decl.var = 1  then
               ox(1) := add(de.stems(1), "us");
               ox(2) := add(de.stems(2), "a -um");
               ox(3) := add(de.stems(3), "or -or -us");
               ox(4) := add(de.stems(4), "mus -a -um");
            elsif de.part.adj.decl.var = 2  then
               ox(1) := add(de.stems(1), "");
               ox(2) := add(de.stems(2), "a -um");
               ox(3) := add(de.stems(3), "or -or -us");
               ox(4) := add(de.stems(4), "mus -a -um");
            end if;

         elsif de.part.adj.decl.which = 3  then
            if de.part.adj.decl.var = 1  then
               ox(1) := add(de.stems(1), "");
               ox(2) := add(de.stems(2), "is (gen.)");
               ox(3) := add(de.stems(3), "or -or -us");
               ox(4) := add(de.stems(4), "mus -a -um");
            elsif de.part.adj.decl.var = 2  then
               ox(1) := add(de.stems(1), "is");
               ox(2) := add(de.stems(2), "e");
               ox(3) := add(de.stems(3), "or -or -us");
               ox(4) := add(de.stems(4), "mus -a -um");
            elsif de.part.adj.decl.var = 3  then
               ox(1) := add(de.stems(1), "");
               ox(2) := add(de.stems(2), "is -e");
               ox(3) := add(de.stems(3), "or -or -us");
               ox(4) := add(de.stems(4), "mus -a -um");
            end if;

         elsif de.part.adj.decl.which = 9  then
            ox(1) := add(de.stems(1), "");
            ox(2) := add(null_ox, "undeclined");
            ox(3) := add(de.stems(3), "or -or -us");
            ox(4) := add(de.stems(4), "mus -a -um");

         else
            raise not_found;
         end if;

      else
         raise not_found;
      end if;

   elsif (de.part.pofs = adv) and then (de.part.adv.co = x)  then
      ox(1) := add(de.stems(1), "");
      ox(2) := add(de.stems(2), "");
      ox(3) := add(de.stems(3), "");

   elsif de.part.pofs = v    then

      if de.part.v.kind = dep  then    --  all DEP
         ox(3) := add(null_ox, "DEP");  --  Flag for later use
         ox(4) := add(de.stems(4), "us sum");
         if de.part.v.con.which = 1  then
            ox(1) := add(de.stems(1), "or");
            ox(2) := add(de.stems(2), "ari");
         elsif de.part.v.con.which = 2  then
            ox(1) := add(de.stems(1), "eor");
            ox(2) := add(de.stems(2), "eri");
         elsif de.part.v.con.which = 3  then
            ox(1) := add(de.stems(1), "or");
            --  Would be wrong for 3 3, but no 3 3 DEP
            if de.part.v.con.var = 4  then
               ox(2) := add(de.stems(2), "iri");
            else
               ox(2) := add(de.stems(2), "i");
            end if;
            --            elsif DE.PART.V.CON.WHICH = 4  then   --  4th amy be 3,4 or 4,1
            --              OX(1) := ADD(DE.STEMS(1), "or");    --  depending on where in code
            --              OX(2) := ADD(DE.STEMS(2), "iri");   --  In practice there is no problem
         else
            raise not_found;
         end if;                      --  all DEP handled

      elsif de.part.v.kind = perfdef  then   --  all PERFDEF handled
         ox(1) := add(de.stems(3), "i");
         ox(2) := add(de.stems(3), "isse");
         ox(3) := add(de.stems(4), "us");
         ox(4) := null_ox;  --  Flag for later use

      elsif de.part.v.kind = impers  and then
        ((de.stems(1)(1..3) = "zzz")  and   -- Recognize as PERFDEF IMPERS
        (de.stems(2)(1..3) = "zzz"))  then
         ox(1) := add(de.stems(3), "it");
         ox(2) := add(de.stems(3), "isse");
         ox(3) := add(de.stems(4), "us est");
         --          OX(4) := ADD(NULL_OX, "PERFDEF");

      else                            --  Not DEP/PERFDEF/IMPERS

         if de.part.v.kind = impers  then
            if de.part.v.con.which = 1  then
               ox(1) := add(de.stems(1), "at");
            elsif de.part.v.con.which = 2  then
               ox(1) := add(de.stems(1), "et");
            elsif de.part.v.con.which = 3  then
               if de.part.v.con.var = 2  then
                  ox(1) := add(de.stems(1), "t");
               else
                  if de.stems(1)(trim(de.stems(1))'last) = 'i'  then
                     ox(1) := add(de.stems(1), "t");
                  else
                     ox(1) := add(de.stems(1), "it");
                  end if;
               end if;
            elsif de.part.v.con.which = 5  then
               if de.part.v.con.var = 1  then
                  ox(1) := add(de.stems(1), "est");
               end if;
            elsif de.part.v.con.which = 7  then
               if de.part.v.con.var = 1  or
                 de.part.v.con.var = 2  then
                  ox(1) := add(de.stems(1), "t");
               end if;
            end if;

         else

            --  OX 1
            if de.part.v.con.which = 2  then
               ox(1) := add(de.stems(1), "eo");

            elsif de.part.v.con.which = 5  then
               ox(1) := add(de.stems(1), "um");
            elsif de.part.v.con = (7, 2)  then
               ox(1) := add(de.stems(1), "am");
            else
               ox(1) := add(de.stems(1), "o");
            end if;                      --  /= IMPERS handled
                                         --end if;
                                         --  OX(1) handled
         end if;

         --  OX 2
         if de.part.v.con.which = 1  then
            ox(2) := add(de.stems(2), "are");
         elsif de.part.v.con.which = 2  then
            ox(2) := add(de.stems(2), "ere");
         elsif de.part.v.con.which = 3  then
            if de.part.v.con.var = 2  then
               ox(2) := add(de.stems(2), "re");
            elsif de.part.v.con.var = 3  then
               ox(2) := add(de.stems(2), "eri");
            elsif de.part.v.con.var = 4  then
               ox(2) := add(de.stems(2), "ire");
            else
               ox(2) := add(de.stems(2), "ere");
            end if;
            --            elsif DE.PART.V.CON.WHICH = 4  then
            --              OX(2) := ADD(DE.STEMS(2), "ire");
         elsif de.part.v.con.which = 5  then
            if de.part.v.con.var = 1  then
               ox(2) := add(de.stems(2), "esse");
            elsif de.part.v.con.var = 2  then
               ox(2) := add(de.stems(1), "e");  --  tricky, but it is 1
            end if;
         elsif de.part.v.con.which = 6  then
            if de.part.v.con.var = 1  then
               ox(2) := add(de.stems(2), "re");
            elsif de.part.v.con.var = 2  then
               ox(2) := add(de.stems(2), "le");
            end if;
         elsif de.part.v.con.which = 7  then
            if de.part.v.con.var = 3  then
               ox(2) := add(de.stems(2), "se");
            end if;
         elsif de.part.v.con.which = 8  then
            if de.part.v.con.var = 1  then
               ox(2) := add(de.stems(2), "are");
            elsif de.part.v.con.var = 2  then
               ox(2) := add(de.stems(2), "ere");
            elsif de.part.v.con.var = 3  then
               ox(2) := add(de.stems(2), "ere");
            elsif de.part.v.con.var = 4  then
               ox(2) := add(de.stems(2), "ire");
            else
               ox(2) := add(de.stems(2), "ere");
            end if;
         elsif de.part.v.con = (9, 8)  then
            ox(1) := add(de.stems(1), ".");
            ox(2) := add(null_ox, "abb.");
         elsif de.part.v.con = (9, 9)  then
            ox(1) := add(de.stems(1), "");
            ox(2) := add(null_ox, "undeclined");

         end if;                        --  OX(2) handled

         --  OX 3 & 4
         if de.part.v.kind = impers  then
            if (ox(3)(1..7) /= "PERFDEF")  then
               ox(3) := add(de.stems(3), "it");
            end if;
            ox(4) := add(de.stems(4), "us est");
         elsif de.part.v.kind  = semidep  then    --  Finalization correction
            ox(4) := add(de.stems(4), "us sum");
         elsif de.part.v.con = (5, 1)  then
            ox(3) := add(de.stems(3), "i");
            ox(4) := add(de.stems(4), "urus");
         elsif de.part.v.con.which = 8  then
            ox(3) := add("", "additional");
            ox(4) := add("", "forms");
         elsif de.part.v.con.which = 9  then
            ox(3) := add(null_ox, "BLANK");  --  Flag for later use
            ox(4) := add(null_ox, "BLANK");  --  Flag for later use
         else
            ox(3) := add(de.stems(3), "i");
            ox(4) := add(de.stems(4), "us");
         end if;                         --  OX(3 & 4) handled

      end if;                 --  On V KIND

      if de.part.v.con = (6, 1)  then      --  Finalization correction
         ox(3) := add(ox(3), " (ii)");
      end if;

   elsif (de.part.pofs = num) and then (de.part.num.sort = x)  then
      if de.part.num.decl.which = 1  then
         if de.part.num.decl.var = 1  then
            ox(1) := add(de.stems(1), "us -a -um");
            ox(2) := add(de.stems(2), "us -a -um");
            ox(3) := add(de.stems(3), "i -ae -a");
            ox(4) := add(de.stems(4), "");
         elsif de.part.num.decl.var = 2  then
            ox(1) := add(de.stems(1), "o -ae o");
            ox(2) := add(de.stems(2), "us -a -um");
            ox(3) := add(de.stems(3), "i -ae -a");
            ox(4) := add(de.stems(4), "");
         elsif de.part.num.decl.var = 3  then
            ox(1) := add(de.stems(1), "es -es -ia");
            ox(2) := add(de.stems(2), "us -a -um");
            ox(3) := add(de.stems(3), "i -ae -a");
            ox(4) := add(de.stems(4), "");
         elsif de.part.num.decl.var = 4  then
            ox(1) := add(de.stems(1), "i -ae -a");
            ox(2) := add(de.stems(2), "us -a -um");
            ox(3) := add(de.stems(3), "i -ae -a");
            ox(4) := add(de.stems(4), "ie(n)s");
         end if;

      elsif de.part.num.decl.which = 2  then
         ox(1) := add(de.stems(1), "");
         ox(2) := add(de.stems(2), "us -a -um");
         ox(3) := add(de.stems(3), "i -ae -a");
         ox(4) := add(de.stems(4), "ie(n)s");

      end if;

   elsif (de.part.pofs = num) and then (de.part.num.sort = card)  then
      if de.part.num.decl.which = 1  then
         if de.part.num.decl.var = 1  then
            ox(1) := add(de.stems(1), "us");
            ox(2) := add(de.stems(1), "a");
            ox(3) := add(de.stems(1), "um");
         elsif de.part.num.decl.var = 2  then
            ox(1) := add(de.stems(1), "o");
            ox(2) := add(de.stems(1), "ae");
            ox(3) := add(de.stems(1), "o");
         elsif de.part.num.decl.var = 3  then
            ox(1) := add(de.stems(1), "es");
            ox(2) := add(de.stems(1), "es");
            ox(3) := add(de.stems(1), "ia");
         elsif de.part.num.decl.var = 4  then
            ox(1) := add(de.stems(1), "i");
            ox(2) := add(de.stems(1), "ae");
            ox(3) := add(de.stems(1), "a");
         end if;

      elsif de.part.num.decl.which = 2  then
         ox(1) := add(de.stems(1), "");

      end if;

   elsif (de.part.pofs = num) and then (de.part.num.sort = ord)  then
      ox(1) := add(de.stems(1), "us");
      ox(2) := add(de.stems(1), "a");
      ox(3) := add(de.stems(1), "um");

   elsif (de.part.pofs = num) and then (de.part.num.sort = dist)  then
      ox(1) := add(de.stems(1), "i");
      ox(2) := add(de.stems(1), "ae");
      ox(3) := add(de.stems(1), "a");

   else
      ox(1) := add(de.stems(1), "");
   end if;     -- On PART

   --TEXT_IO.PUT_LINE(OX(1) & "+" & OX(2) & "+" & OX(3) & "+" & OX(4));

   --  Now clean up and output
   --  Several flags have been set which modify OX's
   if ox(1)(1..3) = "zzz"  then
      add_up(" - ");
   elsif ox(1) /= null_ox  then
      add_up(trim(ox(1)));
   end if;
   if ox(2)(1..3) = "zzz"  then
      add_up(", - ");
   elsif ox(2) /= null_ox  then
      add_up(", " & trim(ox(2)));
   end if;
   if ox(3)(1..3) = "zzz"  then
      add_up(", - ");
   elsif ox(3)(1..3) = "DEP"  then
      null;
   elsif ox(3)(1..7) = "PERFDEF"  then
      null;
   elsif ox(3)(1..5) = "BLANK"  then
      null;
   elsif ox(3) /= null_ox  then
      add_up(", " & trim(ox(3)));
   end if;
   if ox(4)(1..3) = "zzz"  then
      add_up(", - ");
   elsif ox(4)(1..5) = "BLANK"  then
      null;
   elsif ox(4) /= null_ox  then
      add_up(", " & trim(ox(4)));
   end if;

   add_to("  " & part_of_speech_type'image(de.part.pofs)& "  ");

   if de.part.pofs = n  then

      --  For DICTPAGE
      if de.part.n.decl.which in 1..5 and
        de.part.n.decl.var  in 1..5 then
         add_to(" (" & fst(de.part.n.decl.which) & ")");
      end if;

      add_to(" " & gender_type'image(de.part.n.gender) & "  ");
   end if;

   if (de.part.pofs = v)  then

      --  For DICTPAGE
      if de.part.v.con.which in 1..3 then
         if de.part.v.con.var = 1 then
            add_to(" (" & fst(de.part.v.con.which) & ")");
         elsif  de.part.v.con = (3, 4)  then
            add_to(" (" & fst(4) & ")");
         end if;
      end if;

      if  (de.part.v.kind in gen..perfdef)  then
         add_to(" " & verb_kind_type'image(de.part.v.kind) & "  ");
      end if;

   end if;

   --TEXT_IO.PUT_LINE(">>>>" & TRIM(FORM));

   return trim(form);

exception
   when not_found  =>
      return "";
   when others     =>
      return "";
end dictionary_form;
