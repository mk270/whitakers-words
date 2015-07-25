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
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Inflections_Package; use Latin_Utils.Inflections_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
-- with Support_Utils.Line_Stuff; use Support_Utils.Line_Stuff;
procedure check is
   use Latin_Utils.Inflections_Package.Integer_IO;
   use Text_IO;
   use Dictionary_Entry_IO;
   use Part_Entry_IO;
   use Kind_Entry_IO;

   input, output : File_Type;
   de : Dictionary_Entry;

   s : String (1 .. 400) := (others => ' ');
   blank_line : constant String (1 .. 400) := (others => ' ');
   ll, last : Integer := 0;

   line_number : Integer := 0;
   number : Integer := 0;

   function is_vowel (c : Character) return Boolean is
   begin
      if (Lower_Case (c) = 'a') or
        (Lower_Case (c) = 'e') or
        (Lower_Case (c) = 'i') or
        (Lower_Case (c) = 'o') or
        (Lower_Case (c) = 'u')
      then
         return True;
      else
         return False;
      end if;
   end is_vowel;

   function has_punctuation (s : String) return Boolean is
      j : Integer := 0;
   begin
      for i in reverse s'Range  loop
         j := i;
         exit when s (i) /= ' ';
      end loop;
      for i in s'First .. j - 1  loop
         if Lower_Case (s (i)) not in 'a' .. 'z'  then
            return True;
         end if;
      end loop;
      return False;
   end has_punctuation;

   function bk (s : Stem_Type) return Boolean is
   begin
      if s = Null_Stem_Type  then
         return True;
      else
         return False;
      end if;
   end bk;

   function len (st : Stem_Type) return Integer is
      l : Integer := 0;
   begin
      for i in st'Range  loop
         exit when st (i) = ' ';
         l := l + 1;
      end loop;
      return l;
   end len;

   procedure Verify_Stems is
      sts : constant Stems_Type := de.Stems;
      pt  : constant Part_Entry := de.Part;
      --Mean : Meaning_Type := de.Mean;

      procedure prob (message : String) is
      begin
         number := number + 1;
         Put (output, "LINE"); Put (output, line_number);
         Put (output, "  " & message & "  ");
         Part_Entry_IO.Put (output, pt);
         New_Line (output);
         Put_Line (output, s (1 .. last));
      end prob;

   begin

      --  Process parts
      if pt.pofs = N  then

         --  Check that there are two and only two stems for a noun
         if (bk (sts (1)) or
           bk (sts (2)) or
           not bk (sts (3)) or
           not bk (sts (4)))  and then
           not (pt.N.Decl = (9, 9) or pt.N.Decl = (9, 8))
         then  --  Undeclined
            prob ("    EXPECTED  exactly 2  NOUN STEMS");
         end if;

         --  Check that the stems are the same when expected
         if pt.N.Decl = (1, 1) or
           pt.N.Decl = (2, 1) or
           pt.N.Decl = (2, 2) or
           pt.N.Decl = (4, 1) or
           pt.N.Decl = (5, 1)
         then
            if sts (1) /= sts (2)  and then
              ((sts (1) /= ZZZ_Stem) and (sts (2) /= ZZZ_Stem))
            then
               prob ("    EXPECTED IDENTICAL NOUN STEMS");
            end if;
         end if;

         --  Check that the stems progress as expected
         if pt.N.Decl = (1, 2)   and then
           ((sts (1) /= ZZZ_Stem)  or (sts (2) /= ZZZ_Stem))
         then
            if len (sts (1)) >= 3                           and then
              sts (1)(len (sts (1)) .. len (sts (1))) /= "r"  and then
              sts (2)(len (sts (2)) .. len (sts (2))) /= "r"
            then
               prob ("    EXPECTED  r and r   (1, 2)  NOUN STEMS");
            end if;
         end if;

         --  Check that N 2 4 has the 'i's
         if pt.N.Decl = (2, 4)    then
            if (len (sts (1)) >= 3) and then
              (((sts (1) /= ZZZ_Stem)   and
              (sts (1)(len (sts (1)) .. len (sts (1))) /= "i"))  or else
              ((sts (1) /= ZZZ_Stem)   and
              (sts (2)(len (sts (2)) .. len (sts (2))) /= "i")))
            then
               prob ("    EXPECTED  i and i   (2, 4)  NOUN STEMS");
            end if;
         end if;

         --   N 3 1

         --  Check N 3 1 er is M/C
         if pt.N.Decl = (3, 1)     then
            if (len (sts (1)) >= 3                           and then
              sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "er")  and then
              (pt.N.Gender /= M  and   pt.N.Gender /= C)
            then
               prob ("    EXPECTED  -er M/C for  N 3 1 NOUN STEMS");
            end if;
         end if;

         --  Check er -> er
         if pt.N.Decl = (3, 1) and
            pt.N.Gender = M
         then
            if (len (sts (1)) >= 3                           and then
              sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "er")  and then
              ((sts (2)(len (sts (2)) - 1 .. len (sts (2))) /= "er")         or
              (sts (1)(1 .. len (sts (1)) - 2) /=
               sts (2)(1 .. len (sts (2)) - 2)))
            then
               prob ("    EXPECTED  er -> er   N 3 1 NOUN STEMS");
            end if;
         end if;

         --  Check N 3 1 or is M
         if pt.N.Decl = (3, 1)     then
            if (len (sts (1)) >= 3                           and then
              sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "or")  and then
              pt.N.Gender /= M
            then
               prob ("    EXPECTED  -or M for  N 3 1 NOUN STEMS");
            end if;
         end if;

         --  Check or -> or
         if pt.N.Decl = (3, 1) and
            pt.N.Gender = M
         then
            if (len (sts (1)) >= 3                           and then
              sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "or")  and then
              ((sts (2)(len (sts (2)) - 1 .. len (sts (2))) /= "or")         or
              (sts (1)(1 .. len (sts (1)) - 2) /=
               sts (2)(1 .. len (sts (2)) - 2)))
            then
               prob ("    EXPECTED  or -> or   and S1=S2   N 3 1 NOUN STEMS");
            end if;
         end if;

         --  Check N 3 1 -o is M except -do, -go, -io
         if pt.N.Decl = (3, 1) and then
           (sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "o")   and then
           pt.N.Gender /= M
         then
            if (len (sts (1)) >= 3)                          and then
              ((sts (1)(len (sts (1)) - 2 .. len (sts (1))) /= "do")   and
              (sts (1)(len (sts (1)) - 2 .. len (sts (1))) /= "go")   and
              (sts (1)(len (sts (1)) - 2 .. len (sts (1))) /= "io"))  and then
              pt.N.Gender /= F
            then
               prob ("    EXPECTED  N 3 1 -o M excpt -do, -go, -io F");
            end if;
         end if;

         --  Check io -> ion
         if pt.N.Decl = (3, 1) and
            pt.N.Gender = F
         then
            if len (sts (1)) >= 3                           and then
              sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "io"  and then
              sts (2)(len (sts (2)) - 2 .. len (sts (2))) /= "ion"
            then
               prob ("    EXPECTED  io -> ion  NOUN STEMS");
            elsif len (sts (1)) >= 3                            and then
              sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "io"   and then
              sts (2)(len (sts (2)) - 2 .. len (sts (2))) = "ion"
            then
               if sts (1)(1 .. len (sts (1)) - 1) /=
                  sts (2)(1 .. len (sts (2)) - 2)
               then
                  prob ("    SUSPECT  io - ion  NOUN STEMS  misspelling");
               end if;
            end if;
         end if;

         if pt.N.Decl = (3, 1) and
           pt.N.Gender =  F    and
           len (sts (1)) >= 3
         then
            if sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "do"  and then
              sts (2)(len (sts (2)) - 2 .. len (sts (2))) /= "din"
            then
               prob ("    EXPECTED  do -> din  NOUN  3 1 STEMS");
            end if;
         elsif sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "do"   and then
           sts (2)(len (sts (2)) - 2 .. len (sts (2))) = "din"
         then
            if sts (1)(1 .. len (sts (1)) - 1) /=
               sts (2)(1 .. len (sts (2)) - 2)
            then
               prob ("    SUSPECT  do - din  NOUN STEMS  misspelling");
            end if;
         end if;

         --  Check as -> at/ad
         if pt.N.Decl = (3, 1) and
            pt.N.Gender = F
         then
            if len (sts (1)) >= 3                           and then
              sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "as"  and then
              (sts (2)(len (sts (2)) - 1 .. len (sts (2))) /= "at"    and
              sts (2)(len (sts (2)) - 1 .. len (sts (2))) /= "ad")
            then
               prob ("    EXPECTED  as -> at/ad   NOUN STEMS");
            end if;
         end if;

         --  Check -a, -at is N 3 2 N
         if pt.N.Decl.Which  = 3   then
            if (len (sts (1)) >= 3)                          and then
              (sts (1)(len (sts (1)) .. len (sts (1))) = "a")  and then
              ((sts (2)(len (sts (2)) - 1 .. len (sts (2))) /= "at")    or
              (pt.N.Decl /= (3, 2)))
            then
               prob ("    EXPECTED  a -> at   NOUN STEMS  to be N 3 2 N");
            end if;
         end if;

         --  Check es/is -> I-stem
         if pt.N.Decl.Which = 3 and
           (pt.N.Gender = M or pt.N.Gender = F or pt.N.Gender = C)
         then
            if len (sts (1)) >= 3                           and then
              (sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "es"  or
              sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "is")    and then
              sts (1)(1 .. len (sts (1)) - 2) = sts (2)(1 .. len (sts (2)))
            then
               if pt.N.Decl.Var /= 3  and pt.N.Decl.Var /= 9 then
                  prob ("    EXPECTED  es/is I-stem (3, 3)");
               end if;
            end if;
         end if;

         --  Check is I-stem -es is F                 G&L 58
         if pt.N.Decl = (3, 3) then
            if (len (sts (1)) >= 3)                           and then
              (sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "es")
            then
               if pt.N.Gender /= F  then
                  prob ("    EXPECTED  -es to be F  N 3 3");
               end if;
            end if;
         end if;

         --  Check ns/rs -> I-stem
         if pt.N.Decl.Which = 3 and
           (pt.N.Gender = M or pt.N.Gender = F or pt.N.Gender = C)
         then
            if len (sts (1)) >= 3                           and then
              (sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "ns"  or
              sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "rs")
            then
               if pt.N.Decl.Var /= 3  then
                  prob ("    EXPECTED  ns/rs M/F I-stem (3, 3)");
               end if;
            end if;
         end if;

         --  Check al/e  -> I-stem
         if pt.N.Decl.Which = 3 and
           (pt.N.Gender = N)
         then
            if len (sts (1)) >= 3                           and then
              (sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "al"  or
              sts (1)(len (sts (1)) .. len (sts (1))) = "e")
            then
               if pt.N.Decl.Var /= 4  then
                  prob ("    EXPECTED  al/e neuter I-stem (3, 4)");
               end if;
            end if;
         end if;

         --  Check N 3 starts the same
         if pt.N.Decl.Which = 3  then
            if len (sts (1)) >= 4                           and then
              (sts (1) /= ZZZ_Stem and sts (2) /= ZZZ_Stem)  and then
              (sts (1)(len (sts (1)) .. len (sts (1)) - 1) /=
              sts (2)(len (sts (1)) .. len (sts (1)) - 1))
            then
               prob ("    EXPECTED  1st and 2nd stems similiar for N 3 X");
            end if;
         end if;

         --  Check N 3 GENDER
         if (pt.N.Decl = (3, 1))  and  (pt.N.Gender = N)   then
            prob ("    EXPECTED  N 3 1 not to be N");
         elsif (pt.N.Decl = (3, 2))  and  (pt.N.Gender /= N)   then
            prob ("    EXPECTED  N 3 2  to be N");
         elsif (pt.N.Decl = (3, 3))  and  (pt.N.Gender = N)   then
            prob ("    EXPECTED  N 3 3 not to be N");
         elsif (pt.N.Decl = (3, 4))  and  (pt.N.Gender /= N)   then
            prob ("    EXPECTED  N 3 4 to be N");
         end if;

      elsif pt.pofs = Pron  then
         null;

      elsif pt.pofs = Adj  then

         --  Can only check consistency if more than one stem,
         --    CO /= COMP | SUPER
         if (pt.Adj.Co = Pos or pt.Adj.Co = X)  and
           (pt.Adj.Decl /= (9, 9)   and
           pt.Adj.Decl /= (9, 8))
         then

            --  Check that the stems are the same when expected
            if (
              (pt.Adj.Decl = (3, 2)) or
              ((pt.Adj.Decl = (3, 3)) and then
              (sts (1)(len (sts (1)) .. len (sts (1))) /= "r"))) and then

              ((sts (1) /= ZZZ_Stem) and (sts (2) /= ZZZ_Stem))
            then
               if sts (1) /= sts (2)  then
                  prob ("    EXPECTED IDENTICAL ADJ STEMS");
               end if;
            end if;

            --  Check that the stems progress as expected
            if pt.Adj.Decl = (1, 2)   then
               if len (sts (1)) >= 3                           and then
                 sts (1)(len (sts (1)) .. len (sts (1))) /= "r"  and then
                 sts (2)(len (sts (2)) .. len (sts (2))) /= "r"
               then
                  prob ("    EXPECTED  r and r   (1, 2)  ADJ STEMS");
               end if;
            end if;

            if pt.Adj.Decl = (3, 1)   then
               if len (sts (1)) >= 3                           and then
                 sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "ns"  and then
                 sts (2)(len (sts (2)) - 1 .. len (sts (2))) /= "nt"
               then
                  prob ("    EXPECTED  ns -> nt  (3, 1)  ADJ STEMS");
               end if;
            end if;

            if pt.Adj.Decl.Which = 3   then
               if len (sts (1)) >= 3                           and then
                 sts (1)(len (sts (1)) - 1 .. len (sts (1))) = "er"  and then
                 pt.Adj.Decl /= (3, 3)
               then
                  prob ("    EXPECTED  ADJ 3 with -er to be (3, 3)");
               end if;
            end if;

            if pt.Adj.Decl = (3, 1)   then
               if len (sts (1)) > len (sts (2)) then
                  prob ("    EXPECTED ADJ (3, 1)  1st stem to be shorter");
               end if;
               if sts (1)(1 .. len (sts (1)) - 1) /=
                 sts (2)(1 .. len (sts (1)) - 1)
               then
                  prob (
                    "    EXPECTED ADJ (3, 1)  stems to agree in first letters");
               end if;
            end if;

         end if;

         --  General ADJ things

         --  Check that ADJ 9 is POS
         if Pt.Adj.Decl.Which = 9 and
           Pt.Adj.Co /= Pos
         then
            Prob ("    EXPECTED  ADJ 9 to be POS");
         end if;

         --  Check that ADJ 9 has 1 stem
         if (Pt.Adj.Decl.Which = 9)    and
           (Sts (2) /= Null_Stem_Type)
         then
            Prob ("    EXPECTED  ADJ 9 have just 1 stem");
         end if;

         --  Check that there are two and only two stems if POS
         if Pt.Adj.Co = Pos  and Pt.Adj.Decl /= (9, 9) and
            Pt.Adj.Decl /= (9, 8)
         then
            if Sts (3) /= Null_Stem_Type  or
               Sts (4) /= Null_Stem_Type
            then
               Prob ("    EXPECTED  exactly 2  POS ADJ  STEMS");
            end if;
         end if;

         --  Check that there are more than two stems if X
         if Pt.Adj.Co = X    then
            if Sts (3) = Null_Stem_Type  or
               Sts (4) = Null_Stem_Type
            then
               Prob ("    EXPECTED  4  X  ADJ  STEMS");
            end if;
         end if;

         --  Check that COMP ends in i, mostly
         if Pt.Adj.Co = X   then
            if Sts (3) /= Null_Stem_Type  and
               Sts (3) /= ZZZ_Stem
            then
               if Sts (3)(Len (Sts (3))) /= 'i'  then
                  Prob ("    EXPECTED  ADJ  STEM 3 to end in 'i'");
               elsif Sts (3)(1 .. Len (Sts (3)) - 1) /=
                     Sts (2)(1 .. Len (Sts (2)))
               then
                  Prob ("    EXPECTED  ADJ  STEM 3  = STEM 2 & 'i'");
               end if;
            end if;
         end if;

         --  Check that SUPER ends in issi, mostly
         if Pt.Adj.Co = X    then
            if ((Len (Sts (3)) > 3) and  (Len (Sts (4)) > 4))    and then
              ((Sts (3) /= Null_Stem_Type  and
              Sts (3) /= ZZZ_Stem)    and then
              (Sts (4) /= Null_Stem_Type  and
              Sts (4) /= ZZZ_Stem))
            then
               if Sts (3)(Len (Sts (3)) - 3 .. Len (Sts (3))) = "cili"  then
                  if Sts (4)(Len (Sts (4)) - 4 .. Len (Sts (4))) /= "cilli" then
                     Prob ("    EXPECTED  'cil' ADJ  STEM 4 to end in 'cilli'");
                  end if;
               elsif Sts (3)(Len (Sts (3)) - 3 .. Len (Sts (3))) = "mili"  then
                  if Sts (4)(Len (Sts (4)) - 4 .. Len (Sts (4))) /= "milli" then
                     Prob ("    EXPECTED  'mil' ADJ  STEM 4 to end in 'milli'");
                  end if;
               elsif Sts (3)(Len (Sts (3)) - 1 .. Len (Sts (3))) = "ri"  then
                  if Sts (4)(Len (Sts (4)) - 2 .. Len (Sts (4))) /= "rri" then
                     Prob ("    EXPECTED  'r' ADJ  STEM 4 to end in 'rri'");
                  end if;
               elsif Sts (4)(Len (Sts (4)) - 3 .. Len (Sts (4))) /= "issi"  then
                  Prob ("    EXPECTED  ADJ  STEM 4 to end in 'issi'");
               elsif Sts (4)(1 .. Len (Sts (4)) - 3) /=
                     Sts (3)(1 .. Len (Sts (3)))
               then
                  Prob ("    EXPECTED  ADJ  STEM 4 to be STEM 3 & 'ssi'");
               elsif Sts (4)(1 .. Len (Sts (4)) - 4) /=
                     Sts (2)(1 .. Len (Sts (2)))
               then
                  Prob ("    EXPECTED  ADJ  STEM 4 to be STEM 2 & 'issi'");
               end if;
            end if;
         end if;

         --  Check that COMP and SUPER are (0, 0)
         if ((Pt.Adj.Co = Comp) or
           (Pt.Adj.Co = Super))     and then
           (Pt.Adj.Decl /= (0, 0))
         then
            Prob ("    EXPECTED  ADJ  COMP/SUPER to be (0, 0)");
         end if;

         --  Check that COMP and SUPER have only one stem
         if ((Pt.Adj.Co = Comp) or
           (Pt.Adj.Co = Super))     and then
           (Sts (2) /= Null_Stem_Type)
         then
            Prob ("    EXPECTED  ADJ  COMP/SUPER to have only one stem");
         end if;

      elsif Pt.Pofs = Adv  then

         --  Can only check consistency if more than one stem,
         --    CO /= COMP | SUPER
         if Pt.Adv.Co = Pos or Pt.Adv.Co = X  then

            --  Check that there are two and only two stems if POS
            if Pt.Adv.Co = Pos     then
               if Sts (2) /= Null_Stem_Type  or
                  Sts (3) /= Null_Stem_Type
               then
                  Prob ("    EXPECTED  exactly 1  POS ADV  STEM ");
               end if;
            end if;

            --  Check that there are more than two stems if X
            if Pt.Adv.Co = X   then
               if Sts (2) = Null_Stem_Type  or
                  Sts (3) = Null_Stem_Type
               then
                  Prob ("    EXPECTED  3  X  ADV  STEMS");
               end if;
            end if;

         end if;

         --  Check that COMP  ends in ius, mostly
         if Pt.Adv.Co = X        then
            if (Sts (2) /= Null_Stem_Type  and
              Sts (2) /= ZZZ_Stem)     and then
              (Sts (2)(Len (Sts (2)) - 2 .. Len (Sts (2))) /= "ius")
            then
               Prob ("    EXPECTED  ADV  STEM 2 to end in 'ius'");
            end if;
         end if;

         --  Check that SUPER ends in ime, mostly
         if Pt.Adv.Co = X        then
            if (Sts (3) /= Null_Stem_Type  and
              Sts (3) /= ZZZ_Stem)     and then
              (Sts (3)(Len (Sts (3)) - 2 .. Len (Sts (3))) /= "ime")
            then
               Prob ("    EXPECTED  ADV  STEM 3 to end in 'ime'");
            end if;
         end if;

         --  Check that SUPER ends in issime, mostly
         if Pt.Adv.Co = X      then
            if ((Len (Sts (2)) > 4) and  (Len (Sts (3)) > 6))    and then
              ((Sts (2) /= Null_Stem_Type  and
              Sts (2) /= ZZZ_Stem)    and then
              (Sts (3) /= Null_Stem_Type  and
              Sts (3) /= ZZZ_Stem))
            then
               if Sts (2)(Len (Sts (2)) - 5 .. Len (Sts (2))) = "cilius"  then
                  if Sts (3)(Len (Sts (3)) - 6 ..
                             Len (Sts (3))) /= "cillime"
                  then
                     Prob (
                       "    EXPECTED  'cil' ADV  STEM 3 to end in 'cillime'");
                  end if;
               elsif Sts (2)(Len (Sts (2)) - 5 ..
                             Len (Sts (2))) = "milius"
               then
                  if Sts (3)(Len (Sts (3)) - 6 ..
                             Len (Sts (3))) /= "millime"
                  then
                     Prob (
                       "    EXPECTED  'mil' ADV  STEM 3 to end in 'millime'");
                  end if;
               elsif Sts (2)(Len (Sts (2)) - 3 .. Len (Sts (2))) = "rius" then
                  if Sts (3)(Len (Sts (3)) - 4 .. Len (Sts (3))) /= "rrime" then
                     Prob ("    EXPECTED  'r' ADV  STEM 3 to end in 'rrime'");
                  end if;
               elsif Sts (3)(Len (Sts (3)) - 5 ..
                             Len (Sts (3))) /= "issime"
               then
                  Prob ("    EXPECTED  ADV  STEM 3 to end in 'issime'");
               end if;
            end if;
         end if;

      elsif Pt.Pofs = V  then

         --  Check that V 9 9 has ony one stem
         if Pt.V.Con = (9, 9)    then
            if Sts (2) /= Null_Stem_Type  or
               Sts (3) /= Null_Stem_Type  or
               Sts (4) /= Null_Stem_Type
            then
               Prob ("    EXPECTED  exactly 1  V (9, 9)  STEM ");
            end if;

         else

            --  Check to see no first verb stem has lingering 'o'
            if Sts (1)(Len (Sts (1)))  =  'o'  then
               Prob ("    EXPECTED VERB not to have -o 1st stem");
            end if;

            --  Check to see no third verb stem has lingering 'i'
            if (Sts (3)(Len (Sts (3)))  =  'i')  and
              (Pt.V.Con /= (6, 1))
            then
               Prob ("    EXPECTED VERB not to have -i 3rd stem");
            end if;

            --  Check that the stems are the same when expected
            if Pt.V.Con.Which < 5  and  Sts (1)(1 .. 3) /= "zzz"   then
               if Pt.V.Con  /= (3, 1) and
                  Pt.V.Con  /= (3, 2) and
                  Pt.V.Con  /= (3, 4)
               then
                  if Sts (1) /= Sts (2)  then
                     Prob ("    EXPECTED IDENTICAL VERB 1 & 2 STEMS");
                  end if;
               elsif Pt.V.Con.Which  = 2 and then
                 (Sts (1)(Len (Sts (1)))  =  'e')
               then
                  Prob ("    EXPECTED (2, X) not to have -e 1st stem");
               elsif Pt.V.Con  = (3, 1) and then
                 Sts (1) /= Sts (2)  and then
                 (Sts (1)(1 .. Len (Sts (1)))  /=
                 Sts (2)(1 .. Len (Sts (2))) & 'i')
               then
                  Prob ("    EXPECTED (3, 1) i-stem  VERB  STEMS");
               elsif Pt.V.Con  = (3, 4) and then
                 (Sts (1)(1 .. Len (Sts (1)))  /=
                 Sts (2)(1 .. Len (Sts (2))) & 'i')
               then
                  Prob ("    EXPECTED (3, 4) i-stem  VERB  STEMS");
               elsif Pt.V.Con  = (3, 2)  then
                  if ((Sts (1)(Len (Sts (1)) - 2 .. Len (Sts (1)))  /=  "fer")
                    or
                    (Sts (2)(Len (Sts (2)) - 3 .. Len (Sts (2)))  /=  "ferr"))
                    or
                    (Sts (1)(1 .. Len (Sts (1)) - 3)  /=
                     Sts (2)(1 .. Len (Sts (2)) - 4))
                  then
                     Prob ("    EXPECTED (3, 2) fer   VERB  STEMS");
                  end if;
               end if;
            end if;

            --  Check that the last 2 verb stems progress as expected
            if Pt.V.Con  = (1, 1)   and then
              (Sts (3) /= ZZZ_Stem and  Sts (4) /= ZZZ_Stem)
            then
               if Len (Sts (3)) >= 3                           and then
                 (Sts (3)(Len (Sts (3)) - 1 .. Len (Sts (3))) = "av")
               then
                  if Sts (4)(Len (Sts (4)) - 1 .. Len (Sts (4))) /= "at"   or
                    (Sts (3)(1 .. Len (Sts (3)) - 2)  /=
                    Sts (4)(1 .. Len (Sts (4)) - 2))
                  then
                     Prob ("    EXPECTED  (1, 1) 3/4  av -> at   VERB STEMS");
                  end if;
               elsif Len (Sts (3)) >= 4                           and then
                 (Sts (3)(Len (Sts (3)) - 2 .. Len (Sts (3))) = "ubu")
               then
                  if Sts (4)(Len (Sts (4)) - 3 .. Len (Sts (4))) /= "ubit"   or
                    (Sts (3)(1 .. Len (Sts (3)) - 3)  /=
                    Sts (4)(1 .. Len (Sts (4)) - 4))
                  then
                     Prob ("    EXPECTED  (1, 1) 3/4 ubu -> ubit VERB STEMS");
                  end if;
               elsif Len (Sts (3)) >= 4                           and then
                 (Sts (3)(Len (Sts (3)) - 2 .. Len (Sts (3))) = "icu")
               then
                  if Sts (4)(Len (Sts (4)) - 3 .. Len (Sts (4))) /= "icit"   or
                    (Sts (3)(1 .. Len (Sts (3)) - 3)  /=
                    Sts (4)(1 .. Len (Sts (4)) - 4))
                  then
                     Prob ("    EXPECTED  (1, 1) 3/4 icu -> icit VERB STEMS");
                  end if;
               elsif Len (Sts (3)) >= 4                           and then
                 (Sts (3)(Len (Sts (3)) - 2 .. Len (Sts (3))) = "onu")
               then
                  if Sts (4)(Len (Sts (4)) - 3 .. Len (Sts (4))) /= "onit"   or
                    (Sts (3)(1 .. Len (Sts (3)) - 3)  /=
                    Sts (4)(1 .. Len (Sts (4)) - 4))
                  then
                     Prob ("    EXPECTED  (1, 1) 3/4 onu -> onit VERB STEMS");
                  end if;
               elsif Sts (1)(Len (Sts (1)) - 1 .. Len (Sts (1))) = "st" then
                  --  sto bad
                  Prob ("           V  (1, 1) 'st' verb  ???? VERB STEMS");
               else
                  Prob ("    EXPECTED  (1, 1) 3/4 regular     VERB STEMS");
               end if;

            end if;

            if Pt.V.Con  = (3, 1)   and then
              (Sts (1) /= ZZZ_Stem  and then Sts (2) /= ZZZ_Stem  and then
              Sts (3) /= ZZZ_Stem  and then Sts (4) /= ZZZ_Stem)
            then
               if Len (Sts (1)) >= 4                           and then
                 (Sts (3)(Len (Sts (3)) - 1 .. Len (Sts (3))) = "ec")
               then
                  if (Sts (1)(Len (Sts (1)) - 3 .. Len (Sts (1))) = "faci")
                    and then
                    ((Sts (2)(1 .. Len (Sts (2))) /=
                      Sts (1)(1 .. Len (Sts (1)) - 4) & "fac") or
                     (Sts (3)(1 .. Len (Sts (3))) /=
                      Sts (1)(1 .. Len (Sts (1)) - 4) & "fec") or
                     (Sts (4)(1 .. Len (Sts (4))) /=
                      Sts (1)(1 .. Len (Sts (1)) - 4) & "fact"))
                  then
                     Prob (
                       "    EXPECTED  (3, 1) 3/4  feci, fec, fec, fact" &
                       "  VERB STEMS");
                  end if;
                  if (Sts (1)(Len (Sts (1)) - 3 .. Len (Sts (1))) = "fici")
                    and then
                    ((Sts (2)(1 .. Len (Sts (2))) /=
                      Sts (1)(1 .. Len (Sts (1)) - 4) & "fic") or
                     (Sts (3)(1 .. Len (Sts (3))) /=
                      Sts (1)(1 .. Len (Sts (1)) - 4) & "fec") or
                     (Sts (4)(1 .. Len (Sts (4))) /=
                      Sts (1)(1 .. Len (Sts (1)) - 4) & "fect"))
                  then
                     Prob ("    EXPECTED  (3, 1) 3/4  fici, fic, fec, fect" &
                       "  VERB STEMS");
                  end if;
               end if;
            elsif Pt.V.Con  = (3, 1)  and then
              Sts (4) /= ZZZ_Stem
            then
               if Len (Sts (3)) >= 3                           and then
                 (Sts (3)(Len (Sts (3)) .. Len (Sts (3))) = "x")
               then
                  if Sts (3)(Len (Sts (3)) - 1 .. Len (Sts (3))) = "nx"   then
                     if not ((Sts (4)(Len (Sts (4)) - 2 .. Len (Sts (4))) =
                       "nct"   and
                       Sts (3)(1 .. Len (Sts (3)) - 1) =
                       Sts (4)(1 .. Len (Sts (4)) - 3))  or
                       (Sts (4)(Len (Sts (4)) - 1 .. Len (Sts (4))) = "ct"   and
                       Sts (3)(1 .. Len (Sts (3)) - 1) =
                       Sts (4)(1 .. Len (Sts (4)) - 3)))
                     then
                        Prob ("    EXPECTED  (3, 1) 3/4  nx -> (n)ct" &
                          "   VERB STEMS");
                     end if;
                  elsif Sts (3)(Len (Sts (3)) - 2 .. Len (Sts (3))) = "fix" then
                     if Sts (3)(1 .. Len (Sts (3)))  /=
                        Sts (4)(1 .. Len (Sts (4)))
                     then
                        Prob (
                          "    EXPECTED  (3, 1) 3/4  fix -> fix  VERB STEMS");
                     end if;
                  elsif Len (Sts (3)) >= 4                           and then
                    Sts (3)(Len (Sts (3)) - 3 .. Len (Sts (3))) = "flex"
                  then
                     if Sts (3)(1 .. Len (Sts (3)))  /=
                        Sts (4)(1 .. Len (Sts (4)))
                     then
                        Prob (
                          "    EXPECTED  (3, 1) 3/4  flex -> flex  VERB STEMS");
                     end if;
                  elsif Len (Sts (3)) >= 4                           and then
                    Sts (3)(Len (Sts (3)) - 3 .. Len (Sts (3))) = "flux"
                  then
                     if Sts (3)(1 .. Len (Sts (3)))  /=
                        Sts (4)(1 .. Len (Sts (4)))
                     then
                        Prob ("    EXPECTED  (3, 1) 3/4  flux -> flux" &
                          "  VERB STEMS");
                     end if;
                  elsif Sts (4)(Len (Sts (4)) - 1 .. Len (Sts (4))) /= "ct"   or
                    (Sts (3)(1 .. Len (Sts (3)) - 1)  /=
                    Sts (4)(1 .. Len (Sts (4)) - 2))
                  then
                     Prob ("    EXPECTED  (3, 1) 3/4  x  -> ct   VERB STEMS");
                  end if;
               end if;
            end if;

            --  Check DEP has no third stem
            if De.Part.V.Kind = Dep  and then
              Sts (3)(1 .. 3) /= "zzz"
            then
               Prob ("    EXPECTED  3 = zzz  DEPON VERB STEMS");
            end if;

         end if;  -- V

      elsif Pt.Pofs = Num  then
         null;

      else

         null;

      end if;

      --  Catch others
      if (Pt.Pofs = N)  or
         (Pt.Pofs = Adj)
      then
         if Len (Sts (1)) >= 3                           and then
           Sts (1)(Len (Sts (1)) .. Len (Sts (1))) = "u"  and then
           Is_Vowel (Sts (2)(Len (Sts (2)) - 1))
         then
            Prob ("    CHECK for terminal u or v      ");
         end if;
         if Len (Sts (1)) >= 3                           and then
           Sts (1)(Len (Sts (1)) .. Len (Sts (1))) = "v"  and then
           not Is_Vowel (Sts (2)(Len (Sts (2)) - 1))
         then
            Prob ("    CHECK for terminal u or v      ");
         end if;
      end if;

      if (Pt.Pofs = V)  and then
         (Pt.V.Con = (2, 1))
      then
         if (Len (Sts (1)) >= 3)                           and then
           (Sts (1)(Len (Sts (1)) - 1 .. Len (Sts (1))) = "pl")  and then
           (Sts (3)(Len (Sts (3)) - 1 .. Len (Sts (3))) /= "ev")
         then
            Prob ("    EXPECTED pleo -> plev  V 2 1   ");
         end if;
      end if;

   exception
      when others   =>
         Put_Line ("VERIFY_STEMS exception        !!!!!!!!!!!!!!!!!!     "
           & Sts (1));
         Put_Line (S (1 .. Last));
         Put_Line (Output,
           "VERIFY_STEMS exception        !!!!!!!!!!!!!!!!!!     " & Sts (1));
         Put_Line (Output, S (1 .. Last));
         --CLOSE (OUTPUT);
   end Verify_Stems;

begin
   Put_Line ("Takes a DICTLINE form named CHECK.IN, " &
     "analyzes for possible errors, and");
   Put_Line ("produces a report CHECK.OUT - " &
     "Remember to process CHECK.OUT from end");
   Create (Output, Out_File, "CHECK.OUT");
   Open (Input, In_File, "CHECK.IN");

   while not End_Of_File (Input) loop
      S := Blank_Line;
      Get_Line (Input, S, Last);
      --PUT_LINE (S (1 .. LAST));
      Line_Number := Line_Number + 1;
      begin

         if S (19)  /= ' '  or
           S (38)  /= ' '  or
           S (57)  /= ' '  or
           S (76)  /= ' '  or
           S (102) /= ' '
         then
            Number := Number + 1;
            Put (Output, "LINE"); Put (Output, Line_Number);
            Put_Line (Output, "      BLANKS not in right place");
            Put_Line (Output, S (1 .. Last));
         end if;

         if S (112)  = ' ' then
            Number := Number + 1;
            Put (Output, "LINE"); Put (Output, Line_Number);
            Put_Line (Output, "      FLAGS may be offset");
            Put_Line (Output, S (1 .. Last));
         end if;

         if Last > 190 and then S (191) /= ' ' then
            Number := Number + 1;
            Put (Output, "LINE"); Put (Output, Line_Number);
            Put_Line (Output, "      LINE is too long");
            Put_Line (Output, S (1 .. Last));
         end if;

         De.Stems (1) := S (1 .. Max_Stem_Size);
         De.Stems (2) := S (Max_Stem_Size + 2 .. 2*Max_Stem_Size + 1);
         De.Stems (3) := S (2*Max_Stem_Size + 3 .. 3*Max_Stem_Size + 2);
         De.Stems (4) := S (3*Max_Stem_Size + 4 .. 4*Max_Stem_Size + 3);

         for I in Stem_Key_Type range 1 .. 4  loop
            if Has_Punctuation (De.Stems (I))  then
               Put (Output, "LINE"); Put (Output, Line_Number);
               Put_Line (Output, "   Offset or Punctuation in line      ");
               Put_Line (Output, S (1 .. 4*Max_Stem_Size + 3));
            end if;
         end loop;

         Get (S (4*Max_Stem_Size + 5 .. Last), De.Part, Ll);
         --GET (S (L+1 .. LAST), DE.PART.POFS, DE.PART.POFS.KIND, LL);

         De.Mean := S (111 .. 110 + Max_Meaning_Size);

         Verify_Stems;

      exception
         when others =>
            Put (Output, "LINE"); Put (Output, Line_Number);
            Put_Line (Output, "      Exception");
            Put_Line (Output, S (1 .. Last));
            Put (Output, De); New_Line (Output);
      end;
   end loop;

   New_Line (Output, 3);
   Put (Output, "Number of entries = ");
   Put (Output, Line_Number); New_Line (Output);
   Put (Output, "Number of errors  = ");
   Put (Output, Number); New_Line (Output);
   Put (Output, "Ratio             = 1 : ");
   Put (Output, Line_Number / Number); New_Line (Output);
   Close (Output);

   New_Line;
   Put ("Number of entries = "); Put (Line_Number); New_Line;
   Put ("Number of errors  = "); Put (Number); New_Line;
   Put ("Ratio             = 1 :"); Put (Line_Number / Number); New_Line;

exception
   when Name_Error  =>
      Put_Line ("No CHECK.IN file to process");
      Close (Output);

   when others =>
      Put ("Exception on LINE"); Put (Line_Number); New_Line;
      Put_Line (S (1 .. Last));
      Close (Output);

end Check;
