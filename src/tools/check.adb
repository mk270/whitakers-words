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

with text_io;
with strings_package; use strings_package;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with line_stuff; use line_stuff;
procedure check is
   use inflections_package.integer_io;
   use text_io;
   use dictionary_entry_io;
   use part_entry_io;
   use kind_entry_io;

   input, output : file_type;
   de : dictionary_entry;

   s, line, oldline, blank_line : string(1..400) := (others => ' ');
   j, l, ll, last : integer := 0;

   line_number : integer := 0;
   number : integer := 0;

   function is_vowel(c : character) return boolean is
   begin
	  if (lower_case(c) = 'a') or
		(lower_case(c) = 'e') or
		(lower_case(c) = 'i') or
		(lower_case(c) = 'o') or
		(lower_case(c) = 'u')     then
		 return true;
	  else
		 return false;
	  end if;
   end is_vowel;

   function has_punctuation(s : string) return boolean is
	  j : integer := 0;
   begin
	  for i in reverse s'range  loop
		 j := i;
		 exit when s(i) /= ' ';
	  end loop;
	  for i in s'first..j-1  loop
		 if lower_case(s(i)) not in 'a'..'z'  then
			return true;
		 end if;
	  end loop;
	  return false;
   end has_punctuation;

   function mt(s : stem_type) return boolean is
   begin
	  if s = null_stem_type  then
		 return true;
	  elsif s(1..3) = "zzz"  then
		 return false;
	  else
		 return false;
	  end if;
   end mt;

   function bk(s : stem_type) return boolean is
   begin
	  if s = null_stem_type  then
		 return true;
	  else
		 return false;
	  end if;
   end bk;

   function len(st : stem_type) return integer is
	  l : integer := 0;
   begin
	  for i in st'range  loop
		 exit when st(i) = ' ';
		 l := l + 1;
	  end loop;
	  return l;
   end len;

   procedure verify_stems is
	  sts : stems_type := de.stems;
	  pt  : part_entry := de.part;
	  mean : meaning_type := de.mean;

	  procedure prob(message : string) is
	  begin
		 number := number + 1;
		 put(output, "LINE"); put(output, line_number);
		 put(output, "  " & message & "  ");
		 part_entry_io.put(output, pt);
		 new_line(output);
		 put_line(output, s(1..last));
	  end prob;

   begin

      --  Process parts
	  if pt.pofs = n  then

         --  Check that there are two and only two stems for a noun
		 if (    bk(sts(1)) or
		   bk(sts(2)) or
		   not bk(sts(3)) or
		   not bk(sts(4)) )  and then
		   not (pt.n.decl = (9, 9) or pt.n.decl = (9, 8))   then  --  Undeclined
			prob("    EXPECTED  exactly 2  NOUN STEMS");
		 end if;

         --  Check that the stems are the same when expected
		 if pt.n.decl = (1, 1) or
		   pt.n.decl = (2, 1) or
		   pt.n.decl = (2, 2) or
		   pt.n.decl = (4, 1) or
		   pt.n.decl = (5, 1)    then
			if sts(1) /= sts(2)  and then
			  ((sts(1) /= zzz_stem) and (sts(2) /= zzz_stem))  then
			   prob("    EXPECTED IDENTICAL NOUN STEMS");
			end if;
		 end if;

         --  Check that the stems progress as expected
		 if pt.n.decl = (1, 2)   and then
		   ((sts(1) /= zzz_stem)  or (sts(2) /= zzz_stem))  then
			if len(sts(1)) >= 3                           and then
			  sts(1)(len(sts(1))..len(sts(1))) /= "r"  and then
			  sts(2)(len(sts(2))..len(sts(2))) /= "r"          then
			   prob("    EXPECTED  r and r   (1, 2)  NOUN STEMS");
			end if;
		 end if;

         --  Check that N 2 4 has the 'i's
		 if pt.n.decl = (2, 4)    then
			if (len(sts(1)) >= 3)                          and then

			  (( (sts(1) /= zzz_stem)   and
			  (sts(1)(len(sts(1))..len(sts(1))) /= "i") )  or else
			  ( (sts(1) /= zzz_stem)   and
			  (sts(2)(len(sts(2))..len(sts(2))) /= "i") ))        then

			   prob("    EXPECTED  i and i   (2, 4)  NOUN STEMS");
			end if;
		 end if;

         --   N 3 1

         --  Check N 3 1 er is M/C
		 if pt.n.decl = (3, 1)     then
			if (len(sts(1)) >= 3                           and then
			  sts(1)(len(sts(1))-1..len(sts(1))) = "er")  and then
			  (pt.n.gender /= m  and   pt.n.gender /= c)    then
			   prob("    EXPECTED  -er M/C for  N 3 1 NOUN STEMS");
			end if;
		 end if;

         --  Check er -> er
		 if pt.n.decl = (3, 1) and
		   pt.n.gender = m        then
			if (len(sts(1)) >= 3                           and then
			  sts(1)(len(sts(1))-1..len(sts(1))) = "er")  and then
			  ((sts(2)(len(sts(2))-1..len(sts(2))) /= "er")         or
			  (sts(1)(1..len(sts(1))-2) /= sts(2)(1..len(sts(2))-2)))   then
			   prob("    EXPECTED  er -> er   N 3 1 NOUN STEMS");
			end if;
		 end if;

         --  Check N 3 1 or is M
		 if pt.n.decl = (3, 1)     then
			if (len(sts(1)) >= 3                           and then
			  sts(1)(len(sts(1))-1..len(sts(1))) = "or")  and then
			  pt.n.gender /= m    then
			   prob("    EXPECTED  -or M for  N 3 1 NOUN STEMS");
			end if;
		 end if;

         --  Check or -> or
		 if pt.n.decl = (3, 1) and
		   pt.n.gender = m        then
			if (len(sts(1)) >= 3                           and then
			  sts(1)(len(sts(1))-1..len(sts(1))) = "or")  and then
			  ((sts(2)(len(sts(2))-1..len(sts(2))) /= "or")         or
			  (sts(1)(1..len(sts(1))-2) /= sts(2)(1..len(sts(2))-2)))   then
			   prob("    EXPECTED  or -> or   and S1=S2   N 3 1 NOUN STEMS");
			end if;
		 end if;

         --  Check N 3 1 -o is M except -do, -go, -io
		 if pt.n.decl = (3, 1) and then
		   (sts(1)(len(sts(1))-1..len(sts(1))) = "o")   and then
		   pt.n.gender /= m        then
			if (len(sts(1)) >= 3)                          and then
			  ((sts(1)(len(sts(1))-2..len(sts(1))) /= "do")   and
			  (sts(1)(len(sts(1))-2..len(sts(1))) /= "go")   and
			  (sts(1)(len(sts(1))-2..len(sts(1))) /= "io"))  and then
			  pt.n.gender /= f        then
			   prob("    EXPECTED  N 3 1 -o M excpt -do, -go, -io F");
			end if;
		 end if;

         --  Check io -> ion
		 if pt.n.decl = (3, 1) and
		   pt.n.gender = f        then
			if len(sts(1)) >= 3                           and then
			  sts(1)(len(sts(1))-1..len(sts(1))) = "io"  and then
			  sts(2)(len(sts(2))-2..len(sts(2))) /= "ion"          then
			   prob("    EXPECTED  io -> ion  NOUN STEMS");
			elsif len(sts(1)) >= 3                            and then
			  sts(1)(len(sts(1))-1..len(sts(1))) = "io"   and then
			  sts(2)(len(sts(2))-2..len(sts(2))) = "ion"       then
			   if sts(1)(1..len(sts(1))-1) /=
				 sts(2)(1..len(sts(2))-2)        then
				  prob("    SUSPECT  io - ion  NOUN STEMS  misspelling");
			   end if;
			end if;
		 end if;

		 if pt.n.decl = (3, 1) and
		   pt.n.gender = f     and
		   len(sts(1)) >= 3                            then
			if sts(1)(len(sts(1))-1..len(sts(1))) = "do"  and then
			  sts(2)(len(sts(2))-2..len(sts(2))) /= "din"          then
			   prob("    EXPECTED  do -> din  NOUN  3 1 STEMS");
			end if;
		 elsif sts(1)(len(sts(1))-1..len(sts(1))) = "do"   and then
		   sts(2)(len(sts(2))-2..len(sts(2))) = "din"  then
			if sts(1)(1..len(sts(1))-1) /=
			  sts(2)(1..len(sts(2))-2)        then
			   prob("    SUSPECT  do - din  NOUN STEMS  misspelling");
			end if;
		 end if;

         --  Check as -> at/ad
		 if pt.n.decl = (3, 1) and
		   pt.n.gender = f        then
			if len(sts(1)) >= 3                           and then
			  sts(1)(len(sts(1))-1..len(sts(1))) = "as"  and then
              (sts(2)(len(sts(2))-1..len(sts(2))) /= "at"    and
			  sts(2)(len(sts(2))-1..len(sts(2))) /= "ad" )         then
			   prob("    EXPECTED  as -> at/ad   NOUN STEMS");
			end if;
		 end if;

         --  Check -a, -at is N 3 2 N
		 if pt.n.decl.which  = 3   then
			if (len(sts(1)) >= 3)                          and then
			  (sts(1)(len(sts(1))..len(sts(1))) = "a")  and then
			  ((sts(2)(len(sts(2))-1..len(sts(2))) /= "at")    or
			  (pt.n.decl /= (3, 2)) )      then
			   prob("    EXPECTED  a -> at   NOUN STEMS  to be N 3 2 N");
			end if;
		 end if;

         --  Check es/is -> I-stem
		 if pt.n.decl.which = 3 and
		   (pt.n.gender = m or pt.n.gender = f or pt.n.gender = c)    then
			if len(sts(1)) >= 3                           and then
			  (sts(1)(len(sts(1))-1..len(sts(1))) = "es"  or
			  sts(1)(len(sts(1))-1..len(sts(1))) = "is")    and then
			  sts(1)(1..len(sts(1))-2) = sts(2)(1..len(sts(2)))    then
			   if (pt.n.decl.var /= 3  and pt.n.decl.var /= 9) then
				  prob("    EXPECTED  es/is I-stem (3, 3)");
			   end if;
			end if;
		 end if;

         --  Check is I-stem -es is F                 G&L 58
		 if pt.n.decl = (3, 3) then
			if (len(sts(1)) >= 3)                           and then
			  (sts(1)(len(sts(1))-1..len(sts(1))) = "es")    then
			   if pt.n.gender /= f  then
				  prob("    EXPECTED  -es to be F  N 3 3");
			   end if;
			end if;
		 end if;

         --  Check ns/rs -> I-stem
		 if pt.n.decl.which = 3 and
		   (pt.n.gender = m or pt.n.gender = f or pt.n.gender = c)    then
			if len(sts(1)) >= 3                           and then
			  (sts(1)(len(sts(1))-1..len(sts(1))) = "ns"  or
			  sts(1)(len(sts(1))-1..len(sts(1))) = "rs")    then
			   if pt.n.decl.var /= 3  then
				  prob("    EXPECTED  ns/rs M/F I-stem (3, 3)");
			   end if;
			end if;
		 end if;

         --  Check al/e  -> I-stem
		 if pt.n.decl.which = 3 and
		   (pt.n.gender = n)    then
			if len(sts(1)) >= 3                           and then
			  (sts(1)(len(sts(1))-1..len(sts(1))) = "al"  or
			  sts(1)(len(sts(1))..len(sts(1))) = "e")    then
			   if pt.n.decl.var /= 4  then
				  prob("    EXPECTED  al/e neuter I-stem (3, 4)");
			   end if;
			end if;
		 end if;

         --  Check N 3 starts the same
		 if pt.n.decl.which = 3  then
			if len(sts(1)) >= 4                           and then
			  (sts(1) /= zzz_stem and sts(2) /= zzz_stem)  and then
			  (sts(1)(len(sts(1))..len(sts(1))-1) /=
			  sts(2)(len(sts(1))..len(sts(1))-1))    then
			   prob("    EXPECTED  1st and 2nd stems similiar for N 3 X");
			end if;
		 end if;

         --  Check N 3 GENDER
		 if (pt.n.decl = (3, 1))  and  (pt.n.gender = n)   then
			prob("    EXPECTED  N 3 1 not to be N");
		 elsif (pt.n.decl = (3, 2))  and  (pt.n.gender /= n)   then
			prob("    EXPECTED  N 3 2  to be N");
		 elsif (pt.n.decl = (3, 3))  and  (pt.n.gender = n)   then
			prob("    EXPECTED  N 3 3 not to be N");
		 elsif (pt.n.decl = (3, 4))  and  (pt.n.gender /= n)   then
			prob("    EXPECTED  N 3 4 to be N");
		 end if;

	  elsif pt.pofs = pron  then
		 null;

	  elsif pt.pofs = adj  then

         --  Can only check consistency if more than one stem, CO /= COMP | SUPER
		 if (pt.adj.co = pos or pt.adj.co = x)  and
		   (pt.adj.decl /= (9, 9)   and
		   pt.adj.decl /= (9, 8))        then

            --  Check that the stems are the same when expected
			if (
			  (pt.adj.decl = (3, 2)) or
			  ( (pt.adj.decl = (3, 3)) and then
			  (sts(1)( len(sts(1))..len(sts(1)) ) /= "r")  ) ) and then

			  ( (sts(1) /= zzz_stem) and (sts(2) /= zzz_stem)  )    then
			   if sts(1) /= sts(2)  then
				  prob("    EXPECTED IDENTICAL ADJ STEMS");
			   end if;
			end if;

            --  Check that the stems progress as expected
			if pt.adj.decl = (1, 2)   then
			   if len(sts(1)) >= 3                           and then
				 sts(1)(len(sts(1))..len(sts(1))) /= "r"  and then
				 sts(2)(len(sts(2))..len(sts(2))) /= "r"          then
				  prob("    EXPECTED  r and r   (1, 2)  ADJ STEMS");
			   end if;
			end if;

			if pt.adj.decl = (3, 1)   then
			   if len(sts(1)) >= 3                           and then
				 sts(1)(len(sts(1))-1..len(sts(1))) = "ns"  and then
				 sts(2)(len(sts(2))-1..len(sts(2))) /= "nt"          then
				  prob("    EXPECTED  ns -> nt  (3, 1)  ADJ STEMS");
			   end if;
			end if;

			if pt.adj.decl.which = 3   then
			   if len(sts(1)) >= 3                           and then
				 sts(1)(len(sts(1))-1..len(sts(1))) = "er"  and then
				 pt.adj.decl /= (3, 3)   then
				  prob("    EXPECTED  ADJ 3 with -er to be (3, 3)");
			   end if;
			end if;

			if pt.adj.decl = (3, 1)   then
			   if (len(sts(1)) > len(sts(2)))    then
				  prob("    EXPECTED ADJ (3, 1)  1st stem to be shorter");
			   end if;
			   if (sts(1)(1..len(sts(1))-1) /=
				 sts(2)(1..len(sts(1))-1))         then
				  prob("    EXPECTED ADJ (3, 1)  stems to agree in first letters");
			   end if;
			end if;

		 end if;

		 --  General ADJ things

		 --  Check that ADJ 9 is POS
		 if pt.adj.decl.which = 9 and
		   pt.adj.co /= pos  then
			prob("    EXPECTED  ADJ 9 to be POS");
		 end if;

		 --  Check that ADJ 9 has 1 stem
		 if (pt.adj.decl.which = 9)    and
		   (sts(2) /= null_stem_type)     then
			prob("    EXPECTED  ADJ 9 have just 1 stem");
		 end if;

		 --  Check that there are two and only two stems if POS
		 if pt.adj.co = pos  and pt.adj.decl /= (9, 9) and
		   pt.adj.decl /= (9, 8)    then
			if (sts(3) /= null_stem_type  or
			  sts(4) /= null_stem_type)     then
			   prob("    EXPECTED  exactly 2  POS ADJ  STEMS");
			end if;
		 end if;

		 --  Check that there are more than two stems if X
		 if pt.adj.co = x    then
			if (sts(3) = null_stem_type  or
			  sts(4) = null_stem_type)     then
			   prob("    EXPECTED  4  X  ADJ  STEMS");
			end if;
		 end if;

         --  Check that COMP ends in i, mostly
		 if pt.adj.co = x   then
			if (sts(3) /= null_stem_type  and
			  sts(3) /= zzz_stem)      then
			   if (sts(3)(len(sts(3))) /= 'i')  then
				  prob("    EXPECTED  ADJ  STEM 3 to end in 'i'");
			   elsif sts(3)(1..len(sts(3))-1) /= sts(2)(1..len(sts(2)))  then
				  prob("    EXPECTED  ADJ  STEM 3  = STEM 2 & 'i'");
			   end if;
			end if;
		 end if;

         --  Check that SUPER ends in issi, mostly
		 if pt.adj.co = x    then
			if ((len(sts(3)) > 3) and  (len(sts(4)) > 4))    and then
			  ((sts(3) /= null_stem_type  and
			  sts(3) /= zzz_stem)    and then
			  (sts(4) /= null_stem_type  and
			  sts(4) /= zzz_stem))     then
			   if (sts(3)(len(sts(3))-3..len(sts(3))) = "cili")  then
				  if (sts(4)(len(sts(4))-4..len(sts(4))) /= "cilli") then
					 prob("    EXPECTED  'cil' ADJ  STEM 4 to end in 'cilli'");
				  end if;
			   elsif (sts(3)(len(sts(3))-3..len(sts(3))) = "mili")  then
				  if (sts(4)(len(sts(4))-4..len(sts(4))) /= "milli") then
					 prob("    EXPECTED  'mil' ADJ  STEM 4 to end in 'milli'");
				  end if;
			   elsif (sts(3)(len(sts(3))-1..len(sts(3))) = "ri")  then
				  if (sts(4)(len(sts(4))-2..len(sts(4))) /= "rri") then
					 prob("    EXPECTED  'r' ADJ  STEM 4 to end in 'rri'");
				  end if;
			   elsif sts(4)(len(sts(4))-3..len(sts(4))) /= "issi"  then
				  prob("    EXPECTED  ADJ  STEM 4 to end in 'issi'");
			   elsif sts(4)(1..len(sts(4))-3) /= sts(3)(1..len(sts(3)))  then
				  prob("    EXPECTED  ADJ  STEM 4 to be STEM 3 & 'ssi'");
			   elsif sts(4)(1..len(sts(4))-4) /= sts(2)(1..len(sts(2)))  then
				  prob("    EXPECTED  ADJ  STEM 4 to be STEM 2 & 'issi'");
			   end if;
			end if;
		 end if;

         --  Check that COMP and SUPER are (0, 0)
		 if ((pt.adj.co = comp) or
		   (pt.adj.co = super))     and then
		   (pt.adj.decl /= (0, 0))         then
			prob("    EXPECTED  ADJ  COMP/SUPER to be (0, 0)");
		 end if;

         --  Check that COMP and SUPER have only one stem
		 if ((pt.adj.co = comp) or
		   (pt.adj.co = super))     and then
		   (sts(2) /= null_stem_type)         then
			prob("    EXPECTED  ADJ  COMP/SUPER to have only one stem");
		 end if;

	  elsif pt.pofs = adv  then

         --  Can only check consistency if more than one stem, CO /= COMP | SUPER
		 if (pt.adv.co = pos or pt.adv.co = x)  then

            --  Check that there are two and only two stems if POS
			if pt.adv.co = pos     then
			   if (sts(2) /= null_stem_type  or
				 sts(3) /= null_stem_type)     then
				  prob("    EXPECTED  exactly 1  POS ADV  STEM ");
			   end if;
			end if;

            --  Check that there are more than two stems if X
			if pt.adv.co = x   then
			   if (sts(2) = null_stem_type  or
				 sts(3) = null_stem_type)     then
				  prob("    EXPECTED  3  X  ADV  STEMS");
			   end if;
			end if;

		 end if;

         --  Check that COMP  ends in ius, mostly
		 if pt.adv.co = x        then
			if (sts(2) /= null_stem_type  and
			  sts(2) /= zzz_stem)     and then
			  (sts(2)(len(sts(2))-2..len(sts(2))) /= "ius")  then
			   prob("    EXPECTED  ADV  STEM 2 to end in 'ius'");
			end if;
		 end if;

         --  Check that SUPER ends in ime, mostly
		 if pt.adv.co = x        then
			if (sts(3) /= null_stem_type  and
			  sts(3) /= zzz_stem)     and then
			  (sts(3)(len(sts(3))-2..len(sts(3))) /= "ime")  then
			   prob("    EXPECTED  ADV  STEM 3 to end in 'ime'");
			end if;
		 end if;

         --  Check that SUPER ends in issime, mostly
		 if pt.adv.co = x      then
			if ((len(sts(2)) > 4) and  (len(sts(3)) > 6))    and then
			  ((sts(2) /= null_stem_type  and
			  sts(2) /= zzz_stem)    and then
			  (sts(3) /= null_stem_type  and
			  sts(3) /= zzz_stem))     then
			   if (sts(2)(len(sts(2))-5..len(sts(2))) = "cilius")  then
				  if (sts(3)(len(sts(3))-6..len(sts(3))) /= "cillime") then
					 prob("    EXPECTED  'cil' ADV  STEM 3 to end in 'cillime'");
				  end if;
			   elsif(sts(2)(len(sts(2))-5..len(sts(2))) = "milius")  then
				  if (sts(3)(len(sts(3))-6..len(sts(3))) /= "millime") then
					 prob("    EXPECTED  'mil' ADV  STEM 3 to end in 'millime'");
				  end if;
			   elsif (sts(2)(len(sts(2))-3..len(sts(2))) = "rius")  then
				  if (sts(3)(len(sts(3))-4..len(sts(3))) /= "rrime") then
					 prob("    EXPECTED  'r' ADV  STEM 3 to end in 'rrime'");
				  end if;
			   elsif sts(3)(len(sts(3))-5..len(sts(3))) /= "issime"  then
				  prob("    EXPECTED  ADV  STEM 3 to end in 'issime'");
			   end if;
			end if;
		 end if;

	  elsif pt.pofs = v  then

		 --  Check that V 9 9 has ony one stem
		 if pt.v.con = (9, 9)    then
			if (sts(2) /= null_stem_type  or
			  sts(3) /= null_stem_type  or
			  sts(4) /= null_stem_type)     then
			   prob("    EXPECTED  exactly 1  V (9, 9)  STEM ");
			end if;

		 else

            --  Check to see no first verb stem has lingering 'o'
            if (sts(1)(len(sts(1)))  =  'o')  then
               prob("    EXPECTED VERB not to have -o 1st stem");
            end if;

			--  Check to see no third verb stem has lingering 'i'
            if (sts(3)(len(sts(3)))  =  'i')  and
			  (pt.v.con /= (6, 1))  then
               prob("    EXPECTED VERB not to have -i 3rd stem");
            end if;

			--  Check that the stems are the same when expected
            if pt.v.con.which < 5  and  sts(1)(1..3) /= "zzz"   then
               if (pt.v.con  /= (3, 1) and
				 pt.v.con  /= (3, 2) and
				 pt.v.con  /= (3, 4))   then
                  if sts(1) /= sts(2)  then
                     prob("    EXPECTED IDENTICAL VERB 1 & 2 STEMS");
                  end if;
               elsif pt.v.con.which  = 2 and then
				 (sts(1)(len(sts(1)))  =  'e')  then
                  prob("    EXPECTED (2, X) not to have -e 1st stem");
               elsif pt.v.con  = (3, 1) and then
				 sts(1) /= sts(2)  and then
				 (sts(1)(1..len(sts(1)))  /=
				 sts(2)(1..len(sts(2))) & 'i')  then
                  prob("    EXPECTED (3, 1) i-stem  VERB  STEMS");
               elsif pt.v.con  = (3, 4) and then
				 (sts(1)(1..len(sts(1)))  /=
				 sts(2)(1..len(sts(2))) & 'i')  then
                  prob("    EXPECTED (3, 4) i-stem  VERB  STEMS");
               elsif pt.v.con  = (3, 2)  then
                  if ((sts(1)(len(sts(1))-2..len(sts(1)))  /=  "fer") or
					(sts(2)(len(sts(2))-3..len(sts(2)))  /=  "ferr"))  or
					(sts(1)(1..len(sts(1))-3)  /=
					sts(2)(1..len(sts(2))-4))  then
                     prob("    EXPECTED (3, 2) fer   VERB  STEMS");
                  end if;
               end if;
            end if;

			--  Check that the last 2 verb stems progress as expected
            if pt.v.con  = (1, 1)   and then
			  (sts(3) /= zzz_stem and  sts(4) /= zzz_stem)  then

               if len(sts(3)) >= 3                           and then
				 (sts(3)(len(sts(3))-1..len(sts(3))) = "av")     then
                  if sts(4)(len(sts(4))-1..len(sts(4))) /= "at"   or
					(sts(3)(1..len(sts(3))-2)  /=
					sts(4)(1..len(sts(4))-2))  then
                     prob("    EXPECTED  (1, 1) 3/4  av -> at   VERB STEMS");
                  end if;
               elsif len(sts(3)) >= 4                           and then
				 (sts(3)(len(sts(3))-2..len(sts(3))) = "ubu")     then
                  if sts(4)(len(sts(4))-3..len(sts(4))) /= "ubit"   or
					(sts(3)(1..len(sts(3))-3)  /=
					sts(4)(1..len(sts(4))-4))  then
                     prob("    EXPECTED  (1, 1) 3/4 ubu -> ubit VERB STEMS");
                  end if;
               elsif len(sts(3)) >= 4                           and then
				 (sts(3)(len(sts(3))-2..len(sts(3))) = "icu")     then
                  if sts(4)(len(sts(4))-3..len(sts(4))) /= "icit"   or
					(sts(3)(1..len(sts(3))-3)  /=
					sts(4)(1..len(sts(4))-4))  then
                     prob("    EXPECTED  (1, 1) 3/4 icu -> icit VERB STEMS");
                  end if;
               elsif len(sts(3)) >= 4                           and then
				 (sts(3)(len(sts(3))-2..len(sts(3))) = "onu")     then
                  if sts(4)(len(sts(4))-3..len(sts(4))) /= "onit"   or
					(sts(3)(1..len(sts(3))-3)  /=
					sts(4)(1..len(sts(4))-4))  then
                     prob("    EXPECTED  (1, 1) 3/4 onu -> onit VERB STEMS");
                  end if;
               elsif (sts(1)(len(sts(1))-1..len(sts(1))) = "st")  then  --  sto bad
                  prob("           V  (1, 1) 'st' verb  ???? VERB STEMS");
               else
                  prob("    EXPECTED  (1, 1) 3/4 regular     VERB STEMS");
               end if;

            end if;

            if pt.v.con  = (3, 1)   and then
			  (sts(1) /= zzz_stem  and then sts(2) /= zzz_stem  and then
			  sts(3) /= zzz_stem  and then sts(4) /= zzz_stem)  then
               if len(sts(1)) >= 4                           and then
				 (sts(3)(len(sts(3))-1..len(sts(3))) = "ec")     then
                  if (sts(1)(len(sts(1))-3..len(sts(1))) = "faci")  and then
					((sts(2)(1..len(sts(2))) /= sts(1)(1..len(sts(1))-4) & "fac") or
					(sts(3)(1..len(sts(3))) /= sts(1)(1..len(sts(1))-4) & "fec") or
					(sts(4)(1..len(sts(4))) /= sts(1)(1..len(sts(1))-4) & "fact")) then
                     prob("    EXPECTED  (3, 1) 3/4  feci, fec, fec, fact  VERB STEMS");
                  end if;
                  if (sts(1)(len(sts(1))-3..len(sts(1))) = "fici")  and then
					((sts(2)(1..len(sts(2))) /= sts(1)(1..len(sts(1))-4) & "fic") or
					(sts(3)(1..len(sts(3))) /= sts(1)(1..len(sts(1))-4) & "fec") or
					(sts(4)(1..len(sts(4))) /= sts(1)(1..len(sts(1))-4) & "fect")) then
                     prob("    EXPECTED  (3, 1) 3/4  fici, fic, fec, fect  VERB STEMS");
                  end if;
               end if;
            elsif pt.v.con  = (3, 1)  and then
			  sts(4) /= zzz_stem  then
               if len(sts(3)) >= 3                           and then
				 (sts(3)(len(sts(3))..len(sts(3))) = "x")     then
                  if sts(3)(len(sts(3))-1..len(sts(3))) = "nx"   then
                     if not ((sts(4)(len(sts(4))-2..len(sts(4))) = "nct"   and
					   sts(3)(1..len(sts(3))-1) = sts(4)(1..len(sts(4))-3))  or
					   (sts(4)(len(sts(4))-1..len(sts(4))) = "ct"   and
					   sts(3)(1..len(sts(3))-1) = sts(4)(1..len(sts(4))-3))) then
                        prob("    EXPECTED  (3, 1) 3/4  nx -> (n)ct   VERB STEMS");
                     end if;
                  elsif sts(3)(len(sts(3))-2..len(sts(3))) = "fix"   then
                     if(sts(3)(1..len(sts(3)))  /=
					   sts(4)(1..len(sts(4))))  then
                        prob("    EXPECTED  (3, 1) 3/4  fix -> fix  VERB STEMS");
                     end if;
                  elsif len(sts(3)) >= 4                           and then
					sts(3)(len(sts(3))-3..len(sts(3))) = "flex"   then
                     if(sts(3)(1..len(sts(3)))  /=
					   sts(4)(1..len(sts(4))))  then
                        prob("    EXPECTED  (3, 1) 3/4  flex -> flex  VERB STEMS");
                     end if;
                  elsif len(sts(3)) >= 4                           and then
					sts(3)(len(sts(3))-3..len(sts(3))) = "flux"   then
                     if(sts(3)(1..len(sts(3)))  /=
					   sts(4)(1..len(sts(4))))  then
                        prob("    EXPECTED  (3, 1) 3/4  flux -> flux  VERB STEMS");
                     end if;
                  elsif sts(4)(len(sts(4))-1..len(sts(4))) /= "ct"   or
					(sts(3)(1..len(sts(3))-1)  /=
					sts(4)(1..len(sts(4))-2))  then
                     prob("    EXPECTED  (3, 1) 3/4  x  -> ct   VERB STEMS");
                  end if;
               end if;
            end if;

			--  Check DEP has no third stem
            if de.part.v.kind = dep  and then
			  sts(3)(1..3) /= "zzz"         then
               prob("    EXPECTED  3 = zzz  DEPON VERB STEMS");
            end if;

		 end if;  -- V

	  elsif pt.pofs = num  then
		 null;

	  else

		 null;

	  end if;

      --  Catch others
	  if (pt.pofs = n)  or
		(pt.pofs = adj)  then
		 if len(sts(1)) >= 3                           and then
		   sts(1)(len(sts(1))..len(sts(1))) = "u"  and then
		   is_vowel(sts(2)(len(sts(2))-1))           then
			prob("    CHECK for terminal u or v      ");
		 end if;
		 if len(sts(1)) >= 3                           and then
		   sts(1)(len(sts(1))..len(sts(1))) = "v"  and then
		   not is_vowel(sts(2)(len(sts(2))-1))           then
			prob("    CHECK for terminal u or v      ");
		 end if;
	  end if;

	  if (pt.pofs = v)  and then
		(pt.v.con = (2, 1))  then
		 if (len(sts(1)) >= 3)                           and then
		   (sts(1)(len(sts(1))-1..len(sts(1))) = "pl")  and then
		   (sts(3)(len(sts(3))-1..len(sts(3))) /= "ev")    then
			prob("    EXPECTED pleo -> plev  V 2 1   ");
		 end if;
	  end if;

   exception
	  when others   =>
		 put_line("VERIFY_STEMS exception        !!!!!!!!!!!!!!!!!!     " & sts(1));
		 put_line(s(1..last));
		 put_line(output, "VERIFY_STEMS exception        !!!!!!!!!!!!!!!!!!     " & sts(1));
		 put_line(output, s(1..last));
		 --CLOSE(OUTPUT);
   end verify_stems;

begin
   put_line("Takes a DICTLINE form named CHECK.IN, analyzes for possible errors, and" );
   put_line("produces a report CHECK.OUT - Remember to process CHECK.OUT from end");
   create(output, out_file, "CHECK.OUT");
   open(input, in_file, "CHECK.IN");

   while not end_of_file(input) loop
	  s := blank_line;
	  get_line(input, s, last);
      --PUT_LINE(S(1..LAST));
	  line_number := line_number + 1;
	  begin

		 if s(19)  /= ' '  or
		   s(38)  /= ' '  or
		   s(57)  /= ' '  or
		   s(76)  /= ' '  or
		   s(102) /= ' '    then
			number := number + 1;
			put(output, "LINE"); put(output, line_number);
			put_line(output, "      BLANKS not in right place");
			put_line(output, s(1..last));
		 end if;

		 if s(112)  = ' ' then
			number := number + 1;
			put(output, "LINE"); put(output, line_number);
			put_line(output, "      FLAGS may be offset");
			put_line(output, s(1..last));
		 end if;

		 if (last > 190 and then s(191) /= ' ')  then
			number := number + 1;
			put(output, "LINE"); put(output, line_number);
			put_line(output, "      LINE is too long");
			put_line(output, s(1..last));
		 end if;

		 de.stems(1) := s(1..max_stem_size);
		 de.stems(2) := s(max_stem_size+2..2*max_stem_size+1);
		 de.stems(3) := s(2*max_stem_size+3..3*max_stem_size+2);
		 de.stems(4) := s(3*max_stem_size+4..4*max_stem_size+3);

		 for i in stem_key_type range 1..4  loop
			if has_punctuation(de.stems(i))  then
			   put(output, "LINE"); put(output, line_number);
			   put_line(output, "   Offset or Punctuation in line      ");
			   put_line(output, s(1..4*max_stem_size+3));
			end if;
		 end loop;

		 get(s(4*max_stem_size+5..last), de.part, ll);
		 --GET(S(L+1..LAST), DE.PART.POFS, DE.PART.POFS.KIND, LL);

		 de.mean := s(111..110+max_meaning_size);

         --if LOWER_CASE(S(1..86)) = LOWER_CASE(OLDLINE(1..86)) and then
         --      --  This way I get N  2 1 and 2 2 not duplicate
         --      --  If I make it 1..83, I catch a lot more, a few of which might be ???
         --      --  If I make it 1..88 or 95, I catch a lot less
         --        (OLDLINE(90..95) /= "IMPERS")  and then
         --        not ((LEN(DE.STEMS(1)) >= 2)  and then
         --            (DE.STEMS(1)(LEN(DE.STEMS(1))-1..LEN(DE.STEMS(1))) = "qu"))  then
         --        NUMBER := NUMBER + 1;
         --        PUT(OUTPUT, "LINE"); PUT(OUTPUT, LINE_NUMBER);
         --        PUT_LINE(OUTPUT, "   Possible duplicate lines ");
         --        PUT_LINE(OUTPUT, S(1..LAST));
         --      end if;
         --      OLDLINE(1..190) := S(1..190);

		 verify_stems;

	  exception
		 when others =>
			put(output, "LINE"); put(output, line_number);
			put_line(output, "      Exception");
			put_line(output, s(1..last));
			put(output, de); new_line(output);
	  end;
   end loop;

   new_line(output, 3);
   put(output, "Number of entries = ");
   put(output, line_number); new_line(output);
   put(output, "Number of errors  = ");
   put(output, number); new_line(output);
   put(output, "Ratio             = 1 : ");
   put(output, line_number/number); new_line(output);
   close(output);

   new_line;
   put("Number of entries = "); put(line_number); new_line;
   put("Number of errors  = "); put(number); new_line;
   put("Ratio             = 1 :"); put(line_number/number); new_line;

exception
   when name_error  =>
	  put_line("No CHECK.IN file to process");
	  close(output);

   when others =>
	  put("Exception on LINE"); put(line_number); new_line;
	  put_line(s(1..last));
	  close(output);

end check;
