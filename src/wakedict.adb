with text_io; 
with strings_package; use strings_package;  
with latin_file_names; use latin_file_names;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with line_stuff; use line_stuff;
procedure wakedict is 
   package integer_io is new text_io.integer_io(integer);
   use text_io;
   use stem_key_type_io;
   use dictionary_entry_io;
   use part_entry_io;
   use kind_entry_io;
   use translation_record_io;
   use age_type_io;
   use area_type_io;
   use geo_type_io;
   use frequency_type_io;
   use source_type_io;
   use dict_io;
   
   porting : constant boolean := false;
   
   be_ve : verb_entry := (con => (5, 1), kind => to_be);
   
   d_k : dictionary_kind := xxx;       --  ######################
   
   
   start_stem_1  : constant := 1;
   start_stem_2  : constant := start_stem_1 + max_stem_size + 1;
   start_stem_3  : constant := start_stem_2 + max_stem_size + 1;
   start_stem_4  : constant := start_stem_3 + max_stem_size + 1;
   start_part    : constant := start_stem_4 + max_stem_size + 1;
   start_tran    : constant integer := 
	 start_part + 
	 integer(part_entry_io.default_width + 1);
   finish_line   : constant integer := 
	 start_tran +
	 translation_record_io.default_width - 1;
   
   
   
   
   
   dictfile : dict_io.file_type;
   input, stemlist : text_io.file_type;
   de : dictionary_entry;
   
   s, line, blank_line : string(1..400) := (others => ' ');
   l, ll, last : integer := 0;
   j : dict_io.count := 0;
   mean_to_be : constant meaning_type := 
	 head("be; exist; (also used to form verb perfect passive tenses)" &
			" with NOM PERF PPL", max_meaning_size);
   
begin
   put_line(
			"Takes a DICTLINE.D_K and produces a STEMLIST.D_K and DICTFILE.D_K");
   put_line("This version inserts ESSE when D_K = GEN");
   put("What dictionary to list, GENERAL or SPECIAL  (Reply G or S) =>");
   get_line(line, last);
   if last > 0  then
	  if trim(line(1..last))(1) = 'G'  or else
		trim(line(1..last))(1) = 'g'     then
		 d_k := general;
	  elsif trim(line(1..last))(1) = 'S'  or else
		trim(line(1..last))(1) = 's'     then
		 d_k := special;
	  else
		 put_line("No such dictionary");
		 raise text_io.data_error;
	  end if; 
   end if;
   
   
   open(input, in_file, add_file_name_extension(dict_line_name, 
                                                dictionary_kind'image(d_k))); 
   
   if not porting  then
      
	  create(stemlist, out_file, add_file_name_extension(stem_list_name, 
														 dictionary_kind'image(d_k)));
   end if;
   
   create(dictfile, out_file, add_file_name_extension(dict_file_name, 
													  dictionary_kind'image(d_k)));
   
   --      if D_K = GENERAL  then
   --         PUT_LINE("WAKEDICT reads DICTLINE.d_k and produces DICTFILE.d_k");
   --         PUT_LINE("WAKEDICT also produces STEMLIST.d_k");
   --         PUT_LINE("This version inserts ESSE when d_k = GEN");
   --      
   --         J := J + 1;
   --      
   --      --  First construct ESSE
   --         DE.STEMS(1) := "s                 ";
   --         DE.STEMS(2) := "                  ";
   --         DE.STEMS(3) := "fu                ";
   --         DE.STEMS(4) := "fut               ";
   --      --DE.PART := (PART => V,  CON => (5, 10));
   --      --DE.PART := (V, ((5, 1)));
   --         DE.PART := (V, BE_VE);
   --         DE.KIND := (V, TO_BE);
   --         DE.TRAN := (X, X, X, A, X);
   --         DE.MEAN := MEAN_TO_BE;
   --      
   --      
   --         if not PORTING  then
   --         --  Load ESSE
   --            for I in STEM_KEY_TYPE range 1..4  loop
   --               PUT(STEMLIST, DE.STEMS(I)); PUT(STEMLIST, ' ');
   --               PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
   --               SET_COL(STEMLIST, 45);
   --               PUT(STEMLIST, I, 2); PUT(STEMLIST, ' ');
   --            --      PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
   --            --      PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
   --            --      PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
   --            --      PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
   --            --      PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
   --               SET_COL(STEMLIST, 50);
   --               INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
   --            end loop;
   --         end if;
   --      
   --         WRITE(DICTFILE, DE, J);        --  J = 1
   --      end if;
   --   
   
   --  Now do the rest 
over_lines:
	while not end_of_file(input) loop
	   s := blank_line;
	   get_line(input, s, last);
	   if trim(s(1..last)) /= ""  then
		  l := 0;
		  
	  form_de:
		  begin
			 
			 de.stems(1) := s(start_stem_1..max_stem_size);
			 --NEW_LINE; PUT(DE.STEMS(1));
			 de.stems(2) := s(start_stem_2..start_stem_2+max_stem_size-1);
			 de.stems(3) := s(start_stem_3..start_stem_3+max_stem_size-1);
			 de.stems(4) := s(start_stem_4..start_stem_4+max_stem_size-1);
			 --PUT('#'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
			 --PUT('@'); 
			 get(s(start_part..last), de.part, l);
			 --PUT('%'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
			 --PUT('&'); PUT(S(L+1..LAST)); PUT('3'); 
			 -- GET(S(L+1..LAST), DE.PART.POFS, DE.KIND, L);
			 get(s(l+1..last), de.tran.age, l);
			 get(s(l+1..last), de.tran.area, l);
			 get(s(l+1..last), de.tran.geo, l);
			 get(s(l+1..last), de.tran.freq, l);
			 get(s(l+1..last), de.tran.source, l);
			 de.mean := head(s(l+2..last), max_meaning_size);
			 --  Note that this allows initial blanks
			 --  L+2 skips over the SPACER, required because this is STRING, not ENUM
			 
		  exception
			 when others =>
				new_line;
				put_line("Exception");
				put_line(s(1..last));
				integer_io.put(integer(j)); new_line;
				put(de); new_line;
		  end form_de;
		  
		  j := j + 1;
		  write(dictfile, de, j);
		  
		  
		  if not porting  then
			 
			 if de.part.pofs = n    and then
               de.stems(1) = de.stems(2)     and then
               de.stems(1) /= zzz_stem       then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 0, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
				--        if DE.STEMS(3) /= NULL_STEM_TYPE  and DE.STEMS(3) /= ZZZ_STEM  then
				--          PUT(STEMLIST, DE.STEMS(3)); PUT(STEMLIST, ' ');
				--          PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
				--          SET_COL(STEMLIST, 45);
				--          INTEGER_IO.PUT(STEMLIST, 3, 2); PUT(STEMLIST, ' ');
				----          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				----          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				----          PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				----          PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				----          PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				--          INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
				--        end if;
				--        if DE.STEMS(4) /= NULL_STEM_TYPE  and DE.STEMS(4) /= ZZZ_STEM  then
				--          PUT(STEMLIST, DE.STEMS(4)); PUT(STEMLIST, ' ');
				--          PUT(STEMLIST, DE.PART); PUT(STEMLIST, ' ');
				--          SET_COL(STEMLIST, 45);
				--          INTEGER_IO.PUT(STEMLIST, 4, 2); PUT(STEMLIST, ' ');
				--          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				--          INTEGER_IO.PUT(STEMLIST, INTEGER(J), 6); NEW_LINE(STEMLIST);
				--        end if;
			 elsif de.part.pofs = adj  and then
               de.stems(1) = de.stems(2)     and then
               de.stems(1) /= zzz_stem       then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 0, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
				if de.stems(3) /= null_stem_type  and de.stems(3) /= zzz_stem  then
				   put(stemlist, de.stems(3)); put(stemlist, ' ');
				   put(stemlist, de.part); put(stemlist, ' ');
				   set_col(stemlist, 45);
				   integer_io.put(stemlist, 3, 2); put(stemlist, ' ');
				   --          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				   set_col(stemlist, 50);
				   integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
				end if;
				if de.stems(4) /= null_stem_type  and de.stems(4) /= zzz_stem  then
				   put(stemlist, de.stems(4)); put(stemlist, ' ');
				   put(stemlist, de.part); put(stemlist, ' ');
				   set_col(stemlist, 45);
				   integer_io.put(stemlist, 4, 2); put(stemlist, ' ');
				   --          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				   set_col(stemlist, 50);
				   integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
				end if;
			 elsif de.part.pofs = adj  and then
               --  POS taken care of by position
               de.part.adj.co = comp   then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 3, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
			 elsif de.part.pofs = adj  and then
               de.part.adj.co = super  then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 4, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
			 elsif de.part.pofs = adv  and then
               --  POS taken care of by position
               de.part.adv.co = comp   then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 2, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
			 elsif de.part.pofs = adv  and then
               de.part.adv.co = super  then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 3, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
			 elsif de.part.pofs = v    and then
               de.stems(1) = de.stems(2)     and then
               de.stems(1) /= zzz_stem       then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 0, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
				if de.stems(3) /= null_stem_type  and de.stems(3) /= zzz_stem  then
				   put(stemlist, de.stems(3)); put(stemlist, ' ');
				   put(stemlist, de.part); put(stemlist, ' ');
				   set_col(stemlist, 45);
				   integer_io.put(stemlist, 3, 2); put(stemlist, ' ');
				   --          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				   set_col(stemlist, 50);
				   integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
				end if;
				if de.stems(4) /= null_stem_type  and de.stems(4) /= zzz_stem  then
				   put(stemlist, de.stems(4)); put(stemlist, ' ');
				   put(stemlist, de.part); put(stemlist, ' ');
				   set_col(stemlist, 45);
				   integer_io.put(stemlist, 4, 2); put(stemlist, ' ');
				   --          PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				   --          PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				   set_col(stemlist, 50);
				   integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
				end if;
			 elsif de.part.pofs = num  and then
               de.part.num.sort = card   then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 1, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
			 elsif de.part.pofs = num  and then
               de.part.num.sort = ord    then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 2, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
			 elsif de.part.pofs = num  and then
               de.part.num.sort = dist   then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 3, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
			 elsif de.part.pofs = num  and then
               de.part.num.sort = adverb  then
				put(stemlist, de.stems(1)); put(stemlist, ' ');
				put(stemlist, de.part); put(stemlist, ' ');
				set_col(stemlist, 45);
				integer_io.put(stemlist, 4, 2); put(stemlist, ' ');
				--        PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
				--        PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
				set_col(stemlist, 50);
				integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
			 else
				for i in stem_key_type range 1..4  loop
				   if de.stems(i) /= zzz_stem  and 
                     de.stems(i) /= null_stem_type  then
					  put(stemlist, de.stems(i)); put(stemlist, ' ');
					  put(stemlist, de.part); put(stemlist, ' ');
					  set_col(stemlist, 45);
					  put(stemlist, i, 2); put(stemlist, ' ');
					  --            PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
					  --            PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
					  --            PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
					  --            PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
					  --            PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
					  set_col(stemlist, 50);
					  integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
				   end if;
				end loop;
			 end if;
		  end if;   --  PORTING
	   end if;
	end loop over_lines;
	
	
	
	
	
	
	
	
	
	if d_k = general  then
	   
	   
	   j := j + 1;
	   
	   --  First construct ESSE
	   de.stems(1) := "s                 ";
	   de.stems(2) := "                  ";
	   de.stems(3) := "fu                ";
	   de.stems(4) := "fut               ";
	   --DE.PART := (PART => V,  CON => (5, 10));
	   --DE.PART := (V, ((5, 1)));
	   de.part := (v, be_ve);
	   --DE.KIND := (V, TO_BE);
	   de.tran := (x, x, x, a, x);
	   de.mean := mean_to_be;
	   
	   
	   if not porting  then
		  --  Load ESSE
		  for i in stem_key_type range 1..4  loop
			 put(stemlist, de.stems(i)); put(stemlist, ' ');
			 put(stemlist, de.part); put(stemlist, ' ');
			 set_col(stemlist, 45);
			 put(stemlist, i, 2); put(stemlist, ' ');
			 --      PUT(STEMLIST, DE.TRAN.AGE); PUT(STEMLIST, ' ');
			 --      PUT(STEMLIST, DE.TRAN.AREA); PUT(STEMLIST, ' ');
			 --      PUT(STEMLIST, DE.TRAN.GEO); PUT(STEMLIST, ' ');
			 --      PUT(STEMLIST, DE.TRAN.FREQ); PUT(STEMLIST, ' ');
			 --      PUT(STEMLIST, DE.TRAN.SOURCE); PUT(STEMLIST, ' ');
			 set_col(stemlist, 50);
			 integer_io.put(stemlist, integer(j), 6); new_line(stemlist);
		  end loop;
	   end if;
	   
	   write(dictfile, de, j);       
	end if;
	
	
	
	
	if not porting  then
	   close(stemlist);
	end if;
	
exception
   when text_io.data_error  =>
	  null;
   when others =>
	  put_line(s(1..last));
	  integer_io.put(integer(j)); new_line;
	  close(stemlist);
	  
end wakedict;
