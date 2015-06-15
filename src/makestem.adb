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
with latin_file_names; use latin_file_names;
with inflections_package; use inflections_package;
with dictionary_package; use dictionary_package;
with word_support_package; use word_support_package;

procedure makestem is
   use stem_key_type_io;
   use count_io;
   use text_io;
   use stem_io;
   use mnpc_io;
   use part_entry_io;

   d_k : dictionary_kind := xxx;   --  ######################

   i : stem_io.count := 0;
   line : string(1..200) := (others => ' ');
   blanks : constant string(1..200) := (others => ' ');
   last, ll : integer := 0;
   ds : dictionary_stem;
   fc, ofc : character := ' ';
   sc, osc : character := ' ';

   procedure put_indices(ch : string;
                         d_k : dictionary_kind) is
      wd : constant string(1..2) := ch(ch'first..ch'first+1);
   begin
      --Put_Line("Put_Indices");
      if ch = "  "  then
         if (bblf(ch(ch'first), ch(ch'first + 1), d_k) > 0)                    and then
           (bbll(ch(ch'first), ch(ch'first + 1), d_k) >= bblf(ch(ch'first), ch(ch'first + 1), d_k))  then
            put("CH = ("); put(ch); put(") index is of range  ");
            put(bblf(ch(ch'first), ch(ch'first + 1), d_k)); put(".."); put(bbll(ch(ch'first), ch(ch'first + 1), d_k));
            put("    number ");
            put(bbll(ch(ch'first), ch(ch'first + 1), d_k) - bblf(ch(ch'first), ch(ch'first + 1), d_k) + 1);
            new_line;
         end if;
      elsif ch(ch'first + 1) = ' '  then
         if (bdlf(ch(ch'first), ch(ch'first + 1), d_k) > 0)                    and then
           (bdll(ch(ch'first), ch(ch'first + 1), d_k) >= bdlf(ch(ch'first), ch(ch'first + 1), d_k))  then
            put("CH = ("); put(ch); put(") index is of range  ");
            put(bdlf(ch(ch'first), ch(ch'first + 1), d_k)); put(".."); put(bdll(ch(ch'first), ch(ch'first + 1), d_k));
            put("    number ");
            put(bdll(ch(ch'first), ch(ch'first + 1), d_k) - bdlf(ch(ch'first), ch(ch'first + 1), d_k) + 1);
            new_line;
         end if;
      else
         if (first_index(wd, d_k) > 0)                and then
           (last_index(wd, d_k) >= first_index(wd, d_k))  then
            put("CH = ("); put(wd); put(") index is of range  ");
            put(first_index(wd, d_k)); put(".."); put(last_index(wd, d_k));
            put("    number ");
            put(last_index(wd, d_k) - first_index(wd, d_k) + 1);
            new_line;
         end if;
      end if;
   end put_indices;

begin
   put_line("Creates STEMFILE.D_K and INDXFILE.D_K from STEMLIST.D_K");

   put("What dictionary to load, GENERAL or SPECIAL  =>");
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

   open(   stem_list(d_k), in_file,
           add_file_name_extension(stem_list_name,
                                   dictionary_kind'image(d_k)));

   create(stem_file(d_k), inout_file,
          add_file_name_extension(stem_file_name,
                                  dictionary_kind'image(d_k)));

   create(indx_file(d_k), out_file,
          add_file_name_extension(indx_file_name,
                                  dictionary_kind'image(d_k)));

   ------------------------------------------------------------------

   --  This section assumes the blank ESSE stem is first - D_K GENERAL
   if d_k = general  then
      i := i + 1;
      bblf(' ', ' ', general) := i;
      bbll(' ', ' ', general) := 0;
      line := blanks;
      get_line(stem_list(d_k), line, last);
      put_line(line(1..last));

      fc := line(1);
      sc := line(2);
      ds.stem := line(1..max_stem_size);
      --PUT_LINE("#" & LINE(MAX_STEM_SIZE+1..LAST));
      get(line(max_stem_size+1..last), ds.part, ll);
      --PUT(DS.PART); NEW_LINE;
      --PUT_LINE("#" & LINE(LL+1..LAST));
      get(line(ll+1..last), ds.key , ll);
      --PUT(DS.KEY ); NEW_LINE;
      --PUT_LINE("#" & LINE(LL+1..LAST));
      get(line(ll+1..last), ds.mnpc , ll);
      --PUT(DS.AAMNPC); NEW_LINE;
      write(stem_file(d_k), ds);
      bbll(fc, sc, general) := i;          --  1

      put(indx_file(d_k), "  ");
      put(indx_file(d_k), ' ');
      put(indx_file(d_k), bblf(' ', ' ', general));
      put(indx_file(d_k), ' ');
      put(indx_file(d_k), bbll(' ', ' ', general));
      put(indx_file(d_k), ' ');
      new_line(indx_file(d_k));

      put_indices("  ", general);

   end if;
   ------------------------------------------------------------------

   fc  := 'a';
   ofc := 'a';
   sc  := ' ';
   osc := ' ';
   bdlf(ofc, ' ', d_k) := i + 1;
   --DEBUG.PUT(" bf1 BDLF("); DEBUG.PUT(OFC);
   --DEBUG.PUT(' '); DEBUG.PUT(")  "); DEBUG.PUT(BDLF(OFC, ' ', D_K));
   --DEBUG.NEW_LINE;

first_character_loop:
    while not end_of_file(stem_list(d_k))  loop
       --OSC := ' ';
       osc := sc;
   second_character_loop:
       while not end_of_file(stem_list(d_k))  loop

      inner_loop:
          while not end_of_file(stem_list(d_k))  loop
             line := blanks;
             get_line(stem_list(d_k), line, last);
             --Put_Line("* " & Line(1..Last));

             if trim(line(1..last)) = "" then put_line("Trim(Line(1..Last)) BLANK"); end if;
             exit first_character_loop when trim(line(1..last)) = "";
             fc := lower_case(line(1));
             sc := lower_case(line(2));
             --------------------------------------------------------------------
             if fc = 'v'  then fc := 'u'; end if;
             if sc = 'v'  then sc := 'u'; end if;
             if fc = 'j'  then fc := 'i'; end if;
             if sc = 'j'  then sc := 'i'; end if;
             --------------------------------------------------------------------
             i := i + 1;

             if sc = ' '  then
                --Put("BDL    I -> "); Put(I       ); New_Line;
                if fc /= ofc  then
                   bdlf(fc, ' ', d_k) := i;
                   --Put(" bf2 BDLF("); Put(Fc);Put(' '); Put(")  ");
                   --Put(Bdlf(Fc, ' ', D_K)); New_Line;
                end if;
             else
                null;
                --Put("I        -> "); Put(I); New_Line;
             end if;

             ds.stem := line(1..max_stem_size);
             get(line(max_stem_size+1..last), ds.part, ll);
             get(line(ll+1..last), ds.key , ll);
             get(line(ll+1..last), ds.mnpc , ll);
             write(stem_file(d_k), ds);
             --Put_Line("Wrote STEMfile");

             if fc /= ofc   then  --  Jumped FC, effectively must have jumped a SC
                                  --Put_Line("Jumped FC");
                if osc = ' '  then
                   bdll(ofc, osc, d_k) := i - 1;
                else
                   ddll(ofc, osc, d_k) := i - 1;
                end if;

                if sc = ' '  then
                   --Put("BDLF  "); Put(Bdlf(Fc, Sc, D_K)); New_Line;
                   bdlf(fc, sc, d_k) := i;
                else
                   ddlf(fc, sc, d_k) := i;
                end if;
                --Put_Line("if Sc done");
                --Put("Ofc = '"); Put(Ofc); Put("'   Osc = '"); Put(Osc); Put_Line("'");
                put_indices(ofc & osc, d_k);
                ofc := fc;
                osc := sc;
                --Put_Line("exit Second_Character_Loop");

                exit second_character_loop;
             else
                if sc /= osc  then          --  Jumped a SC, but not a FC
                   if osc = ' '  then        --  Jumped a SC from ' ' to something
                      bdll(fc, osc, d_k) := i - 1;            --  So set BDLL
                                                              --DEBUG.PUT(" bl1 BDLL("); DEBUG.PUT(FC); DEBUG.PUT(OSC); DEBUG.PUT(")  ");
                                                              --DEBUG.PUT(BDLL(FC, OSC, D_K)); DEBUG.NEW_LINE;
                      ddlf(fc, sc, d_k) := i;
                      --DEBUG.PUT(" df1 DDLF("); DEBUG.PUT( FC); DEBUG.PUT( SC); DEBUG.PUT(")  ");
                      --DEBUG.PUT(DDLF( FC,  SC, D_K)); DEBUG.NEW_LINE;
                      put_indices(fc & osc, d_k);
                      osc := sc;

                      exit inner_loop;
                   else                      --  Jumped a SL from something, not ' '
                      ddll(fc, osc, d_k) := i - 1;    --  So set DDLL
                                                      --DEBUG.PUT(" dl2 DDLL("); DEBUG.PUT(FC); DEBUG.PUT(OSC); DEBUG.PUT(")  ");
                                                      --DEBUG.PUT(DDLL(FC, OSC, D_K)); DEBUG.NEW_LINE;
                      ddlf(fc, sc, d_k) := i;
                      --DEBUG.PUT(" df2 DDLF("); DEBUG.PUT( FC); DEBUG.PUT( SC); DEBUG.PUT(")  ");
                      --DEBUG.PUT(DDLF( FC,  SC, D_K)); DEBUG.NEW_LINE;
                      put_indices(fc & osc, d_k);
                      osc := sc;

                      exit inner_loop;
                   end if;
                end if;
             end if;

          end loop inner_loop;
          --Put_Line("Exitted Inner_Loop");

       end loop second_character_loop;
       --Put_Line("Exitted Second_Character_Loop");

    end loop first_character_loop;
    --Put_Line("Exitted First_Character_Loop");
    ddll(ofc, osc, d_k) := i;

    --  To reprint correctly the last letter information
    --Put_Line("--  To reprint correctly the last letter information");
    put_indices(ofc & osc, d_k);
    close(stem_file(d_k));

    for i in character'('a')..character'('z')  loop
       for j in character'(' ')..character'(' ')  loop
          text_io.put(indx_file(d_k), (i, j));
          put(indx_file(d_k), ' ');
          put(indx_file(d_k), bdlf(i, j, d_k));
          put(indx_file(d_k), ' ');
          put(indx_file(d_k), bdll(i, j, d_k));
          put(indx_file(d_k), ' ');
          new_line(indx_file(d_k));
       end loop;
    end loop;
    for i in character'('a')..character'('z')  loop
       for j in character'('a')..character'('z')  loop
          text_io.put(indx_file(d_k), (i, j));
          put(indx_file(d_k), ' ');
          put(indx_file(d_k), ddlf(i, j, d_k));
          put(indx_file(d_k), ' ');
          put(indx_file(d_k), ddll(i, j, d_k));
          put(indx_file(d_k), ' ');
          new_line(indx_file(d_k));
       end loop;
    end loop;
    close(indx_file(d_k));

end makestem;
