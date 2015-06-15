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
with english_support_package; use english_support_package;
procedure makeefil is
   use text_io;
   use ewds_direct_io;
   ewds_list : text_io.file_type;
   ewds, new_ewds : ewds_record := null_ewds_record;
begin
   text_io.open(ewds_list, text_io.in_file, "EWDSLIST.GEN");
   create(ewds_file, out_file, "EWDSFILE.GEN");

   while not text_io.end_of_file(ewds_list)  loop
      ewds_record_io.get(ewds_list, new_ewds);
      text_io.skip_line(ewds_list);

      --  Eliminate doubles    --  If sort is OK
      if ewds.w = new_ewds.w  and  --  AUX ????
        ewds.n = new_ewds.n  then
         -- PUT_LINE("DOUBLES   ");
         -- EWDS_RECORD_IO.PUT(EWDS); NEW_LINE;
         -- EWDS_RECORD_IO.PUT(NEW_EWDS); NEW_LINE;

         if ewds.kind > new_ewds.kind  then  --  Large KIND = high priority
            null;
         elsif ewds.kind < new_ewds.kind  then
            ewds := new_ewds;
         elsif ewds.kind = new_ewds.kind  then
            if ewds.semi > new_ewds.semi  then
               ewds := new_ewds;
            end if;
         end if;

      else

         write(ewds_file, ewds);
         ewds := new_ewds;
         number_of_ewords := number_of_ewords + 1;
      end if;
      --PUT('.');
   end loop;
   close(ewds_file);
   text_io.new_line;
   text_io.put_line("NUMBER_OF_EWORDS = " & integer'image(number_of_ewords));
exception
   when others =>
      close(ewds_file);
      text_io.new_line;
      text_io.put_line("MAKEEFIL terminated on an exception");
      text_io.put_line("NUMBER_OF_EWORDS = " & integer'image(number_of_ewords));
end makeefil;
