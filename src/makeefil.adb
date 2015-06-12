     with TEXT_IO;
     with ENGLISH_SUPPORT_PACKAGE; use ENGLISH_SUPPORT_PACKAGE;
   procedure MAKEEFIL is
       use TEXT_IO;
       use EWDS_DIRECT_IO;
       EWDS_LIST : TEXT_IO.FILE_TYPE;
       EWDS, NEW_EWDS : EWDS_RECORD := NULL_EWDS_RECORD;
     begin
       TEXT_IO.OPEN(EWDS_LIST, TEXT_IO.IN_FILE, "EWDSLIST.GEN");
       CREATE(EWDS_FILE, OUT_FILE, "EWDSFILE.GEN");
       
       while not TEXT_IO.END_OF_FILE(EWDS_LIST)  loop
         EWDS_RECORD_IO.GET(EWDS_LIST, NEW_EWDS);
         TEXT_IO.SKIP_LINE(EWDS_LIST);
         
         
         --  Eliminate doubles    --  If sort is OK
         if EWDS.W = NEW_EWDS.W  and  --  AUX ???? 
            EWDS.N = NEW_EWDS.N  then
-- PUT_LINE("DOUBLES   ");
-- EWDS_RECORD_IO.PUT(EWDS); NEW_LINE;
-- EWDS_RECORD_IO.PUT(NEW_EWDS); NEW_LINE;
              
           if EWDS.KIND > NEW_EWDS.KIND  then  --  Large KIND = high priority
             null;
           elsif EWDS.KIND < NEW_EWDS.KIND  then 
             EWDS := NEW_EWDS;
           elsif EWDS.KIND = NEW_EWDS.KIND  then 
             if EWDS.SEMI > NEW_EWDS.SEMI  then
               EWDS := NEW_EWDS;
             end if;
           end if;
         
         else
               
           WRITE(EWDS_FILE, EWDS);
           EWDS := NEW_EWDS;
           NUMBER_OF_EWORDS := NUMBER_OF_EWORDS + 1;
       end if;
--PUT('.');
       end loop;
       CLOSE(EWDS_FILE);
       TEXT_IO.NEW_LINE;
       TEXT_IO.PUT_LINE("NUMBER_OF_EWORDS = " & INTEGER'IMAGE(NUMBER_OF_EWORDS));
     exception
       when others => 
         CLOSE(EWDS_FILE);
         TEXT_IO.NEW_LINE;
         TEXT_IO.PUT_LINE("MAKEEFIL terminated on an exception");
         TEXT_IO.PUT_LINE("NUMBER_OF_EWORDS = " & INTEGER'IMAGE(NUMBER_OF_EWORDS));
     end MAKEEFIL;

