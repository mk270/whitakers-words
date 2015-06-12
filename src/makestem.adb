   with Text_Io; 
   with Strings_Package; use Strings_Package;  
   with Latin_File_Names; use Latin_File_Names;
   with Inflections_Package; use Inflections_Package;
   with Dictionary_Package; use Dictionary_Package;
   with Word_Support_Package; use Word_Support_Package;

   procedure MAKESTEM is
      use Integer_Io;
      use STEM_KEY_TYPE_IO;
      use COUNT_IO;
      use Text_IO; 
      use STEM_IO;
      use MNPC_IO;
      use Part_Entry_IO;
   
      D_K : Dictionary_Kind := XXX;   --  ######################
   
   
      I : STEM_IO.COUNT := 0;
      Line, Blanks : String(1..200) := (others => ' ');
      Last, Ll : Integer := 0;
      M : STEM_IO.Positive_Count := 1;
      Ds : Dictionary_Stem;
      Fc, Ofc : Character := ' ';
      Sc, Osc : Character := ' ';
   
      procedure Put_Indices(Ch : String; 
                            D_K : Dictionary_Kind) is
         Wd : String(1..2) := Ch(1..2);
      begin
      --Put_Line("Put_Indices");
         if Ch = "  "  then
            if (Bblf(Ch(1), Ch(2), D_K) > 0)                    and then 
               (Bbll(Ch(1), Ch(2), D_K) >= Bblf(Ch(1), Ch(2), D_K))  then
               Put("CH = ("); Put(Ch); Put(") index is of range  "); 
               Put(Bblf(Ch(1), Ch(2), D_K)); Put(".."); Put(Bbll(Ch(1), Ch(2), D_K)); 
               Put("    number ");
               Put(Bbll(Ch(1), Ch(2), D_K) - Bblf(Ch(1), Ch(2), D_K) + 1);
               New_Line;
            end if;
         elsif Ch(2) = ' '  then
            if (Bdlf(Ch(1), Ch(2), D_K) > 0)                    and then
               (Bdll(Ch(1), Ch(2), D_K) >= Bdlf(Ch(1), Ch(2), D_K))  then
               Put("CH = ("); Put(Ch); Put(") index is of range  "); 
               Put(Bdlf(Ch(1), Ch(2), D_K)); Put(".."); Put(Bdll(Ch(1), Ch(2), D_K)); 
               Put("    number ");
               Put(Bdll(Ch(1), Ch(2), D_K) - Bdlf(Ch(1), Ch(2), D_K) + 1);
               New_Line;
            end if;
         else
            if (First_Index(Wd, D_K) > 0)                and then
               (Last_Index(Wd, D_K) >= First_Index(Wd, D_K))  then
               Put("CH = ("); Put(Wd); Put(") index is of range  "); 
               Put(First_Index(Wd, D_K)); Put(".."); Put(Last_Index(Wd, D_K)); 
               Put("    number ");
               Put(Last_Index(Wd, D_K) - First_Index(Wd, D_K) + 1);
               New_Line;
            end if;
         end if;
      end Put_Indices;
   
   begin
      Put_Line("Creates STEMFILE.D_K and INDXFILE.D_K from STEMLIST.D_K");
   
      Put("What dictionary to load, GENERAL or SPECIAL  =>");
      Get_Line(Line, Last);
      if Last > 0  then
         if Trim(Line(1..Last))(1) = 'G'  or else
         Trim(Line(1..Last))(1) = 'g'     then
            D_K := General;
         elsif Trim(Line(1..Last))(1) = 'S'  or else
         Trim(Line(1..Last))(1) = 's'     then
            D_K := Special;
         else
            Put_Line("No such dictionary");
            raise Text_Io.Data_Error;
         end if; 
      end if;
   
      Open(   STEM_List(D_K), In_File,
              Add_File_Name_Extension(STEM_List_Name, 
                                      Dictionary_Kind'Image(D_K)));
   
      Create(STEM_File(D_K), Inout_File, 
             Add_File_Name_Extension(STEM_File_Name, 
                                     Dictionary_Kind'Image(D_K)));
   
      Create(Indx_File(D_K), Out_File, 
             Add_File_Name_Extension(Indx_File_Name, 
                                     Dictionary_Kind'Image(D_K)));
   
   ------------------------------------------------------------------
   
    --  This section assumes the blank ESSE stem is first - D_K GENERAL
      if D_K = General  then
         I := I + 1;
         Bblf(' ', ' ', General) := I;
         Bbll(' ', ' ', General) := 0;
         Line := Blanks;
         Get_Line(STEM_List(D_K), Line, Last);
      PUT_LINE(LINE(1..LAST));
      
         Fc := Line(1);
         Sc := Line(2);
         Ds.Stem := Line(1..Max_Stem_Size);
--PUT_LINE("#" & LINE(MAX_STEM_SIZE+1..LAST)); 
         Get(Line(Max_Stem_Size+1..Last), Ds.Part, Ll); 
      --PUT(DS.PART); NEW_LINE;
      --PUT_LINE("#" & LINE(LL+1..LAST));
         Get(Line(Ll+1..Last), Ds.Key , Ll);
      --PUT(DS.KEY ); NEW_LINE;
      --PUT_LINE("#" & LINE(LL+1..LAST));
         Get(Line(Ll+1..Last), Ds.mnpc , Ll);
      --PUT(DS.AAMNPC); NEW_LINE;
         Write(STEM_File(D_K), Ds);
         Bbll(Fc, Sc, General) := I;          --  1
      
         Put(Indx_File(D_K), "  ");
         Put(Indx_File(D_K), ' ');
         Put(Indx_File(D_K), Bblf(' ', ' ', General));
         Put(Indx_File(D_K), ' ');
         Put(Indx_File(D_K), Bbll(' ', ' ', General));
         Put(Indx_File(D_K), ' ');
         New_Line(Indx_File(D_K));
      
         Put_Indices("  ", General);
      
      end if;
   ------------------------------------------------------------------
   
      Fc  := 'a';
      Ofc := 'a';
      Sc  := ' ';
      Osc := ' ';
      Bdlf(Ofc, ' ', D_K) := I + 1;
   --DEBUG.PUT(" bf1 BDLF("); DEBUG.PUT(OFC); 
   --DEBUG.PUT(' '); DEBUG.PUT(")  "); DEBUG.PUT(BDLF(OFC, ' ', D_K)); 
   --DEBUG.NEW_LINE;
   
   First_Character_Loop:
      while not End_Of_File(STEM_List(D_K))  loop
      --OSC := ' ';
         Osc := Sc;  
      Second_Character_Loop:
         while not End_Of_File(STEM_List(D_K))  loop
         
         Inner_Loop:
            while not End_Of_File(STEM_List(D_K))  loop
               Line := Blanks;
               Get_Line(STEM_List(D_K), Line, Last);
 --Put_Line("* " & Line(1..Last));
            
               if Trim(Line(1..Last)) = "" then Put_Line("Trim(Line(1..Last)) BLANK"); end if;
               exit First_Character_Loop when Trim(Line(1..Last)) = "";
               Fc := Lower_Case(Line(1));
               Sc := Lower_Case(Line(2));
            --------------------------------------------------------------------
               if Fc = 'v'  then Fc := 'u'; end if;
               if Sc = 'v'  then Sc := 'u'; end if;
               if Fc = 'j'  then Fc := 'i'; end if;
               if Sc = 'j'  then Sc := 'i'; end if;
            --------------------------------------------------------------------        
               I := I + 1;
            
               if Sc = ' '  then
               --Put("BDL    I -> "); Put(I       ); New_Line;
                  if Fc /= Ofc  then
                     Bdlf(Fc, ' ', D_K) := I;        
                  --Put(" bf2 BDLF("); Put(Fc);Put(' '); Put(")  "); 
                  --Put(Bdlf(Fc, ' ', D_K)); New_Line;
                  end if;
               else 
                  null;
               --Put("I        -> "); Put(I); New_Line;
               end if;
            
               Ds.Stem := Line(1..Max_Stem_Size);
               Get(Line(Max_Stem_Size+1..Last), Ds.Part, Ll);
               Get(Line(Ll+1..Last), Ds.Key , Ll);
               Get(Line(Ll+1..Last), Ds.mnpc , Ll);
               Write(STEM_File(D_K), Ds);
            --Put_Line("Wrote STEMfile");
            
               if Fc /= Ofc   then  --  Jumped FC, effectively must have jumped a SC
               --Put_Line("Jumped FC");
                  if Osc = ' '  then
                     Bdll(Ofc, Osc, D_K) := I - 1;
                  else
                     Ddll(Ofc, Osc, D_K) := I - 1;
                  end if;
               
                  if Sc = ' '  then 
                  --Put("BDLF  "); Put(Bdlf(Fc, Sc, D_K)); New_Line;
                     Bdlf(Fc, Sc, D_K) := I;
                  else
                     Ddlf(Fc, Sc, D_K) := I;
                  end if;
               --Put_Line("if Sc done");
               --Put("Ofc = '"); Put(Ofc); Put("'   Osc = '"); Put(Osc); Put_Line("'");
                  Put_Indices(Ofc & Osc, D_K);
                  Ofc := Fc;
                  Osc := Sc;
               --Put_Line("exit Second_Character_Loop");
               
                  exit Second_Character_Loop;
               else
                  if Sc /= Osc  then          --  Jumped a SC, but not a FC
                     if Osc = ' '  then        --  Jumped a SC from ' ' to something
                        Bdll(Fc, Osc, D_K) := I - 1;            --  So set BDLL
                     --DEBUG.PUT(" bl1 BDLL("); DEBUG.PUT(FC); DEBUG.PUT(OSC); DEBUG.PUT(")  "); 
                     --DEBUG.PUT(BDLL(FC, OSC, D_K)); DEBUG.NEW_LINE;
                        Ddlf(Fc, Sc, D_K) := I;
                     --DEBUG.PUT(" df1 DDLF("); DEBUG.PUT( FC); DEBUG.PUT( SC); DEBUG.PUT(")  "); 
                     --DEBUG.PUT(DDLF( FC,  SC, D_K)); DEBUG.NEW_LINE;
                        Put_Indices(Fc & Osc, D_K);
                        Osc := Sc;
                     
                        exit Inner_Loop;
                     else                      --  Jumped a SL from something, not ' '
                        Ddll(Fc, Osc, D_K) := I - 1;    --  So set DDLL
                     --DEBUG.PUT(" dl2 DDLL("); DEBUG.PUT(FC); DEBUG.PUT(OSC); DEBUG.PUT(")  "); 
                     --DEBUG.PUT(DDLL(FC, OSC, D_K)); DEBUG.NEW_LINE;
                        Ddlf(Fc, Sc, D_K) := I;
                     --DEBUG.PUT(" df2 DDLF("); DEBUG.PUT( FC); DEBUG.PUT( SC); DEBUG.PUT(")  "); 
                     --DEBUG.PUT(DDLF( FC,  SC, D_K)); DEBUG.NEW_LINE;
                        Put_Indices(Fc & Osc, D_K);
                        Osc := Sc;
                     
                        exit Inner_Loop;
                     end if;
                  end if;
               end if;
            
            
            
            end loop Inner_Loop;
         --Put_Line("Exitted Inner_Loop");
         
         end loop Second_Character_Loop;
      --Put_Line("Exitted Second_Character_Loop");
      
      end loop First_Character_Loop;
   --Put_Line("Exitted First_Character_Loop");
      Ddll(Ofc, Osc, D_K) := I;
   
   --  To reprint correctly the last letter information
   --Put_Line("--  To reprint correctly the last letter information");
      Put_Indices(Ofc & Osc, D_K);
      Close(STEM_File(D_K));
   
   
      for I in Character'('a')..Character'('z')  loop
         for J in Character'(' ')..Character'(' ')  loop
            Text_Io.Put(Indx_File(D_K), (I, J)); 
            Put(Indx_File(D_K), ' ');
            Put(Indx_File(D_K), Bdlf(I, J, D_K));
            Put(Indx_File(D_K), ' ');
            Put(Indx_File(D_K), Bdll(I, J, D_K));
            Put(Indx_File(D_K), ' ');
            New_Line(Indx_File(D_K));
         end loop;
      end loop;
      for I in Character'('a')..Character'('z')  loop
         for J in Character'('a')..Character'('z')  loop
            Text_Io.Put(Indx_File(D_K), (I, J)); 
            Put(Indx_File(D_K), ' ');
            Put(Indx_File(D_K), Ddlf(I, J, D_K));
            Put(Indx_File(D_K), ' ');
            Put(Indx_File(D_K), Ddll(I, J, D_K));
            Put(Indx_File(D_K), ' ');
            New_Line(Indx_File(D_K));
         end loop;
      end loop;
      Close(Indx_File(D_K));
   
   end MAKESTEM; 
