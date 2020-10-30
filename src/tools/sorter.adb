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

with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Text_IO;
with Direct_IO;
with Latin_Utils.Strings_Package; use Latin_Utils.Strings_Package;
with Latin_Utils.Dictionary_Package; use Latin_Utils.Dictionary_Package;
procedure Sorter is
   --  This program sorts a file of lines (Strings) on 4 subStrings Mx .. Nx
   --  Sort by Stringwise (different cases), numeric, or POS enumeration

   use Ada.Integer_Text_IO;
   use Text_IO;

   Name_Length : constant := 80;
   Enter_Line : String (1 .. Name_Length) := (others => ' ');
   Ls, Last : Integer := 0;
   Input_Name : String (1 .. 80) := (others => ' ');

   Line_Length : constant := 300;
   --  ##################################
   --  Max line length on input file
   --  Shorter => less disk space to sort

   Current_Length : Integer := 0;
   subtype Text_Type is String (1 .. Line_Length);
   --type LINE_TYPE is
   -- record
   --   CURRENT_LENGTH : CURRENT_LINE_LENGTH_TYPE := 0;
   --   TEXT : TEXT_TYPE;
   -- end record;
   package Line_Io is new Direct_IO (Text_Type);
   use Line_Io;
   Blank_Text : constant Text_Type := (others => ' ');

   Line_Text : Text_Type := Blank_Text;
   Old_Line : Text_Type := Blank_Text;
   P_Line : Text_Type := Blank_Text;

   type Sort_Type is (A, C, G, U, N, F, P, S);
   package Sort_Type_Io is new Text_IO.Enumeration_IO (Sort_Type);
   use Sort_Type_Io;

   type Way_Type is (I, D);
   package Way_Type_Io is new Text_IO.Enumeration_IO (Way_Type);
   use Way_Type_Io;

   Input  : Text_IO.File_Type;
   Output : Text_IO.File_Type;
   Work   : Line_Io.File_Type;

   M1, M2, M3, M4 : Natural := 1;
   N1, N2, N3, N4 : Natural := Line_Length;

   S1, S2, S3, S4 : Sort_Type := A;
   W1, W2, W3, W4 : Way_Type := I;

   Entry_Finished : exception;

   --  For section numbering of large documents and standards
   type Section_Type is
      record
         First_Level   : Integer := 0;
         Second_Level  : Integer := 0;
         Third_Level   : Integer := 0;
         Fourth_Level  : Integer := 0;
         Fifth_Level   : Integer := 0;
      end record;

   No_Section : constant Section_Type := (0, 0, 0, 0, 0);

   type Appendix_Type is (None, A, B, C, D, E, F, G, H, I, J, K, L, M,
     N, O, P, Q, R, S, T, U, V, W, X, Y, Z);
   package Appendix_Io is new Text_IO.Enumeration_IO (Appendix_Type);

   type Appendix_Section_Type is    record
      Appendix : Appendix_Type := None;
      Section  : Section_Type  := No_Section;
   end record;

   No_Appendix_Section : constant Appendix_Section_Type :=
     (None, (0, 0, 0, 0, 0));

   --  procedure PUT (OUTPUT : TEXT_IO.FILE_TYPE; S : SECTION_TYPE);
   --  procedure PUT (S : SECTION_TYPE);
   --  procedure GET (FROM : in STRING;
   --                   S : out SECTION_TYPE; LAST : out POSITIVE);
   --  function "<"(A, B : SECTION_TYPE) return BOOLEAN;
   --
   --  procedure PUT (OUTPUT : TEXT_IO.FILE_TYPE; S : APPENDIX_SECTION_TYPE);
   --  procedure PUT (S : APPENDIX_SECTION_TYPE);
   --  procedure GET (FROM : in STRING;
   --                   S : out APPENDIX_SECTION_TYPE; LAST : out POSITIVE);
   --  function "<"(A, B : APPENDIX_SECTION_TYPE) return BOOLEAN;
   --
   procedure Get (From : in String;
                  S : out Section_Type; Last : out Integer) is
      L  : Integer := 0;
      Lt : constant Integer := From'Last;
   begin
      S := No_Section;
      if Trim (From)'Last < From'First   then
         Last := From'First - 1; -- Nothing got processed
         return;   --  Empty String, no data         --  Return default
      end if;

      Get (From, S.First_Level, L);
      if L + 1 >= Lt  then
         Last := L;
         return;
      end if;
      Get (From (L + 2 .. Lt), S.Second_Level, L);
      if L + 1 >= Lt  then
         Last := L;
         return;
      end if;
      Get (From (L + 2 .. Lt), S.Third_Level, L);
      if L + 1 >= Lt  then
         Last := L;
         return;
      end if;
      Get (From (L + 2 .. Lt), S.Fourth_Level, L);
      if L + 1 >= Lt  then
         Last := L;
         return;
      end if;
      Get (From (L + 2 .. Lt), S.Fifth_Level, L);
      Last := L;
      return;
   exception
      when Text_IO.End_Error =>
         Last := L;
         return;
      when Text_IO.Data_Error =>
         Last := L;
         return;
      when others =>
         Put (" Unexpected exception in GET (FROM; SECTION_TYPE)" &
           " with input =>");
         Put (From);
         New_Line;
         Last := L;
         raise;
   end Get;

   procedure Get (From : in String;
                  S : out Appendix_Section_Type; Last : out Integer) is
      use Appendix_Io;
      L  : Integer := 0;
      Ft : constant Integer := From'First;
      Lt : constant Integer := From'Last;
   begin

      S := No_Appendix_Section;
      if (Ft = Lt)  or else
        (Trim (From)'Length = 0)
      then   --  Empty/blank String, no data
         Put ("@");
         Last := From'First - 1; -- Nothing got processed
         return;                      --  Return default
      end if;

      --PUT_LINE ("In GET =>" & FROM & '|');

      begin
         Get (From, S.Appendix, L);
         --PUT ("A");
         if L + 1 >= Lt  then
            Last := L;
            return;
         end if;
      exception
         when others  =>
            S.Appendix := None;
            L := Ft - 2;
      end;

      --    PUT ("B");
      --    GET (FROM (L+2 .. LT), S.SECTION.FIRST_LEVEL, L);
      --    if L+1 >= LT  then
      --      LAST := L;
      --      return;
      --    end if;
      --PUT ("C");
      --    GET (FROM (L+2 .. LT), S.SECTION.SECOND_LEVEL, L);
      --    if L+1 >= LT  then
      --      LAST := L;
      --      return;
      --    end if;
      --PUT ("D");
      --    GET (FROM (L+2 .. LT), S.SECTION.THIRD_LEVEL, L);
      --    if L+1 >= LT  then
      --      LAST := L;
      --      return;
      --    end if;
      --PUT ("E");
      --    GET (FROM (L+2 .. LT), S.SECTION.FOURTH_LEVEL, L);
      --    if L+1 >= LT  then
      --      LAST := L;
      --      return;
      --    end if;
      --PUT ("F");
      --    GET (FROM (L+2 .. LT), S.SECTION.FIFTH_LEVEL, L);
      --    LAST := L;
      --PUT ("G");

      Get (From (L + 2 .. Lt), S.Section, L);
      --PUT ("F");
      return;
   exception
      when Text_IO.End_Error =>
         Last := L;
         return;
      when Text_IO.Data_Error =>
         Last := L;
         return;
      when others =>
         Put
           (" Unexpected exception in GET (FROM; APPENDIX_SECTION_TYPE)" &
           " with input =>");
         Put (From);
         New_Line;
         Last := L;
         return;
   end Get;

   function "<"(A, B : Appendix_Section_Type) return Boolean is
   begin

      if A.Appendix > B.Appendix  then
         return False;
      elsif A.Appendix < B.Appendix  then
         return True;
      else
         if A.Section.First_Level > B.Section.First_Level  then
            return False;
         elsif A.Section.First_Level < B.Section.First_Level  then
            return True;
         else
            if A.Section.Second_Level > B.Section.Second_Level  then
               return False;
            elsif A.Section.Second_Level < B.Section.Second_Level  then
               return True;
            else
               if A.Section.Third_Level > B.Section.Third_Level  then
                  return False;
               elsif A.Section.Third_Level < B.Section.Third_Level  then
                  return True;
               else
                  if A.Section.Fourth_Level > B.Section.Fourth_Level  then
                     return False;
                  elsif A.Section.Fourth_Level < B.Section.Fourth_Level  then
                     return True;
                  else
                     if A.Section.Fifth_Level > B.Section.Fifth_Level  then
                        return False;
                     elsif A.Section.Fifth_Level < B.Section.Fifth_Level  then
                        return True;
                     else
                        return False;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end "<";

   procedure Prompt_For_Entry (Entry_Number : String) is
   begin
      Put ("Give starting column and size of ");
      Put (Entry_Number);
      Put_Line (" significant sort field ");
      Put ("  with optional sort type and way  => ");
   end Prompt_For_Entry;

   procedure Get_Entry (Mx, Nx  : out Natural;
                        Sx  : out Sort_Type;
                        Wx  : out Way_Type) is
      M : Natural := 1;
      N : Natural := Line_Length;
      S : Sort_Type := A;
      W : Way_Type := I;
      Z : Natural := 0;

      procedure Echo_Entry is
      begin
         Put ("                    Sorting on LINE ("); Put (M, 3);
         Put (" .. "); Put (N, 3); Put (")");
         Put ("  with S = "); Put (S); Put (" and W = "); Put (W);
         New_Line (2);
      end Echo_Entry;

   begin

      M := 0;
      N := Line_Length;
      S := A;
      W := I;

      Get_Line (Enter_Line, Ls);
      if Ls = 0  then
         raise Entry_Finished;
      end if;
      Get (Enter_Line (1 .. Ls), M, Last);
      begin
         Get (Enter_Line (Last + 1 .. Ls), Z, Last);
         if  M = 0 or Z = 0  then
            Put_Line ("Start or size of zero, you must be kidding, aborting");
            raise Program_Error;
         elsif M + Z > Line_Length  then
            Put_Line ("Size too large, going to end of line");
            N := Line_Length;
         else
            N := M + Z - 1;
         end if;
         Sort_Type_Io.Get (Enter_Line (Last + 1 .. Ls), S, Last);
         Way_Type_Io.Get (Enter_Line (Last + 1 .. Ls), W, Last);
         Mx := M; Nx := N;  Sx := S; Wx := W;
         Echo_Entry;
         return;
      exception
         when Program_Error  =>
            Put_Line ("PROGRAM_ERROR raised in GET_ENTRY");
            raise;
         when others =>
            Mx := M; Nx := N; Sx := S; Wx := W;
            Echo_Entry;
            return;
      end;
   end Get_Entry;

   function Ignore_Separators (S : String) return String is
      T : String (S'First .. S'Last) := Lower_Case (S);
   begin
      for I in S'First + 1 .. S'Last - 1  loop
         if (S (I - 1) /= '-'  and then S (I - 1) /= '_')  and then
           (S (I) = '-'  or else S (I) = '_')  and then
           (S (I + 1) /= '-'  and then S (I + 1) /= '_')
         then
            T (I) := ' ';
         end if;
      end loop;
      return T;
   end Ignore_Separators;

   function Ltu (C, D : Character) return Boolean is
   begin
      if D = 'v'  then
         if C < 'u'  then
            return True;
         else
            return False;
         end if;
      elsif D = 'j'  then
         if C < 'i'  then
            return True;
         else
            return False;
         end if;
      elsif D = 'V'  then
         if C < 'U'  then
            return True;
         else
            return False;
         end if;
      elsif D = 'J'  then
         if C < 'I'  then
            return True;
         else
            return False;
         end if;
      else
         return C < D;
      end if;
   end Ltu;

   function Equ (C, D : Character) return Boolean is
   begin
      if (D = 'u') or (D = 'v')  then
         if (C = 'u') or (C = 'v')  then
            return True;
         else
            return False;
         end if;
      elsif (D = 'i') or (D = 'j')  then
         if (C = 'i') or (C = 'j')  then
            return True;
         else
            return False;
         end if;
      elsif (D = 'U') or (D = 'V')  then
         if (C = 'U') or (C = 'V')  then
            return True;
         else
            return False;
         end if;
      elsif (D = 'I') or (D = 'J')  then
         if (C = 'I') or (C = 'J')  then
            return True;
         else
            return False;
         end if;
      else
         return C = D;
      end if;
   end Equ;

   function Gtu (C, D : Character) return Boolean is
   begin
      if D = 'u'  then
         if C > 'v'  then
            return True;
         else
            return False;
         end if;
      elsif D = 'i'  then
         if C > 'j'  then
            return True;
         else
            return False;
         end if;
      elsif D = 'U'  then
         if C > 'V'  then
            return True;
         else
            return False;
         end if;
      elsif D = 'I'  then
         if C > 'J'  then
            return True;
         else
            return False;
         end if;
      else
         return C > D;
      end if;
   end Gtu;

   function Ltu (S, T : String) return Boolean is
   begin
      for I in 1 .. S'Length  loop   --  Not TRIMed, so same length
         if Equ (S (S'First + I - 1), T (T'First + I - 1))  then
            null;
         elsif Gtu (S (S'First + I - 1), T (T'First + I - 1))  then
            return False;
         elsif Ltu (S (S'First + I - 1), T (T'First + I - 1))  then
            return True;
         end if;
      end loop;
      return False;
   end Ltu;

   function Gtu (S, T : String) return Boolean is
   begin
      for I in 1 .. S'Length  loop
         if Equ (S (S'First + I - 1), T (T'First + I - 1))  then
            null;
         elsif Ltu (S (S'First + I - 1), T (T'First + I - 1))  then
            return False;
         elsif Gtu (S (S'First + I - 1), T (T'First + I - 1))  then
            return True;
         end if;
      end loop;
      return False;
   end Gtu;

   function Equ (S, T : String) return Boolean is
   begin
      if S'Length /= T'Length  then
         return False;
      end if;

      for I in 1 .. S'Length  loop
         if not Equ (S (S'First + I - 1), T (T'First + I - 1))  then
            return False;
         end if;
      end loop;

      return True;
   end Equ;

   function Slt (X, Y : String;         --  Make LEFT and RIGHT
                 St : Sort_Type := A;
                 Wt : Way_Type := I) return Boolean is
      As : String (X'Range) := X;
      Bs : String (Y'Range) := Y;
      Mn, Nn : Integer := 0;
      Fn, Gn : Float := 0.0;
      --FS, GS : SECTION_TYPE := NO_SECTION;
      Fs, Gs : Appendix_Section_Type := No_Appendix_Section;
      Px, Py : Part_Entry;       --  So I can X here
   begin
      if St = A  then
         As := Lower_Case (As);
         Bs := Lower_Case (Bs);
         if Wt = I  then
            return As < Bs;
         else
            return As > Bs;
         end if;

      elsif St = C  then
         if Wt = I  then
            return As < Bs;
         else
            return As > Bs;
         end if;

      elsif St = G  then
         As := Ignore_Separators (As);
         Bs := Ignore_Separators (Bs);
         if Wt = I  then
            return As < Bs;
         else
            return As > Bs;
         end if;

      elsif St = U  then
         As := Lower_Case (As);
         Bs := Lower_Case (Bs);
         if Wt = I  then
            return Ltu (As, Bs);
         else
            return Gtu (As, Bs);
         end if;

      elsif St = N  then
         Get (As, Mn, Last);
         Get (Bs, Nn, Last);
         if Wt = I  then
            return Mn < Nn;
         else
            return Mn > Nn;
         end if;

      elsif St = F  then
         Ada.Float_Text_IO.Get (As, Fn, Last);
         Ada.Float_Text_IO.Get (Bs, Gn, Last);
         if Wt = I  then
            return Fn < Gn;
         else
            return Fn > Gn;
         end if;

      elsif St = P  then
         Part_Entry_IO.Get (As, Px, Last);
         Part_Entry_IO.Get (Bs, Py, Last);
         if Wt = I  then
            return Px < Py;
         else
            return (not (Px < Py)) and (not (Px = Py));
         end if;

      elsif St = S  then
         --PUT_LINE ("AS =>" & AS & '|');
         Get (As, Fs, Last);
         --PUT_LINE ("BS =>" & BS & '|');
         Get (Bs, Gs, Last);
         --PUT_LINE ("GOT AS & BS");
         if Wt = I  then
            return Fs < Gs;
         else
            return (not (Fs < Gs)) and (not (Fs = Gs));
         end if;

      else
         return False;
      end if;

   exception
      when others  =>
         Text_IO.Put_Line ("exception in SLT    showing LEFT and RIGHT");
         Text_IO.Put_Line (X & "&");
         Text_IO.Put_Line (Y & "|");
         raise;

   end Slt;

   function Sort_Equal (X, Y : String;
                        St : Sort_Type := A;
                        Wt : Way_Type := I) return Boolean
   is
      pragma Unreferenced (Wt);
      As : String (X'Range) := X;
      Bs : String (Y'Range) := Y;
      Mn, Nn : Integer := 0;
      Fn, Gn : Float := 0.0;
      Fs, Gs : Appendix_Section_Type := No_Appendix_Section;
      Px, Py : Part_Entry;
   begin
      if St = A  then
         As := Lower_Case (As);
         Bs := Lower_Case (Bs);
         return As = Bs;

      elsif St = C  then
         return As = Bs;

      elsif St = G  then
         As := Ignore_Separators (As);
         Bs := Ignore_Separators (Bs);
         return As = Bs;

      elsif St = U  then
         As := Lower_Case (As);
         Bs := Lower_Case (Bs);
         return Equ (As,  Bs);

      elsif St = N  then
         Get (As, Mn, Last);
         Get (Bs, Nn, Last);
         return Mn = Nn;

      elsif St = F  then
         Ada.Float_Text_IO.Get (As, Fn, Last);
         Ada.Float_Text_IO.Get (Bs, Gn, Last);
         return Fn = Gn;

      elsif St = P  then
         Part_Entry_IO.Get (As, Px, Last);
         Part_Entry_IO.Get (Bs, Py, Last);
         return Px = Py;

      elsif St = S  then
         Get (As, Fs, Last);
         Get (Bs, Gs, Last);
         return Fs = Gs;

      else
         return False;
      end if;

   exception
      when others  =>
         Text_IO.Put_Line ("exception in LT    showing LEFT and RIGHT");
         Text_IO.Put_Line (X & "|");
         Text_IO.Put_Line (Y & "|");
         raise;

   end Sort_Equal;

   function Lt  (Left, Right : Text_Type) return Boolean is
   begin

      if Slt (Left (M1 .. N1),  Right (M1 .. N1), S1, W1)  then
         return True;
      elsif Sort_Equal (Left (M1 .. N1),  Right (M1 .. N1), S1, W1) then
         if (N2 > 0) and then
           Slt (Left (M2 .. N2),  Right (M2 .. N2), S2, W2)
         then
            return True;
         elsif (N2 > 0) and then
           Sort_Equal (Left (M2 .. N2),  Right (M2 .. N2), S2, W2)
         then
            if (N3 > 0) and then
              Slt (Left (M3 .. N3),  Right (M3 .. N3), S3, W3)
            then
               return True;
            elsif (N3 > 0) and then
              Sort_Equal (Left (M3 .. N3),  Right (M3 .. N3), S3, W3)
            then
               if (N4 > 0) and then
                 Slt (Left (M4 .. N4),  Right (M4 .. N4), S4, W4)
               then
                  return True;
               end if;
            end if;
         end if;
      end if;
      return False;
   exception
      when others =>
         Text_IO.Put_Line ("exception in LT    showing LEFT and RIGHT");
         Text_IO.Put_Line (Left & "|");
         Text_IO.Put_Line (Right & "|");
         raise;
   end Lt;

   procedure Open_File_For_Input (Input : in out Text_IO.File_Type;
                                  Prompt : String := "File for input => ") is
      Last : Natural := 0;
   begin
      Get_Input_File :
      loop
         Check_Input :
            begin
               New_Line;

               Put (Prompt);
               Get_Line (Input_Name, Last);
               Open (Input, In_File, Input_Name (1 .. Last));
               exit Get_Input_File;
            exception
               when others  =>
                  Put_Line ("   !!!!!!!!!  Try Again  !!!!!!!!");
            end Check_Input;
      end loop Get_Input_File;

   end Open_File_For_Input;

   procedure Create_File_For_Output (Output : in out Text_IO.File_Type;
                                     Prompt : String := "File for output => ")
   is
      Name : String (1 .. 80) := (others => ' ');
      Last : Natural := 0;
   begin

      Get_Output_File :
      loop
         Check_Output :
         begin
            New_Line;

            Put (Prompt);
            Get_Line (Name, Last);
            if Trim (Name (1 .. Last))'Length /= 0  then
               Create (Output, Out_File, Name (1 .. Last));
            else
               Create (Output, Out_File, Trim (Input_Name));
            end if;
            exit Get_Output_File;
         exception
            when others  =>
               Put_Line ("   !!!!!!!!!  Try Again  !!!!!!!!");
         end Check_Output;
      end loop Get_Output_File;

   end Create_File_For_Output;

   function Graphic (S : String) return String is
      T : String (1 .. S'Length) := S;
   begin
      for I in S'Range  loop
         if Character'Pos (S (I)) < 32  then
            T (I) := ' ';
         end if;
      end loop;
      return T;
   end Graphic;

begin

   New_Line;
   Put_Line ("Sorts a text file of lines four times on subStrings M .. N");
   Put_Line (
     "A)lphabetic (all case) C)ase sensitive, iG)nore separators, U)i_is_vj,");
   Put_Line ("    iN)teger, F)loating point, S)ection, or P)art entry");
   Put_Line ("         I)ncreasing or D)ecreasing");
   New_Line;

   Open_File_For_Input (Input, "What file to sort from => ");
   New_Line;

   Prompt_For_Entry ("first");
   begin
      Get_Entry (M1, N1, S1, W1);
   exception
      when Program_Error  =>
         raise;
      when others =>
         null;
   end;

   begin
      Prompt_For_Entry ("second");
      Get_Entry (M2, N2, S2, W2);
      Prompt_For_Entry ("third");
      Get_Entry (M3, N3, S3, W3);
      Prompt_For_Entry ("fourth");
      Get_Entry (M4, N4, S4, W4);
   exception
      --when Program_Error  =>
      --   raise;
      when Entry_Finished =>
         null;
      when Text_IO.Data_Error  | Text_IO.End_Error  =>
         null;
   end;

   --PUT_LINE ("CREATING WORK FILE");
   New_Line;
   Create (Work, Inout_File, "WORK.");
   Put_Line ("CREATED  WORK FILE");

   while not End_Of_File (Input)  loop
      --begin
      Get_Line (Input, Line_Text, Current_Length);
      --exception when others  =>
      --TEXT_IO.PUT_LINE ("INPUT GET exception");
      --TEXT_IO.PUT_LINE (LINE_TEXT (1 .. CURRENT_LENGTH) & "|");
      --end;
      --PUT_LINE (LINE_TEXT (1 .. CURRENT_LENGTH));

      if Trim (Line_Text (1 .. Current_Length)) /= ""  then
         --begin
         Write (Work, Head (Line_Text (1 .. Current_Length), Line_Length));
         --exception when others  =>
         --TEXT_IO.PUT_LINE ("WORK WRITE exception");
         --TEXT_IO.PUT_LINE (LINE_TEXT (1 .. CURRENT_LENGTH) & "|");
         --end;
      end if;
   end loop;
   Close (Input);

   Put_Line ("Begin sorting");

   Line_Heapsort :
   declare

      L    : Line_Io.Positive_Count := Size (Work) / 2 + 1;
      Ir   : Line_Io.Positive_Count := Size (Work);
      I, J : Line_Io.Positive_Count;

   begin
      Text_IO.Put_Line ("SIZE OF WORK = " &
        Integer'Image (Integer (Size (Work))));
      Main :
      loop

         if L > 1  then
            L := L - 1;
            Read (Work, Line_Text, L);
            Old_Line := Line_Text;
         else
            Read (Work, Line_Text, Ir);
            Old_Line := Line_Text;
            Read (Work, Line_Text, 1);
            Write (Work, Line_Text, Ir);
            Ir := Ir - 1;
            if Ir = 1 then
               Write (Work, Old_Line, 1);
               exit Main;
            end if;
         end if;
         I := L;
         J := L + L;

         while J <= Ir   loop
            if J < Ir  then
               Read (Work, Line_Text, J);
               Read (Work, P_Line, J + 1);
               --if LT (LINE.TEXT, P_LINE.TEXT)  then
               if Lt (Line_Text, P_Line)  then
                  J := J + 1;
               end if;
            end if;
            Read (Work, Line_Text, J);
            --if OLD_LINE.TEXT < LINE.TEXT  then
            if Lt (Old_Line, Line_Text)  then
               Write (Work, Line_Text, I);
               I := J;
               J := J + J;
            else
               J := Ir + 1;
            end if;
         end loop;
         Write (Work, Old_Line, I);

      end loop Main;

   exception
      when Constraint_Error => Put_Line ("HEAP CONSTRAINT_ERROR");
      when others           => Put_Line ("HEAP other_ERROR");
   end Line_Heapsort;

   Put_Line ("Finished sorting in WORK");

   Create_File_For_Output (Output, "Where to put the output => ");

   --RESET (WORK);
   Set_Index (Work, 1);
   while not End_Of_File (Work)  loop
      Read (Work, Line_Text);
      if Trim (Graphic (Line_Text))'Length > 0  then
         --PUT_LINE (TRIM (LINE_TEXT, RIGHT));
         Put_Line (Output, Trim (Line_Text, Right));
      end if;
   end loop;

   Close (Work);
   Close (Output);
   Put_Line ("Done!");
   New_Line;

exception
   when Program_Error  =>
      Put_Line ("SORT terminated on a PROGRAM_ERROR");
      Close (Output);
   when Text_IO.Data_Error =>     --Terminate on primary start or size = 0
      Put_Line ("SORT terminated on a DATA_ERROR");
      Put_Line (Line_Text);
      Close (Output);
   when Constraint_Error =>       --Terminate on blank line for file name
      Put_Line ("SORT terminated on a CONSTRAINT_ERROR");
      Close (Output);
   when Text_IO.Device_Error  =>     --Ran out of space to write output file
      Put_Line ("SORT terminated on a DEVICE_ERROR");
      Delete (Output);
      Create_File_For_Output (Output, "Wherelse to put the output => ");
      Reset (Work);
      while not End_Of_File (Work)  loop
         Read (Work, Line_Text);
         Put_Line (Output, Line_Text);    --(1 .. LINE.CURRENT_LENGTH));
      end loop;
      Close (Output);
end Sorter;
