with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with PREFACE;
package body INFLECTIONS_PACKAGE is
  use TEXT_IO;


  function "<" (LEFT, RIGHT : DECN_RECORD) return BOOLEAN is
  begin
    if LEFT.WHICH < RIGHT.WHICH  or else
      (LEFT.WHICH = RIGHT.WHICH  and then
       LEFT.VAR < RIGHT.VAR)  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<";

  
  
  function "<" (LEFT, RIGHT : QUALITY_RECORD) return BOOLEAN is
  begin
    if LEFT.POFS = RIGHT.POFS  then
    case LEFT.POFS is
      when N =>
        if LEFT.N.DECL.WHICH < RIGHT.N.DECL.WHICH  or else
          (LEFT.N.DECL.WHICH = RIGHT.N.DECL.WHICH  and then
          LEFT.N.DECL.VAR < RIGHT.N.DECL.VAR)  or else
          (LEFT.N.DECL.WHICH = RIGHT.N.DECL.WHICH  and then
          LEFT.N.DECL.VAR = RIGHT.N.DECL.VAR  and then
          LEFT.N.NUMBER < RIGHT.N.NUMBER) or else
          (LEFT.N.DECL.WHICH = RIGHT.N.DECL.WHICH  and then
          LEFT.N.DECL.VAR = RIGHT.N.DECL.VAR  and then
          LEFT.N.NUMBER = RIGHT.N.NUMBER and then
          LEFT.N.CS < RIGHT.N.CS) or else
         (LEFT.N.DECL.WHICH = RIGHT.N.DECL.WHICH  and then
          LEFT.N.DECL.VAR = RIGHT.N.DECL.VAR  and then
          LEFT.N.NUMBER = RIGHT.N.NUMBER and then
          LEFT.N.CS = RIGHT.N.CS and then
          LEFT.N.GENDER < RIGHT.N.GENDER)  then
          return TRUE;
        end if;
      when PRON =>
        if LEFT.PRON.DECL.WHICH < RIGHT.PRON.DECL.WHICH  or else
          (LEFT.PRON.DECL.WHICH = RIGHT.PRON.DECL.WHICH  and then
          LEFT.PRON.DECL.VAR < RIGHT.PRON.DECL.VAR)  or else
          (LEFT.PRON.DECL.WHICH = RIGHT.PRON.DECL.WHICH  and then
          LEFT.PRON.DECL.VAR = RIGHT.PRON.DECL.VAR  and then
          LEFT.PRON.NUMBER < RIGHT.PRON.NUMBER) or else
          (LEFT.PRON.DECL.WHICH = RIGHT.PRON.DECL.WHICH  and then
          LEFT.PRON.DECL.VAR = RIGHT.PRON.DECL.VAR  and then
          LEFT.PRON.NUMBER = RIGHT.PRON.NUMBER and then
          LEFT.PRON.CS < RIGHT.PRON.CS) or else
         (LEFT.PRON.DECL.WHICH = RIGHT.PRON.DECL.WHICH  and then
          LEFT.PRON.DECL.VAR = RIGHT.PRON.DECL.VAR  and then
          LEFT.PRON.NUMBER = RIGHT.PRON.NUMBER and then
          LEFT.PRON.CS = RIGHT.PRON.CS and then
          LEFT.PRON.GENDER < RIGHT.PRON.GENDER) then
          return TRUE;
        end if;
      when PACK =>
        if LEFT.PACK.DECL.WHICH < RIGHT.PACK.DECL.WHICH  or else
          (LEFT.PACK.DECL.WHICH = RIGHT.PACK.DECL.WHICH  and then
          LEFT.PACK.DECL.VAR < RIGHT.PACK.DECL.VAR)  or else
          (LEFT.PACK.DECL.WHICH = RIGHT.PACK.DECL.WHICH  and then
          LEFT.PACK.DECL.VAR = RIGHT.PACK.DECL.VAR  and then
          LEFT.PACK.NUMBER < RIGHT.PACK.NUMBER) or else
          (LEFT.PACK.DECL.WHICH = RIGHT.PACK.DECL.WHICH  and then
          LEFT.PACK.DECL.VAR = RIGHT.PACK.DECL.VAR  and then
          LEFT.PACK.NUMBER = RIGHT.PACK.NUMBER and then
          LEFT.PACK.CS < RIGHT.PACK.CS) or else
         (LEFT.PACK.DECL.WHICH = RIGHT.PACK.DECL.WHICH  and then
          LEFT.PACK.DECL.VAR = RIGHT.PACK.DECL.VAR  and then
          LEFT.PACK.NUMBER = RIGHT.PACK.NUMBER and then
          LEFT.PACK.CS = RIGHT.PACK.CS and then
          LEFT.PACK.GENDER < RIGHT.PACK.GENDER)   then
          return TRUE;
        end if;
      when ADJ =>
        if LEFT.ADJ.DECL.WHICH < RIGHT.ADJ.DECL.WHICH  or else
          (LEFT.ADJ.DECL.WHICH = RIGHT.ADJ.DECL.WHICH  and then
          LEFT.ADJ.DECL.VAR < RIGHT.ADJ.DECL.VAR)  or else
          (LEFT.ADJ.DECL.WHICH = RIGHT.ADJ.DECL.WHICH  and then
          LEFT.ADJ.DECL.VAR = RIGHT.ADJ.DECL.VAR  and then
          LEFT.ADJ.NUMBER < RIGHT.ADJ.NUMBER) or else
          (LEFT.ADJ.DECL.WHICH = RIGHT.ADJ.DECL.WHICH  and then
          LEFT.ADJ.DECL.VAR = RIGHT.ADJ.DECL.VAR  and then
          LEFT.ADJ.NUMBER = RIGHT.ADJ.NUMBER and then
          LEFT.ADJ.CS < RIGHT.ADJ.CS) or else
         (LEFT.ADJ.DECL.WHICH = RIGHT.ADJ.DECL.WHICH  and then
          LEFT.ADJ.DECL.VAR = RIGHT.ADJ.DECL.VAR  and then
          LEFT.ADJ.NUMBER = RIGHT.ADJ.NUMBER and then
          LEFT.ADJ.CS = RIGHT.ADJ.CS and then
          LEFT.ADJ.GENDER < RIGHT.ADJ.GENDER)  or else
         (LEFT.ADJ.DECL.WHICH = RIGHT.ADJ.DECL.WHICH  and then
          LEFT.ADJ.DECL.VAR = RIGHT.ADJ.DECL.VAR  and then
          LEFT.ADJ.NUMBER = RIGHT.ADJ.NUMBER and then
          LEFT.ADJ.CS = RIGHT.ADJ.CS and then
          LEFT.ADJ.GENDER = RIGHT.ADJ.GENDER  and then
          LEFT.ADJ.CO < RIGHT.ADJ.CO)   then
          return TRUE;
        end if;
      when ADV =>
        return LEFT.ADV.CO < RIGHT.ADV.CO;
      when V =>
        if (LEFT.V.CON.WHICH < RIGHT.V.CON.WHICH)  or else
          (LEFT.V.CON.WHICH = RIGHT.V.CON.WHICH  and then
          LEFT.V.CON.VAR < RIGHT.V.CON.VAR)  or else
          (LEFT.V.CON.WHICH = RIGHT.V.CON.WHICH  and then
          LEFT.V.CON.VAR = RIGHT.V.CON.VAR  and then
          LEFT.V.NUMBER < RIGHT.V.NUMBER) or else
          (LEFT.V.CON.WHICH = RIGHT.V.CON.WHICH  and then
          LEFT.V.CON.VAR = RIGHT.V.CON.VAR  and then
          LEFT.V.NUMBER = RIGHT.V.NUMBER and then
LEFT.V.TENSE_VOICE_MOOD.TENSE < RIGHT.V.TENSE_VOICE_MOOD.TENSE) or else
         (LEFT.V.CON.WHICH = RIGHT.V.CON.WHICH  and then
          LEFT.V.CON.VAR = RIGHT.V.CON.VAR  and then
          LEFT.V.NUMBER = RIGHT.V.NUMBER and then
 LEFT.V.TENSE_VOICE_MOOD.TENSE = RIGHT.V.TENSE_VOICE_MOOD.TENSE and then
 LEFT.V.TENSE_VOICE_MOOD.VOICE < RIGHT.V.TENSE_VOICE_MOOD.VOICE) or else
         (LEFT.V.CON.WHICH = RIGHT.V.CON.WHICH  and then
          LEFT.V.CON.VAR = RIGHT.V.CON.VAR  and then
          LEFT.V.NUMBER = RIGHT.V.NUMBER and then
LEFT.V.TENSE_VOICE_MOOD.TENSE = RIGHT.V.TENSE_VOICE_MOOD.TENSE and then
LEFT.V.TENSE_VOICE_MOOD.VOICE = RIGHT.V.TENSE_VOICE_MOOD.VOICE and then
LEFT.V.TENSE_VOICE_MOOD.MOOD   < RIGHT.V.TENSE_VOICE_MOOD.MOOD )  or else
         (LEFT.V.CON.WHICH = RIGHT.V.CON.WHICH  and then
          LEFT.V.CON.VAR = RIGHT.V.CON.VAR  and then
          LEFT.V.NUMBER = RIGHT.V.NUMBER and then
LEFT.V.TENSE_VOICE_MOOD.TENSE = RIGHT.V.TENSE_VOICE_MOOD.TENSE and then
LEFT.V.TENSE_VOICE_MOOD.VOICE = RIGHT.V.TENSE_VOICE_MOOD.VOICE and then
LEFT.V.TENSE_VOICE_MOOD.MOOD   = RIGHT.V.TENSE_VOICE_MOOD.MOOD   and then
          LEFT.V.PERSON < RIGHT.V.PERSON)   then
          return TRUE;
        end if;
      when VPAR =>
        if LEFT.VPAR.CON.WHICH < RIGHT.VPAR.CON.WHICH  or else
          (LEFT.VPAR.CON.WHICH = RIGHT.VPAR.CON.WHICH  and then
          LEFT.VPAR.CON.VAR < RIGHT.VPAR.CON.VAR)  or else
          (LEFT.VPAR.CON.WHICH = RIGHT.VPAR.CON.WHICH  and then
          LEFT.VPAR.CON.VAR = RIGHT.VPAR.CON.VAR  and then
          LEFT.VPAR.NUMBER < RIGHT.VPAR.NUMBER) or else
          (LEFT.VPAR.CON.WHICH = RIGHT.VPAR.CON.WHICH  and then
          LEFT.VPAR.CON.VAR = RIGHT.VPAR.CON.VAR  and then
          LEFT.VPAR.NUMBER = RIGHT.VPAR.NUMBER and then
          LEFT.VPAR.CS < RIGHT.VPAR.CS) or else
         (LEFT.VPAR.CON.WHICH = RIGHT.VPAR.CON.WHICH  and then
          LEFT.VPAR.CON.VAR = RIGHT.VPAR.CON.VAR  and then
          LEFT.VPAR.NUMBER = RIGHT.VPAR.NUMBER and then
          LEFT.VPAR.CS = RIGHT.VPAR.CS and then
          LEFT.VPAR.GENDER < RIGHT.VPAR.GENDER)  then
          return TRUE;
        end if;
      when SUPINE =>
        if LEFT.SUPINE.CON.WHICH < RIGHT.SUPINE.CON.WHICH  or else
          (LEFT.SUPINE.CON.WHICH = RIGHT.SUPINE.CON.WHICH  and then
          LEFT.SUPINE.CON.VAR < RIGHT.SUPINE.CON.VAR)  or else
          (LEFT.SUPINE.CON.WHICH = RIGHT.SUPINE.CON.WHICH  and then
          LEFT.SUPINE.CON.VAR = RIGHT.SUPINE.CON.VAR  and then
          LEFT.SUPINE.NUMBER < RIGHT.SUPINE.NUMBER) or else
          (LEFT.SUPINE.CON.WHICH = RIGHT.SUPINE.CON.WHICH  and then
          LEFT.SUPINE.CON.VAR = RIGHT.SUPINE.CON.VAR  and then
          LEFT.SUPINE.NUMBER = RIGHT.SUPINE.NUMBER and then
          LEFT.SUPINE.CS < RIGHT.SUPINE.CS) or else
         (LEFT.SUPINE.CON.WHICH = RIGHT.SUPINE.CON.WHICH  and then
          LEFT.SUPINE.CON.VAR = RIGHT.SUPINE.CON.VAR  and then
          LEFT.SUPINE.NUMBER = RIGHT.SUPINE.NUMBER and then
          LEFT.SUPINE.CS = RIGHT.SUPINE.CS and then
          LEFT.SUPINE.GENDER < RIGHT.SUPINE.GENDER)  then
          return TRUE;
        end if;
      when PREP =>
        return LEFT.PREP.OBJ < RIGHT.PREP.OBJ;
      when CONJ =>
        null;
      when INTERJ =>
        null;
      when NUM =>
        if LEFT.NUM.DECL.WHICH < RIGHT.NUM.DECL.WHICH  or else
          (LEFT.NUM.DECL.WHICH = RIGHT.NUM.DECL.WHICH  and then
          LEFT.NUM.DECL.VAR < RIGHT.NUM.DECL.VAR)  or else
          (LEFT.NUM.DECL.WHICH = RIGHT.NUM.DECL.WHICH  and then
          LEFT.NUM.DECL.VAR = RIGHT.NUM.DECL.VAR  and then
          LEFT.NUM.NUMBER < RIGHT.NUM.NUMBER) or else
          (LEFT.NUM.DECL.WHICH = RIGHT.NUM.DECL.WHICH  and then
          LEFT.NUM.DECL.VAR = RIGHT.NUM.DECL.VAR  and then
          LEFT.NUM.NUMBER = RIGHT.NUM.NUMBER and then
          LEFT.NUM.CS < RIGHT.NUM.CS) or else
         (LEFT.NUM.DECL.WHICH = RIGHT.NUM.DECL.WHICH  and then
          LEFT.NUM.DECL.VAR = RIGHT.NUM.DECL.VAR  and then
          LEFT.NUM.NUMBER = RIGHT.NUM.NUMBER and then
          LEFT.NUM.CS = RIGHT.NUM.CS and then
          LEFT.NUM.GENDER < RIGHT.NUM.GENDER)  or else
         (LEFT.NUM.DECL.WHICH = RIGHT.NUM.DECL.WHICH  and then
          LEFT.NUM.DECL.VAR = RIGHT.NUM.DECL.VAR  and then
          LEFT.NUM.NUMBER = RIGHT.NUM.NUMBER and then
          LEFT.NUM.CS = RIGHT.NUM.CS and then
          LEFT.NUM.GENDER = RIGHT.NUM.GENDER  and then
          LEFT.NUM.SORT < RIGHT.NUM.SORT)   then
          return TRUE;
        end if;
      when TACKON =>
        null;
      when PREFIX =>
        null;
      when SUFFIX =>
        null;
      when others =>
        null;
    end case;
    else
      return LEFT.POFS < RIGHT.POFS;
    end if;
    return FALSE;
  exception
    when CONSTRAINT_ERROR  =>
      return LEFT.POFS < RIGHT.POFS;
  end "<";


  function "<=" (LEFT, RIGHT : PART_OF_SPEECH_TYPE) return BOOLEAN is
  begin
    if RIGHT = LEFT  or else
      (LEFT = PACK and RIGHT = PRON)  or else
       RIGHT = X    then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";

  function "<=" (LEFT, RIGHT : DECN_RECORD) return BOOLEAN is
  begin
    if RIGHT = LEFT  or else
      (RIGHT = DECN_RECORD'(0, 0)  and LEFT.WHICH /= 9)  or else
       RIGHT = DECN_RECORD'(LEFT.WHICH, 0)  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";

  function "<=" (LEFT, RIGHT : GENDER_TYPE) return BOOLEAN is
  begin
    if RIGHT = LEFT  or else
       RIGHT = X     or else
      (RIGHT = C  and then (LEFT = M or LEFT = F))  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";

  function "<=" (LEFT, RIGHT : CASE_TYPE) return BOOLEAN is
  begin
    if RIGHT = LEFT  or else
       RIGHT = X  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";

  function "<=" (LEFT, RIGHT : NUMBER_TYPE) return BOOLEAN is
  begin
    if RIGHT = LEFT  or else
       RIGHT = X  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";

  function "<=" (LEFT, RIGHT : PERSON_TYPE) return BOOLEAN is
  begin
    if RIGHT = LEFT  or else
       RIGHT = 0  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";

  function "<=" (LEFT, RIGHT : COMPARISON_TYPE) return BOOLEAN is
  begin
    if RIGHT = LEFT  or else
       RIGHT = X  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";

  function "<=" (LEFT, RIGHT : TENSE_VOICE_MOOD_RECORD)  return BOOLEAN is
  begin
    if (RIGHT.TENSE = LEFT.TENSE  or else
        RIGHT.TENSE = X)                    and then
       (RIGHT.VOICE  = LEFT.VOICE   or else
        RIGHT.VOICE  = X)                    and then
       (RIGHT.MOOD = LEFT.MOOD  or else
        RIGHT.MOOD = X)                        then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";

  function "<=" (LEFT, RIGHT : NOUN_KIND_TYPE)   return BOOLEAN is
  begin
    if (RIGHT = LEFT   or else
        RIGHT = X)  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";


  function "<=" (LEFT, RIGHT : PRONOUN_KIND_TYPE)   return BOOLEAN is
  begin
    if (RIGHT = LEFT   or else
        RIGHT = X)  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";


  function "<=" (LEFT, RIGHT : VERB_KIND_TYPE)   return BOOLEAN is
  begin
    if (RIGHT = LEFT   or else
        RIGHT = X)  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";


  function "<=" (LEFT, RIGHT : NUMERAL_SORT_TYPE)   return BOOLEAN is
  begin
    if (RIGHT = LEFT   or else
        RIGHT = X)  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";


  function "<=" (LEFT, RIGHT : STEM_KEY_TYPE)   return BOOLEAN is
  begin            --  Only works for 2 stem parts, not verbs
    if (RIGHT = LEFT   or else
        RIGHT = 0)  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";

  function "<=" (LEFT, RIGHT : AGE_TYPE) return BOOLEAN is
  begin
    if RIGHT = LEFT  or else
       RIGHT = X  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";


  function "<=" (LEFT, RIGHT : FREQUENCY_TYPE) return BOOLEAN is
  begin
    if RIGHT = LEFT  or else
       RIGHT = X  then
      return TRUE;
    else
      return FALSE;
    end if;
  end "<=";
  
  
  

package body STEM_TYPE_IO is
    procedure GET(F : in FILE_TYPE; D : out STEM_TYPE) is
      C : CHARACTER := ' ';
    begin
      D := NULL_STEM_TYPE;
      for I in 1..STEM_TYPE_IO.DEFAULT_WIDTH  loop
        GET(F, C);
        if (C not in 'A'..'Z') and (C not in 'a'..'z')  then
          exit;
        else 
          D(I) := C;
        end if;
      end loop;
    end GET;
        
            
    procedure GET(D : out STEM_TYPE) is
      C : CHARACTER := ' ';
    begin
      D := NULL_STEM_TYPE;
      for I in 1..STEM_TYPE_IO.DEFAULT_WIDTH  loop
        TEXT_IO.GET(C);
        if (C not in 'A'..'Z') and (C not in 'a'..'z')  then
          exit;
        else 
          D(I) := C;
        end if;
      end loop;
    end GET;
    
    procedure PUT(F : in FILE_TYPE; D : in STEM_TYPE) is
    begin 
      TEXT_IO.PUT(F, D);
    end PUT;
      
    procedure PUT(D : in STEM_TYPE) is
    begin 
      TEXT_IO.PUT(D);
    end PUT;
      
    procedure GET(S : in STRING; D : out STEM_TYPE; 
                                 LAST : out INTEGER) is
      C : CHARACTER;
    begin 
      D := NULL_STEM_TYPE;
      LAST := 0;
      for I in 1..STEM_TYPE_IO.DEFAULT_WIDTH  loop
        C := S(I);
        if (C not in 'A'..'Z') and (C not in 'a'..'z')  then
          exit;
        else 
          D(I) := C;
          LAST := I;
        end if;
      end loop;
    end GET;
                                   
    procedure PUT(S : out STRING; D : in STEM_TYPE) is
    begin 
      S(S'FIRST..S'FIRST+STEM_TYPE_IO.DEFAULT_WIDTH-1) := D;
    end PUT;
        
  end STEM_TYPE_IO;  
  

package body DECN_RECORD_IO is
--  This package will carry the documentation for all the following packages
  --  Must have "use" for _IO for each of the components of the record
  use INTEGER_IO;
  --  This is a dummy used to GET the space character PUT between components
  SPACER : CHARACTER := ' ';

--  The standard 6 procedures are defined as in TEXT_IO

  procedure GET(F : in FILE_TYPE; D : out DECN_RECORD) is
  --  Get from a file
  begin
    --  Get the first component
    GET(F, D.WHICH);
    --  Then Get (and ignore) space character which is Put between components
    GET(F, SPACER);
    --  Get the next component
    GET(F, D.VAR);
  end GET;

  procedure GET(D : out DECN_RECORD) is
  --  Get from the current input, in the same manner
  begin
    GET(D.WHICH);
    GET(SPACER);
    GET(D.VAR);
  end GET;

  procedure PUT(F : in FILE_TYPE; D : in DECN_RECORD) is
  --  Put to a file
  begin
    --  Put the first component, with whatever Put is applicable (and use'd)
    PUT(F, D.WHICH, 1);
    --  Put the blank character between components
    PUT(F, ' ');
    --  Put the next component
    PUT(F, D.VAR, 1);
  end PUT;

  procedure PUT(D : in DECN_RECORD) is
  --  Likewise for Put to current output
  begin
    PUT(D.WHICH, 1);
    PUT(' ');
    PUT(D.VAR, 1);
  end PUT;

  procedure GET(S : in STRING;
                D : out DECN_RECORD; LAST : out INTEGER) is
  --  Get from a string
    --  Initialize the string position parameter
    --  Make it first-1 so the first string specification looks like later ones
    L : INTEGER := S'FIRST - 1;
  begin
    --  Get with the use'd _IO package the first component
    GET(S(L+1..S'LAST), D.WHICH, L);
    --  The L is the last position read, so add one to skip the spacer
    L := L + 1;
    --  Get the next component
    GET(S(L+1..S'LAST), D.VAR, LAST);
  end GET;

  procedure PUT(S : out STRING; D : in DECN_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    --  Make a place the DEFAULT_WIDTH of the component  to be Put
    --  The DEFAULT_WIDTH has been set for these _IO packages to be 
    --  the LONGEST component width, not the normal Ada default
    M := L + 1;           --  But WHICH is to be PUT WIDTH 1
    --  Put onto the substring that is exactly the DEFAULT (LONGEST) size
    PUT(S(L+1..M), D.WHICH);
    --  Advance the position by 1 to the position to make the blank
    L := M + 1;
    --  Write the blank
    S(L) :=  ' ';
    --  Calculate the next substring, of DEFAULT_WIDTH for next component
    M := L + 1;
    --  Put the next component
    PUT(S(L+1..M), D.VAR);
    --  The following may be necessary to fill the out string
    --  but usually the out string has been specified exactly 
    S(M+1..S'LAST) := (others => ' ');
  end PUT;

end DECN_RECORD_IO;

package body TENSE_VOICE_MOOD_RECORD_IO is
  use TENSE_TYPE_IO;
  use VOICE_TYPE_IO;
  use MOOD_TYPE_IO;
  SPACER : CHARACTER := ' ';

  procedure GET(F : in FILE_TYPE; T : out TENSE_VOICE_MOOD_RECORD) is
  begin
    GET(F, T.TENSE);
    GET(F, SPACER);
    GET(F, T.VOICE);
    GET(F, SPACER);
    GET(F, T.MOOD);
  end GET;

  procedure GET(T : out TENSE_VOICE_MOOD_RECORD) is
  begin
    GET(T.TENSE);
    GET(SPACER);
    GET(T.VOICE);
    GET(SPACER);
    GET(T.MOOD);
  end GET;

   procedure PUT(F : in FILE_TYPE; T : in TENSE_VOICE_MOOD_RECORD) is
   begin
     PUT(F, T.TENSE);
     PUT(F, ' ');
     PUT(F, T.VOICE);
     PUT(F, ' ');
     PUT(F, T.MOOD);
  end PUT;

  procedure PUT(T : in TENSE_VOICE_MOOD_RECORD) is
  begin
    PUT(T.TENSE);
    PUT(' ');
    PUT(T.VOICE);
    PUT(' ');
    PUT(T.MOOD);
  end PUT;

  procedure GET(S : in STRING;
                T : out TENSE_VOICE_MOOD_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), T.TENSE, L);
    L := L + 1;
    GET(S(L+1..S'LAST), T.VOICE, L);
    L := L + 1;
    GET(S(L+1..S'LAST), T.MOOD, LAST);
  end GET;

  procedure PUT(S : out STRING; T : in TENSE_VOICE_MOOD_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + TENSE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), T.TENSE);
    L := M + 1;
    S(L) :=  ' ';
    M := L + VOICE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), T.VOICE);
    L := M + 1;
    S(L) :=  ' ';
    M := L + MOOD_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), T.MOOD);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;

end TENSE_VOICE_MOOD_RECORD_IO;


package body NOUN_RECORD_IO is
  use DECN_RECORD_IO;
  use CASE_TYPE_IO;
  use GENDER_TYPE_IO;
  use NUMBER_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; N : out NOUN_RECORD) is
  begin
    GET(F, N.DECL);
    GET(F, SPACER);
    GET(F, N.CS);
    GET(F, SPACER);
    GET(F, N.NUMBER);
    GET(F, SPACER);
    GET(F, N.GENDER);
  end GET;

  procedure GET(N : out NOUN_RECORD) is
  begin
    GET(N.DECL);
    GET(SPACER);
    GET(N.CS);
    GET(SPACER);
    GET(N.NUMBER);
    GET(SPACER);
    GET(N.GENDER);
  end GET;

  procedure PUT(F : in FILE_TYPE; N : in NOUN_RECORD) is
  begin
    PUT(F, N.DECL);
    PUT(F, ' ');
    PUT(F, N.CS);
    PUT(F, ' ');
    PUT(F, N.NUMBER);
    PUT(F, ' ');
    PUT(F, N.GENDER);
  end PUT;

  procedure PUT(N : in NOUN_RECORD) is
  begin
    PUT(N.DECL);
    PUT(' ');
    PUT(N.CS);
    PUT(' ');
    PUT(N.NUMBER);
    PUT(' ');
    PUT(N.GENDER);
  end PUT;

  procedure GET(S : in STRING; N : out NOUN_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), N.DECL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), N.CS, L);
    L := L + 1;
    GET(S(L+1..S'LAST), N.NUMBER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), N.GENDER, LAST);
  end GET;

  procedure PUT(S : out STRING; N : in NOUN_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), N.DECL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + CASE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), N.CS);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NUMBER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), N.NUMBER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + GENDER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), N.GENDER);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end NOUN_RECORD_IO;


package body PRONOUN_RECORD_IO is
  use DECN_RECORD_IO;
  use CASE_TYPE_IO;
  use GENDER_TYPE_IO;
  use NUMBER_TYPE_IO;
  SPACER : CHARACTER := ' ';

  procedure GET(F : in FILE_TYPE; P : out PRONOUN_RECORD) is
  begin
    GET(F, P.DECL);
    GET(F, SPACER);
    GET(F, P.CS);
    GET(F, SPACER);
    GET(F, P.NUMBER);
    GET(F, SPACER);
    GET(F, P.GENDER);
  end GET;

  procedure GET(P : out PRONOUN_RECORD) is
  begin
    GET(P.DECL);
    GET(SPACER);
    GET(P.CS);
    GET(SPACER);
    GET(P.NUMBER);
    GET(SPACER);
    GET(P.GENDER);
  end GET;

  procedure PUT(F : in FILE_TYPE; P : in PRONOUN_RECORD) is
  begin
    PUT(F, P.DECL);
    PUT(F, ' ');
    PUT(F, P.CS);
    PUT(F, ' ');
    PUT(F, P.NUMBER);
    PUT(F, ' ');
    PUT(F, P.GENDER);
  end PUT;

  procedure PUT(P : in PRONOUN_RECORD) is
  begin
    PUT(P.DECL);
    PUT(' ');
    PUT(P.CS);
    PUT(' ');
    PUT(P.NUMBER);
    PUT(' ');
    PUT(P.GENDER);
  end PUT;

  procedure GET(S : in STRING; P : out PRONOUN_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), P.DECL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.CS, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.NUMBER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.GENDER, LAST);
  end GET;

  procedure PUT(S : out STRING; P : in PRONOUN_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.DECL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + CASE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.CS);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NUMBER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.NUMBER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + GENDER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.GENDER);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end PRONOUN_RECORD_IO;


package body PROPACK_RECORD_IO is
  use DECN_RECORD_IO;
  use CASE_TYPE_IO;
  use NUMBER_TYPE_IO;
  use GENDER_TYPE_IO;
  SPACER : CHARACTER := ' ';

  procedure GET(F : in FILE_TYPE; P : out PROPACK_RECORD) is
  begin
    GET(F, P.DECL);
    GET(F, SPACER);
    GET(F, P.CS);
    GET(F, SPACER);
    GET(F, P.NUMBER);
    GET(F, SPACER);
    GET(F, P.GENDER);
  end GET;

  procedure GET(P : out PROPACK_RECORD) is
  begin
    GET(P.DECL);
    GET(SPACER);
    GET(P.CS);
    GET(SPACER);
    GET(P.NUMBER);
    GET(SPACER);
    GET(P.GENDER);
  end GET;

  procedure PUT(F : in FILE_TYPE; P : in PROPACK_RECORD) is
  begin
    PUT(F, P.DECL);
    PUT(F, ' ');
    PUT(F, P.CS);
    PUT(F, ' ');
    PUT(F, P.NUMBER);
    PUT(F, ' ');
    PUT(F, P.GENDER);
  end PUT;

  procedure PUT(P : in PROPACK_RECORD) is
  begin
    PUT(P.DECL);
    PUT(' ');
    PUT(P.CS);
    PUT(' ');
    PUT(P.NUMBER);
    PUT(' ');
    PUT(P.GENDER);
  end PUT;

  procedure GET(S : in STRING; P : out PROPACK_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), P.DECL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.CS, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.NUMBER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.GENDER, LAST);
  end GET;

  procedure PUT(S : out STRING; P : in PROPACK_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.DECL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + CASE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.CS);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NUMBER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.NUMBER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + GENDER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.GENDER);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end PROPACK_RECORD_IO;


package body ADJECTIVE_RECORD_IO is
  use DECN_RECORD_IO;
  use GENDER_TYPE_IO;
  use CASE_TYPE_IO;
  use NUMBER_TYPE_IO;
  use COMPARISON_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; A : out ADJECTIVE_RECORD) is
  begin
    GET(F, A.DECL);
    GET(F, SPACER);
    GET(F, A.CS);
    GET(F, SPACER);
    GET(F, A.NUMBER);
    GET(F, SPACER);
    GET(F, A.GENDER);
    GET(F, SPACER);
    GET(F, A.CO);
  end GET;

  procedure GET(A : out ADJECTIVE_RECORD) is
  begin
    GET(A.DECL);
    GET(SPACER);
    GET(A.CS);
    GET(SPACER);
    GET(A.NUMBER);
    GET(SPACER);
    GET(A.GENDER);
    GET(SPACER);
    GET(A.CO);
  end GET;

  procedure PUT(F : in FILE_TYPE; A : in ADJECTIVE_RECORD) is
  begin
    PUT(F, A.DECL);
    PUT(F, ' ');
    PUT(F, A.CS);
    PUT(F, ' ');
    PUT(F, A.NUMBER);
    PUT(F, ' ');
    PUT(F, A.GENDER);
    PUT(F, ' ');
    PUT(F, A.CO);
  end PUT;

  procedure PUT(A : in ADJECTIVE_RECORD) is
  begin
    PUT(A.DECL);
    PUT(' ');
    PUT(A.CS);
    PUT(' ');
    PUT(A.NUMBER);
    PUT(' ');
    PUT(A.GENDER);
    PUT(' ');
    PUT(A.CO);
  end PUT;

  procedure GET(S : in STRING; A : out ADJECTIVE_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), A.DECL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), A.CS, L);
    L := L + 1;
    GET(S(L+1..S'LAST), A.NUMBER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), A.GENDER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), A.CO, LAST);
  end GET;

  procedure PUT(S : out STRING; A : in ADJECTIVE_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), A.DECL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + CASE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), A.CS);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NUMBER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), A.NUMBER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + GENDER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), A.GENDER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + COMPARISON_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), A.CO);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end ADJECTIVE_RECORD_IO;


 
package body NUMERAL_RECORD_IO is
  use DECN_RECORD_IO;
  use CASE_TYPE_IO;
  use NUMBER_TYPE_IO;
  use GENDER_TYPE_IO;
  use NUMERAL_SORT_TYPE_IO;
  use GENDER_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; NUM : out NUMERAL_RECORD) is
  begin
    GET(F, NUM.DECL);
    GET(F, SPACER);
    GET(F, NUM.CS);
    GET(F, SPACER);
    GET(F, NUM.NUMBER);
    GET(F, SPACER);
    GET(F, NUM.GENDER);
    GET(F, SPACER);
    GET(F, NUM.SORT);
  end GET;

  procedure GET(NUM : out NUMERAL_RECORD) is
  begin
    GET(NUM.DECL);
    GET(SPACER);                                                                             
    GET(SPACER);
    GET(NUM.NUMBER);
    GET(SPACER);
    GET(NUM.GENDER);
    GET(SPACER);
    GET(NUM.SORT);
 end GET;

  procedure PUT(F : in FILE_TYPE; NUM : in NUMERAL_RECORD) is
  begin
    PUT(F, NUM.DECL);
    PUT(F, ' ');
    PUT(F, NUM.CS);
    PUT(F, ' ');
    PUT(F, NUM.NUMBER);
    PUT(F, ' ');
    PUT(F, NUM.GENDER);
    PUT(F, ' ');
    PUT(F, NUM.SORT);
  end PUT;

  procedure PUT(NUM : in NUMERAL_RECORD) is
  begin
    PUT(NUM.DECL);
    PUT(' ');
    PUT(NUM.CS);
    PUT(' ');
    PUT(NUM.NUMBER);
    PUT(' ');
    PUT(NUM.GENDER);
    PUT(' ');
    PUT(NUM.SORT);
  end PUT;

  procedure GET(S : in STRING; NUM : out NUMERAL_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), NUM.DECL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), NUM.CS, L);
    L := L + 1;
    GET(S(L+1..S'LAST), NUM.NUMBER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), NUM.GENDER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), NUM.SORT, LAST);
  end GET;

  procedure PUT(S : out STRING; NUM : in NUMERAL_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), NUM.DECL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + CASE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), NUM.CS);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NUMBER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), NUM.NUMBER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + GENDER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), NUM.GENDER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NUMERAL_SORT_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), NUM.SORT);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end NUMERAL_RECORD_IO;



package body ADVERB_RECORD_IO is
  use COMPARISON_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; A : out ADVERB_RECORD) is
  begin
    GET(F, A.CO);
  end GET;

  procedure GET(A : out ADVERB_RECORD) is
  begin
    GET(A.CO);
  end GET;

  procedure PUT(F : in FILE_TYPE; A : in ADVERB_RECORD) is
  begin
    PUT(F, A.CO);
  end PUT;

  procedure PUT(A : in ADVERB_RECORD) is
  begin
    PUT(A.CO);
  end PUT;

  procedure GET(S : in STRING; A : out ADVERB_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), A.CO, LAST);
  end GET;

  procedure PUT(S : out STRING; A : in ADVERB_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + COMPARISON_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), A.CO);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end ADVERB_RECORD_IO;


package body VERB_RECORD_IO is
  use DECN_RECORD_IO;
  use TENSE_VOICE_MOOD_RECORD_IO;
  use PERSON_TYPE_IO;
  use NUMBER_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; V : out VERB_RECORD) is
  begin
    GET(F, V.CON);
    GET(F, SPACER);
    GET(F, V.TENSE_VOICE_MOOD);
    GET(F, SPACER);
    GET(F, V.PERSON);
    GET(F, SPACER);
    GET(F, V.NUMBER);
  end GET;

  procedure GET(V : out VERB_RECORD) is
  begin
    GET(V.CON);
    GET(SPACER);
    GET(V.TENSE_VOICE_MOOD);
    GET(SPACER);
    GET(V.PERSON);
    GET(SPACER);
    GET(V.NUMBER);
  end GET;

  procedure PUT(F : in FILE_TYPE; V : in VERB_RECORD) is
  begin
    PUT(F, V.CON);
    PUT(F, ' ');
    PUT(F, V.TENSE_VOICE_MOOD);
    PUT(F, ' ');
    PUT(F, V.PERSON);
    PUT(F, ' ');
    PUT(F, V.NUMBER);
  end PUT;

  procedure PUT(V : in VERB_RECORD) is
  begin
    PUT(V.CON);
    PUT(' ');
    PUT(V.TENSE_VOICE_MOOD);
    PUT(' ');
    PUT(V.PERSON);
    PUT(' ');
    PUT(V.NUMBER);
  end PUT;

  procedure GET(S : in STRING; V : out VERB_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), V.CON, L);
    L := L + 1;
    GET(S(L+1..S'LAST), V.TENSE_VOICE_MOOD, L);
    L := L + 1;
    GET(S(L+1..S'LAST), V.PERSON, L);
    L := L + 1;
    GET(S(L+1..S'LAST), V.NUMBER, LAST);
  end GET;

  procedure PUT(S : out STRING; V : in VERB_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), V.CON);
    L := M + 1;
    S(L) :=  ' ';
    M := L + TENSE_VOICE_MOOD_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), V.TENSE_VOICE_MOOD);
    L := M + 1;
    S(L) :=  ' ';
    M := L + PERSON_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), V.PERSON);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NUMBER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), V.NUMBER);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end VERB_RECORD_IO;


package body VPAR_RECORD_IO is
  use DECN_RECORD_IO;
  use CASE_TYPE_IO;
  use NUMBER_TYPE_IO;
  use GENDER_TYPE_IO;
  use TENSE_VOICE_MOOD_RECORD_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; VP : out VPAR_RECORD) is
  begin
    GET(F, VP.CON);
    GET(F, SPACER);
    GET(F, VP.CS);
    GET(F, SPACER);
    GET(F, VP.NUMBER);
    GET(F, SPACER);
    GET(F, VP.GENDER);
    GET(F, SPACER);
    GET(F, VP.TENSE_VOICE_MOOD);
  end GET;

  procedure GET(VP : out VPAR_RECORD) is
  begin
    GET(VP.CON);
    GET(SPACER);
    GET(VP.CS);
    GET(SPACER);
    GET(VP.NUMBER);
    GET(SPACER);
    GET(VP.GENDER);
    GET(SPACER);
    GET(VP.TENSE_VOICE_MOOD);
  end GET;

  procedure PUT(F : in FILE_TYPE; VP : in VPAR_RECORD) is
  begin
    PUT(F, VP.CON);
    PUT(F, ' ');
    PUT(F, VP.CS);
    PUT(F, ' ');
    PUT(F, VP.NUMBER);
    PUT(F, ' ');
    PUT(F, VP.GENDER);
    PUT(F, ' ');
    PUT(F, VP.TENSE_VOICE_MOOD);
  end PUT;

  procedure PUT(VP : in VPAR_RECORD) is
  begin
    PUT(VP.CON);
    PUT(' ');
    PUT(VP.CS);
    PUT(' ');
    PUT(VP.NUMBER);
    PUT(' ');
    PUT(VP.GENDER);
    PUT(' ');
    PUT(VP.TENSE_VOICE_MOOD);
  end PUT;

  procedure GET(S : in STRING; VP : out VPAR_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), VP.CON, L);
    L := L + 1;
    GET(S(L+1..S'LAST), VP.CS, L);
    L := L + 1;
    GET(S(L+1..S'LAST), VP.NUMBER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), VP.GENDER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), VP.TENSE_VOICE_MOOD, LAST);
  end GET;

  procedure PUT(S : out STRING; VP : in VPAR_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), VP.CON);
    L := M + 1;
    S(L) :=  ' ';
    M := L + CASE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), VP.CS);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NUMBER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), VP.NUMBER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + GENDER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), VP.GENDER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + TENSE_VOICE_MOOD_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), VP.TENSE_VOICE_MOOD);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end VPAR_RECORD_IO;


package body SUPINE_RECORD_IO is
  use DECN_RECORD_IO;
  use CASE_TYPE_IO;
  use NUMBER_TYPE_IO;
  use GENDER_TYPE_IO;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; VP : out SUPINE_RECORD) is
  begin
    GET(F, VP.CON);
    GET(F, SPACER);
    GET(F, VP.CS);
    GET(F, SPACER);
    GET(F, VP.NUMBER);
    GET(F, SPACER);
    GET(F, VP.GENDER);
  end GET;

  procedure GET(VP : out SUPINE_RECORD) is
  begin
    GET(VP.CON);
    GET(SPACER);
    GET(VP.CS);
    GET(SPACER);
    GET(VP.NUMBER);
    GET(SPACER);
    GET(VP.GENDER);
  end GET;

  procedure PUT(F : in FILE_TYPE; VP : in SUPINE_RECORD) is
  begin
    PUT(F, VP.CON);
    PUT(F, ' ');
    PUT(F, VP.CS);
    PUT(F, ' ');
    PUT(F, VP.NUMBER);
    PUT(F, ' ');
    PUT(F, VP.GENDER);
  end PUT;

  procedure PUT(VP : in SUPINE_RECORD) is
  begin
    PUT(VP.CON);
    PUT(' ');
    PUT(VP.CS);
    PUT(' ');
    PUT(VP.NUMBER);
    PUT(' ');
    PUT(VP.GENDER);
  end PUT;

  procedure GET(S : in STRING; VP : out SUPINE_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), VP.CON, L);
    L := L + 1;
    GET(S(L+1..S'LAST), VP.CS, L);
    L := L + 1;
    GET(S(L+1..S'LAST), VP.NUMBER, L);
    L := L + 1;
    GET(S(L+1..S'LAST), VP.GENDER, LAST);
  end GET;

  procedure PUT(S : out STRING; VP : in SUPINE_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + DECN_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), VP.CON);
    L := M + 1;
    S(L) :=  ' ';
    M := L + CASE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), VP.CS);
    L := M + 1;
    S(L) :=  ' ';
    M := L + NUMBER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), VP.NUMBER);
    L := M + 1;
    S(L) :=  ' ';
    M := L + GENDER_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), VP.GENDER);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end SUPINE_RECORD_IO;


package body PREPOSITION_RECORD_IO is
  use CASE_TYPE_IO;
  SPACER : CHARACTER := ' ';

  procedure GET(F : in FILE_TYPE; P : out PREPOSITION_RECORD) is
  begin
    GET(F, P.OBJ);
  end GET;

  procedure GET(P : out PREPOSITION_RECORD) is
  begin
    GET(P.OBJ);
  end GET;

  procedure PUT(F : in FILE_TYPE; P : in PREPOSITION_RECORD) is
  begin
    PUT(F, P.OBJ);
  end PUT;

  procedure PUT(P : in PREPOSITION_RECORD) is
  begin
    PUT(P.OBJ);
  end PUT;

  procedure GET(S : in STRING; P : out PREPOSITION_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    GET(S(L+1..S'LAST), P.OBJ, LAST);
  end GET;

  procedure PUT(S : out STRING; P : in PREPOSITION_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + CASE_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.OBJ);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end PREPOSITION_RECORD_IO;


package body CONJUNCTION_RECORD_IO is
  NULL_CONJUNCTION_RECORD : CONJUNCTION_RECORD;
  SPACER : CHARACTER := ' ';


  procedure GET(F : in FILE_TYPE; C : out CONJUNCTION_RECORD) is
  --  There is actually nothing to a CONJUNCTION_RECORD, no compoonents
  begin
    C := NULL_CONJUNCTION_RECORD;
  end GET;

  procedure GET(C : out CONJUNCTION_RECORD) is
  begin
    C := NULL_CONJUNCTION_RECORD;
  end GET;

  procedure PUT(F : in FILE_TYPE; C : in CONJUNCTION_RECORD) is
  begin
    null;
  end PUT;

  procedure PUT(C : in CONJUNCTION_RECORD) is
  begin
    null;
  end PUT;

  procedure GET(S : in STRING; C : out CONJUNCTION_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    C := NULL_CONJUNCTION_RECORD;
    LAST := L - 1;  --  LAST did not even get to S'FIRST, since nothing to read
  end GET;

  procedure PUT(S : out STRING; C : in CONJUNCTION_RECORD) is
  --  Since there is no component, just make the out string blank
  begin
    S(S'FIRST..S'LAST) := (others => ' ');
  end PUT;


end CONJUNCTION_RECORD_IO;


package body INTERJECTION_RECORD_IO is
  NULL_INTERJECTION_RECORD : INTERJECTION_RECORD;
  SPACER : CHARACTER := ' ';

 procedure GET(F : in FILE_TYPE; I : out INTERJECTION_RECORD) is
  begin
    I := NULL_INTERJECTION_RECORD;
  end GET;

  procedure GET(I : out INTERJECTION_RECORD) is
  begin
    I := NULL_INTERJECTION_RECORD;
  end GET;

  procedure PUT(F : in FILE_TYPE; I : in INTERJECTION_RECORD) is
  begin
    null;
  end PUT;

  procedure PUT(I : in INTERJECTION_RECORD) is
  begin
    null;
  end PUT;

  procedure GET(S : in STRING; I : out INTERJECTION_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    I := NULL_INTERJECTION_RECORD;
    LAST := L - 1;
  end GET;

  procedure PUT(S : out STRING; I : in INTERJECTION_RECORD) is
  begin
    S(S'FIRST..S'LAST) := (others => ' ');
  end PUT;


end INTERJECTION_RECORD_IO;



package body TACKON_RECORD_IO is
  NULL_TACKON_RECORD : TACKON_RECORD;
  SPACER : CHARACTER := ' ';

  procedure GET(F : in FILE_TYPE; I : out TACKON_RECORD) is
  begin
    I := NULL_TACKON_RECORD;
  end GET;

  procedure GET(I : out TACKON_RECORD) is
  begin
    I := NULL_TACKON_RECORD;
  end GET;

  procedure PUT(F : in FILE_TYPE; I : in TACKON_RECORD) is
  begin
    null;
  end PUT;

  procedure PUT(I : in TACKON_RECORD) is
  begin
    null;
  end PUT;

  procedure GET(S : in STRING; I : out TACKON_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    I := NULL_TACKON_RECORD;
    LAST := L - 1;
  end GET;

  procedure PUT(S : out STRING; I : in TACKON_RECORD) is
  begin
    S(S'FIRST..S'LAST) := (others => ' ');
  end PUT;


end TACKON_RECORD_IO;


  package body PREFIX_RECORD_IO is

    procedure GET(F : in FILE_TYPE; P : out PREFIX_RECORD) is
    begin
      P := NULL_PREFIX_RECORD;
    end GET;

    procedure GET(P : out PREFIX_RECORD) is
    begin
      P := NULL_PREFIX_RECORD;
    end GET;

    procedure PUT(F : in FILE_TYPE; P : in PREFIX_RECORD) is
    begin
      null;
    end PUT;

    procedure PUT(P : in PREFIX_RECORD) is
    begin
      null;
    end PUT;

    procedure GET(S : in STRING; P : out PREFIX_RECORD; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
    begin
      P := NULL_PREFIX_RECORD;
      LAST := L - 1;
    end GET;

    procedure PUT(S : out STRING; P : in PREFIX_RECORD) is
    begin
      S(S'FIRST..S'LAST) := (others => ' ');
    end PUT;

  end PREFIX_RECORD_IO;


  package body SUFFIX_RECORD_IO is


    procedure GET(F : in FILE_TYPE; P : out SUFFIX_RECORD) is
    begin
      P := NULL_SUFFIX_RECORD;
    end GET;

    procedure GET(P : out SUFFIX_RECORD) is
    begin
      P := NULL_SUFFIX_RECORD;
    end GET;

    procedure PUT(F : in FILE_TYPE; P : in SUFFIX_RECORD) is
    begin
      null;
    end PUT;

    procedure PUT(P : in SUFFIX_RECORD) is
    begin
      null;
    end PUT;

    procedure GET(S : in STRING; P : out SUFFIX_RECORD; LAST : out INTEGER) is
      L : INTEGER := S'FIRST - 1;
    begin
      P := NULL_SUFFIX_RECORD;
      LAST := L - 1;
    end GET;

    procedure PUT(S : out STRING; P : in SUFFIX_RECORD) is
    begin
      S(S'FIRST..S'LAST) := (others => ' ');
    end PUT;

  end SUFFIX_RECORD_IO;


package body QUALITY_RECORD_IO is
  use PART_OF_SPEECH_TYPE_IO;
  use NOUN_RECORD_IO;
  use PRONOUN_RECORD_IO;
  use PROPACK_RECORD_IO;
  use ADJECTIVE_RECORD_IO;
  use NUMERAL_RECORD_IO;
  use ADVERB_RECORD_IO;
  use VERB_RECORD_IO;
  use VPAR_RECORD_IO;
  use SUPINE_RECORD_IO;
  use PREPOSITION_RECORD_IO;
  use CONJUNCTION_RECORD_IO;
  use INTERJECTION_RECORD_IO;
  use TACKON_RECORD_IO;
  use PREFIX_RECORD_IO;
  use SUFFIX_RECORD_IO;
  SPACER : CHARACTER := ' ';


  NOUN  : NOUN_RECORD;
  PRONOUN : PRONOUN_RECORD;
  PROPACK : PROPACK_RECORD;
  ADJECTIVE : ADJECTIVE_RECORD;
  ADVERB : ADVERB_RECORD;
  VERB : VERB_RECORD;
  VPARTICIPLE : VPAR_RECORD;
  SUPIN : SUPINE_RECORD;
  PREPOSITION : PREPOSITION_RECORD;
  CONJUNCTION : CONJUNCTION_RECORD;
  INTERJECTION : INTERJECTION_RECORD;
  NUMERAL : NUMERAL_RECORD;
  TACKN : TACKON_RECORD;
  PREFX : PREFIX_RECORD;
  SUFFX : SUFFIX_RECORD;

  PR : QUALITY_RECORD;


  procedure GET(F : in FILE_TYPE; P : out QUALITY_RECORD) is
    PS : PART_OF_SPEECH_TYPE := X;
  begin
    GET(F, PS);
    GET(F, SPACER);
    case PS is
      when N =>
        GET(F, NOUN);
        P := (N, NOUN);
      when PRON =>
        GET(F, PRONOUN);
        P := (PRON, PRONOUN);
      when PACK =>
        GET(F, PROPACK);
        P := (PACK, PROPACK);
      when ADJ =>
        GET(F, ADJECTIVE);
        P := (ADJ, ADJECTIVE);
      when NUM =>
        GET(F, NUMERAL);
        P := (NUM, NUMERAL);
      when ADV =>
        GET(F, ADVERB);
        P := (ADV, ADVERB);
      when V =>
        GET(F, VERB);
        P := (V, VERB);
      when VPAR =>
        GET(F, VPARTICIPLE);
        P := (VPAR, VPARTICIPLE);
      when SUPINE =>
        GET(F, SUPIN);
        P := (SUPINE, SUPIN);
      when PREP =>
        GET(F, PREPOSITION);
        P := (PREP, PREPOSITION);
      when CONJ =>
        GET(F, CONJUNCTION);
        P := (CONJ, CONJUNCTION);
      when INTERJ =>
        GET(F, INTERJECTION);
        P := (INTERJ, INTERJECTION);
      when TACKON =>
        GET(F, TACKN);
        P := (TACKON, TACKN);
      when PREFIX =>
        GET(F, PREFX);
        P := (PREFIX, PREFX);
      when SUFFIX =>
        GET(F, SUFFX);
        P := (SUFFIX, SUFFX);
      when X =>
        P := (POFS => X);
    end case;
    return;
  end GET;

  procedure GET(P : out QUALITY_RECORD) is
    PS : PART_OF_SPEECH_TYPE := X;
  begin
    GET(PS);
    GET(SPACER);
    case PS is
      when N =>
        GET(NOUN);
        P := (N, NOUN);
      when PRON =>
        GET(PRONOUN);
        P := (PRON, PRONOUN);
      when PACK =>
        GET(PROPACK);
        P := (PACK, PROPACK);
      when ADJ =>
        GET(ADJECTIVE);
        P := (ADJ, ADJECTIVE);
      when NUM =>
        GET(NUMERAL);
        P := (NUM, NUMERAL);
      when ADV =>
        GET(ADVERB);
        P := (ADV, ADVERB);
      when V =>
        GET(VERB);
        P := (V, VERB);
      when VPAR =>
        GET(VPARTICIPLE);
        P := (VPAR, VPARTICIPLE);
      when SUPINE =>
        GET(SUPIN);
        P := (SUPINE, SUPIN);
      when PREP =>
        GET(PREPOSITION);
        P := (PREP, PREPOSITION);
      when CONJ =>
        GET(CONJUNCTION);
        P := (CONJ, CONJUNCTION);
      when INTERJ =>
        GET(INTERJECTION);
        P := (INTERJ, INTERJECTION);
      when TACKON =>
        GET(TACKN);
        P := (TACKON, TACKN);
      when PREFIX =>
        GET(PREFX);
        P := (PREFIX, PREFX);
      when SUFFIX =>
        GET(SUFFX);
        P := (SUFFIX, SUFFX);
      when X =>
        P := (POFS => X);
    end case;
    return;
  end GET;

  procedure PUT(F : in FILE_TYPE; P : in QUALITY_RECORD) is
    C : POSITIVE := POSITIVE(COL(F));
  begin
    PUT(F, P.POFS);
    PUT(F, ' ');
    case P.POFS is
      when N =>
        PUT(F, P.N);
      when PRON =>
        PUT(F, P.PRON);
      when PACK =>
        PUT(F, P.PACK);
      when ADJ =>
        PUT(F, P.ADJ);
      when NUM =>
        PUT(F, P.NUM);
      when ADV =>
        PUT(F, P.ADV);
      when V =>
        PUT(F, P.V);
      when VPAR =>
        PUT(F, P.VPAR);
      when SUPINE =>
        PUT(F, P.SUPINE);
      when PREP =>
        PUT(F, P.PREP);
      when CONJ =>
        PUT(F, P.CONJ);
      when INTERJ =>
        PUT(F, P.INTERJ);
      when TACKON =>
        PUT(F, P.TACKON);
      when PREFIX =>
        PUT(F, P.PREFIX);
      when SUFFIX =>
        PUT(F, P.SUFFIX);
      when others =>
        null;
    end case;
    PUT(F, STRING'((INTEGER(COL(F))..QUALITY_RECORD_IO.DEFAULT_WIDTH+C-1 => ' ')));
    return;
  end PUT;


  procedure PUT(P : in QUALITY_RECORD) is
    C : POSITIVE := POSITIVE(COL);
  begin
    PUT(P.POFS);
    PUT(' ');
    case P.POFS is
      when N =>
        PUT(P.N);
      when PRON =>
        PUT(P.PRON);
      when PACK =>
        PUT(P.PACK);
      when ADJ =>
        PUT(P.ADJ);
      when NUM =>
        PUT(P.NUM);
      when ADV =>
        PUT(P.ADV);
      when V =>
        PUT(P.V);
      when VPAR =>
        PUT(P.VPAR);
      when SUPINE =>
        PUT(P.SUPINE);
      when PREP =>
        PUT(P.PREP);
      when CONJ =>
        PUT(P.CONJ);
      when INTERJ =>
        PUT(P.INTERJ);
      when TACKON =>
        PUT(P.TACKON);
      when PREFIX =>
        PUT(P.PREFIX);
      when SUFFIX =>
        PUT(P.SUFFIX);
      when others =>
        null;
    end case;
    PUT(STRING'((INTEGER(COL)..QUALITY_RECORD_IO.DEFAULT_WIDTH+C-1 => ' ')));
    return;
  end PUT;

  procedure GET(S : in STRING; P : out QUALITY_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
    PS : PART_OF_SPEECH_TYPE := X;
  begin
    GET(S, PS, L);
    LAST := L;         --  In case it is not set later
    L := L + 1;
    case PS is
      when N =>
        GET(S(L+1..S'LAST), NOUN, LAST);
        P := (N, NOUN);
      when PRON =>
        GET(S(L+1..S'LAST), PRONOUN, LAST);
        P := (PRON, PRONOUN);
      when PACK =>
        GET(S(L+1..S'LAST), PROPACK, LAST);
        P := (PACK, PROPACK);
      when ADJ =>
        GET(S(L+1..S'LAST), ADJECTIVE, LAST);
        P := (ADJ, ADJECTIVE);
      when NUM =>
        GET(S(L+1..S'LAST), NUMERAL, LAST);
        P := (NUM, NUMERAL);
      when ADV =>
        GET(S(L+1..S'LAST), ADVERB, LAST);
        P := (ADV, ADVERB);
      when V =>
        GET(S(L+1..S'LAST), VERB, LAST);
        P := (V, VERB);
      when VPAR =>
        GET(S(L+1..S'LAST), VPARTICIPLE, LAST);
        P := (VPAR, VPARTICIPLE);
      when SUPINE =>
        GET(S(L+1..S'LAST), SUPIN, LAST);
        P := (SUPINE, SUPIN);
      when PREP =>
        GET(S(L+1..S'LAST), PREPOSITION, LAST);
        P := (PREP, PREPOSITION);
      when CONJ =>
        GET(S(L+1..S'LAST), CONJUNCTION, LAST);
        P := (CONJ, CONJUNCTION);
      when INTERJ =>
        GET(S(L+1..S'LAST), INTERJECTION, LAST);
        P := (INTERJ, INTERJECTION);
      when TACKON =>
        GET(S(L+1..S'LAST), TACKN, LAST);
        P := (TACKON, TACKN);
      when PREFIX =>
        GET(S(L+1..S'LAST), PREFX, LAST);
        P := (PREFIX, PREFX);
      when SUFFIX =>
        GET(S(L+1..S'LAST), SUFFX, LAST);
        P := (SUFFIX, SUFFX);
      when X =>
        P := (POFS => X);
    end case;
    return;
  end GET;


  procedure PUT(S : out STRING; P : in QUALITY_RECORD) is
  --  Note that this does not Put with a uniform width
  --  which would require a constant QUALITY_RECORD_IO.DEFAULT_WIDTH
  --  Rather we Put to minimal size with NOUN_RECORD_IO.DEFAULT_WIDTH,
  --  PRONOUN_RECORD_IO,DEFAULT_WIDTH, ...
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.POFS);
    L := M + 1;
    S(L) :=  ' ';
    case P.POFS is
      when N =>
        M := L + NOUN_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.N);
      when PRON =>
        M := L + PRONOUN_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.PRON);
      when PACK =>
        M := L + PROPACK_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.PACK);
      when ADJ =>
        M := L + ADJECTIVE_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.ADJ);
      when NUM =>
        M := L + NUMERAL_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.NUM);
      when ADV =>
        M := L + ADVERB_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.ADV);
      when V =>
        M := L + VERB_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.V);
      when VPAR =>
        M := L + VPAR_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.VPAR);
      when SUPINE =>
        M := L + SUPINE_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.SUPINE);
      when PREP =>
        M := L + PREPOSITION_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.PREP);
      when CONJ =>
        M := L + CONJUNCTION_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.CONJ);
      when INTERJ =>
        M := L + INTERJECTION_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.INTERJ);
      when TACKON =>
        M := L + TACKON_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.TACKON);
      when PREFIX =>
        M := L + PREFIX_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.PREFIX);
      when SUFFIX =>
        M := L + SUFFIX_RECORD_IO.DEFAULT_WIDTH;
        PUT(S(L+1..M), P.SUFFIX);
      when others =>
        null;
    end case;
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end QUALITY_RECORD_IO;


package body ENDING_RECORD_IO is
  use INTEGER_IO;
  use TEXT_IO;
  SPACER : CHARACTER := ' ';

  SF, BLANKS : ENDING := (others => ' ');
  N : ENDING_SIZE_TYPE := 0;

  procedure GET(F : in FILE_TYPE; X : out ENDING_RECORD) is
  begin
    SF := BLANKS;
    GET(F, N);
    if N = 0  then
      X := NULL_ENDING_RECORD;
    else
      GET(F, SPACER);             --  Note this means exactly one blank
      GET(F, SF(1..N));
      X := (N, SF);
    end if;
  end GET;

  procedure GET(X : out ENDING_RECORD) is
  begin
    SF := BLANKS;
    GET(N);
    if N = 0  then
      X := NULL_ENDING_RECORD;
    else
      GET(SPACER);
      GET(SF(1..N));
      X := (N, SF);
    end if;
  end GET;

  procedure PUT(F : in FILE_TYPE; X : in ENDING_RECORD) is
  begin
   PUT(F, X.SIZE, 1);
   PUT(F, ' ');
   PUT(F, X.SUF(1..X.SIZE) & BLANKS(X.SIZE+1..MAX_ENDING_SIZE));
  end PUT;

  procedure PUT(X : in ENDING_RECORD) is
  begin
    PUT(X.SIZE, 1);
    PUT(' ');
    PUT(X.SUF(1..X.SIZE) & BLANKS(X.SIZE+1..MAX_ENDING_SIZE));
  end PUT;

  procedure GET(S : in STRING; X : out ENDING_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    SF := BLANKS;
    GET(S(L+1..S'LAST), N, L);
    if N = 0  then
      X := NULL_ENDING_RECORD;
      LAST := L;
    else
      L := L + 1;
--if S(L+N-1) = ' '  or else
--   S(L+N+1) /= ' '  then
--if 
--   S(L+N+1) /= ' '  then
-- TEXT_IO.PUT_LINE("ERROR in INFLECTION =>" & S);   
--else     
      SF := S(L+1..L+N) & BLANKS(N+1..MAX_ENDING_SIZE);
      LAST := L + N;
      X := (N, SF(1..N) & BLANKS(N+1..MAX_ENDING_SIZE));
--end if;
    end if;
    exception
      when others =>
        TEXT_IO.PUT_LINE("ENDING ERRROR " & S);
        end GET;

  procedure PUT(S : out STRING; X : in ENDING_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + 2;
    PUT(S(L+1..M), X.SIZE);
    M := M  + 1;
    S(M) := ' ';
    if X.SIZE > 0  then
      L := M;
      M := L + X.SIZE;
      S(L+1..M) := X.SUF(1..X.SIZE);
    end if;
    --  Being very careful here, first to fill out to the MAX_ENDING_SIZE
    L := M;
    M := L + MAX_ENDING_SIZE - X.SIZE;
    S(L+1..M) := (others => ' ');
    --  Then to fill out the rest of the out string, if any
    S(M+1..S'LAST) := (others => ' ');
  end PUT;


end ENDING_RECORD_IO;


package body INFLECTION_RECORD_IO is
  use QUALITY_RECORD_IO;
  use STEM_KEY_TYPE_IO;
  use ENDING_RECORD_IO;
  use AGE_TYPE_IO;
  use FREQUENCY_TYPE_IO;
  SPACER : CHARACTER := ' ';

  PE : INFLECTION_RECORD;

  procedure GET(F : in FILE_TYPE; P : out INFLECTION_RECORD) is
  begin
    GET(F, P.QUAL);
    GET(F, SPACER);
    GET(F, P.KEY);
    GET(F, SPACER);
    GET(F, P.ENDING);
    GET(F, SPACER);
    GET(F, P.AGE);
    GET(F, SPACER);
    GET(F, P.FREQ);
   end GET;

  procedure GET(P : out INFLECTION_RECORD) is
  begin
    GET(P.QUAL);
    GET(SPACER);
    GET(P.KEY);
    GET(SPACER);
    GET(P.ENDING);
    GET(SPACER);
    GET(P.AGE);
    GET(SPACER);
    GET(P.FREQ);
   end GET;

  procedure PUT(F : in FILE_TYPE; P : in INFLECTION_RECORD) is
  begin
    PUT(F, P.QUAL);
    PUT(F, ' ');
    PUT(F, P.KEY, 1);
    PUT(F, ' ');
    PUT(F, P.ENDING);
    PUT(F, ' ');
    PUT(F, P.AGE);
    PUT(F, ' ');
    PUT(F, P.FREQ);
   end PUT;

  procedure PUT(P : in INFLECTION_RECORD) is
  begin
    PUT(P.QUAL);
    PUT(' ');
    PUT(P.KEY, 1);
    PUT(' ');
    PUT(P.ENDING);
    PUT(' ');
    PUT(P.AGE);
    PUT(' ');
    PUT(P.FREQ);
  end PUT;

  procedure GET(S : in STRING; P : out INFLECTION_RECORD; LAST : out INTEGER) is
    L : INTEGER := S'FIRST - 1;
  begin
    LAST := 0;
    P := PE;
    GET(S(L+1..S'LAST), P.QUAL, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.KEY, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.ENDING, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.AGE, L);
    L := L + 1;
    GET(S(L+1..S'LAST), P.FREQ, LAST);
   end GET;

  procedure PUT(S : out STRING; P : in INFLECTION_RECORD) is
    L : INTEGER := S'FIRST - 1;
    M : INTEGER := 0;
  begin
    M := L + QUALITY_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.QUAL);
    L := M + 1;
    S(L) :=  ' ';
    M := L + 1;
    PUT(S(L+1..M), P.KEY);
    L := M + 1;
    S(L) :=  ' ';
    M := L + ENDING_RECORD_IO.DEFAULT_WIDTH;
    PUT(S(L+1..M), P.ENDING);
    L := M + 1;
    S(L) :=  ' ';
    M := L + 1;
    PUT(S(L+1..M), P.AGE);
    L := M + 1;
    S(L) :=  ' ';
    M := L + 1;
    PUT(S(L+1..M), P.FREQ);
    S(M+1..S'LAST) := (others => ' ');
  end PUT;

end INFLECTION_RECORD_IO;


procedure ESTABLISH_INFLECTIONS_SECTION  is
--  Loads the inflection array from the file prepared in FILE_INFLECTIONS_SECTION
--  If N = 0 (an artifical flag for the section for blank inflections = 5) 
--  computes the LELL..LELF indices for use in WORD
  use TEXT_IO;
  use INFLECTION_RECORD_IO;
  use LEL_SECTION_IO;

  procedure LOAD_LEL_INDEXES is
  --  Load arrays from file
    I  : INTEGER := 0;
    --IR : INFLECTION_RECORD;
    N, XN : INTEGER := 0;
    CH, XCH : CHARACTER := ' ';
    INFLECTIONS_SECTIONS_FILE : LEL_SECTION_IO.FILE_TYPE;
  begin
    OPEN(INFLECTIONS_SECTIONS_FILE, IN_FILE, INFLECTIONS_SECTIONS_NAME);
    NUMBER_OF_INFLECTIONS := 0;

    LEL_SECTION_IO.READ(INFLECTIONS_SECTIONS_FILE,
                        LEL,
                        LEL_SECTION_IO.POSITIVE_COUNT(5));


    I := 1;
    BELF(0, ' ') := I;
    BELL(0, ' ') := 0;
    loop
      exit when LEL(I) = NULL_INFLECTION_RECORD;
      BEL(I) := LEL(I);

      BELL(0, ' ') := I;
      I := I + 1;
    end loop;

    NUMBER_OF_INFLECTIONS := NUMBER_OF_INFLECTIONS + I - 1;

    LEL_SECTION_IO.READ(INFLECTIONS_SECTIONS_FILE,
                        LEL,
                        LEL_SECTION_IO.POSITIVE_COUNT(1));

    I := 1;
    N := LEL(I).ENDING.SIZE;

    CH := LEL(I).ENDING.SUF(N);

    XN := N;
    XCH := CH;
    LELF(N, CH) := I;

    C1_LOOP:
    loop
    N1_LOOP:
    loop
      exit C1_LOOP when LEL(I) = NULL_INFLECTION_RECORD;

      N := LEL(I).ENDING.SIZE;

      CH := LEL(I).ENDING.SUF(N);

            if CH /= XCH  then
        LELL(XN, XCH) := I - 1;
        LELF(N, CH) := I;
        LELL(N, CH) := 0;
        XCH := CH;
        XN := N;
      elsif N /= XN  then
        LELL(XN, CH) := I - 1;
        LELF(N, CH) := I;
        LELL(N, CH) := 0;
        XN := N;
        exit N1_LOOP;
      end if;


      I := I + 1;


    end loop N1_LOOP;

    end loop C1_LOOP;

    LELL(XN, XCH) := I - 1;

    NUMBER_OF_INFLECTIONS := NUMBER_OF_INFLECTIONS + I - 1;

    LEL_SECTION_IO.READ(INFLECTIONS_SECTIONS_FILE,
                        LEL,
                        LEL_SECTION_IO.POSITIVE_COUNT(2));


    I := 1;

    N := LEL(I).ENDING.SIZE;

    CH := LEL(I).ENDING.SUF(N);

    XN := N;
    XCH := CH;
    LELF(N, CH) := I;

    C2_LOOP:
    loop
    N2_LOOP:
    loop
      exit C2_LOOP when LEL(I) = NULL_INFLECTION_RECORD;

      N := LEL(I).ENDING.SIZE;

      CH := LEL(I).ENDING.SUF(N);
      exit when CH > 'r';



      if CH /= XCH  then
        LELL(XN, XCH) := I - 1;
        LELF(N, CH) := I;
        LELL(N, CH) := 0;
        XCH := CH;
        XN := N;
      elsif N /= XN  then
        LELL(XN, CH) := I - 1;
        LELF(N, CH) := I;
        LELL(N, CH) := 0;
        XN := N;
        exit N2_LOOP;
      end if;

      I := I + 1;

    end loop N2_LOOP;

    end loop C2_LOOP;

    LELL(XN, XCH) := I - 1;

    NUMBER_OF_INFLECTIONS := NUMBER_OF_INFLECTIONS + I - 1;

    LEL_SECTION_IO.READ(INFLECTIONS_SECTIONS_FILE,
                        LEL,
                        LEL_SECTION_IO.POSITIVE_COUNT(3));

    I := 1;

    N := LEL(I).ENDING.SIZE;

    CH := LEL(I).ENDING.SUF(N);

    XN := N;
    XCH := CH;
    LELF(N, CH) := I;

    C3_LOOP:
    loop
    N3_LOOP:
    loop
      exit C3_LOOP when LEL(I) = NULL_INFLECTION_RECORD;

      N := LEL(I).ENDING.SIZE;

      CH := LEL(I).ENDING.SUF(N);
      exit when CH > 's';


      if CH /= XCH  then
        LELL(XN, XCH) := I - 1;
        LELF(N, CH) := I;
        LELL(N, CH) := 0;
        XCH := CH;
        XN := N;
      elsif N /= XN  then
        LELL(XN, CH) := I - 1;
        LELF(N, CH) := I;
        LELL(N, CH) := 0;
        XN := N;
        exit N3_LOOP;
      end if;

      I := I + 1;


    end loop N3_LOOP;

    end loop C3_LOOP;

    LELL(XN, XCH) := I - 1;

    NUMBER_OF_INFLECTIONS := NUMBER_OF_INFLECTIONS + I - 1;

    LEL_SECTION_IO.READ(INFLECTIONS_SECTIONS_FILE,
                        LEL,
                        LEL_SECTION_IO.POSITIVE_COUNT(4));


    I := 1;

    N := LEL(I).ENDING.SIZE;

    CH := LEL(I).ENDING.SUF(N);


    XN := N;
    XCH := CH;
    LELF(N, CH) := I;

    C4_LOOP:
    loop
    N4_LOOP:
    loop

       exit C4_LOOP when  LEL(I).QUAL.POFS = PRON  and then
                         (LEL(I).QUAL.PRON.DECL.WHICH = 1  or
                          LEL(I).QUAL.PRON.DECL.WHICH = 2);


      N := LEL(I).ENDING.SIZE;

      CH := LEL(I).ENDING.SUF(N);

            if CH /= XCH  then
        LELL(XN, XCH) := I - 1;
        LELF(N, CH) := I;
        LELL(N, CH) := 0;
        XCH := CH;
        XN := N;
      elsif N /= XN  then
        LELL(XN, CH) := I - 1;
        LELF(N, CH) := I;
        LELL(N, CH) := 0;
        XN := N;
        exit N4_LOOP;
      end if;

      I := I + 1;

    end loop N4_LOOP;

    end loop C4_LOOP;

    LELL(XN, XCH) := I - 1;

begin

    N := LEL(I).ENDING.SIZE;

    CH := LEL(I).ENDING.SUF(N);


    XN := N;
    XCH := CH;
    PELF(N,  CH) := I;
    PELL(N,  CH) := 0;


    C_P_LOOP:
    loop
    N_P_LOOP:
    loop
      exit C_P_LOOP when LEL(I) = NULL_INFLECTION_RECORD;

      N := LEL(I).ENDING.SIZE;

      CH := LEL(I).ENDING.SUF(N);


      if CH /= XCH  then
        PELL(XN, XCH) := I - 1;
        PELF(N, CH) := I;
        PELL(N, CH) := 0;
        XCH := CH;
        XN := N;
      elsif N /= XN  then
        PELL(XN, CH) := I - 1;
        PELF(N, CH) := I;
        PELL(N, CH) := 0;
        XN  := N;
        exit N_P_LOOP;
      end if;

      I := I + 1;

    end loop N_P_LOOP;

    end loop C_P_LOOP;


exception
  when CONSTRAINT_ERROR => null;
end;

    PELL(XN, XCH) := I - 1;
    NUMBER_OF_INFLECTIONS := NUMBER_OF_INFLECTIONS + I - 1;
    CLOSE(INFLECTIONS_SECTIONS_FILE);

  end LOAD_LEL_INDEXES;

begin

  PREFACE.PUT("INFLECTION_ARRAY being loaded");
  PREFACE.SET_COL(33);
  PREFACE.PUT("--  ");
  LOAD_LEL_INDEXES;                    --  Makes indexes from array
  PREFACE.PUT(NUMBER_OF_INFLECTIONS, 6);
  PREFACE.PUT(" entries");
  PREFACE.SET_COL(55); PREFACE.PUT_LINE("--  Loaded correctly");

exception
  when Text_IO.Name_Error  =>
    NEW_LINE;
    PUT_LINE("There is no " & INFLECTIONS_SECTIONS_NAME & " file.");
    PUT_LINE("The program cannot work without one.");
    PUT_LINE("Make sure you are in the subdirectory containing the files");
    PUT_LINE("for inflections, dictionary, addons and uniques.");
    raise GIVE_UP;

end ESTABLISH_INFLECTIONS_SECTION;



begin  --  initialization of body of INFLECTIONS_PACKAGE
--TEXT_IO.PUT_LINE("Initializing INFLECTIONS_PACKAGE");

  PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE'WIDTH;
  GENDER_TYPE_IO.DEFAULT_WIDTH := GENDER_TYPE'WIDTH;
  CASE_TYPE_IO.DEFAULT_WIDTH := CASE_TYPE'WIDTH;
  NUMBER_TYPE_IO.DEFAULT_WIDTH := NUMBER_TYPE'WIDTH;
  PERSON_TYPE_IO.DEFAULT_WIDTH := 1;
  COMPARISON_TYPE_IO.DEFAULT_WIDTH := COMPARISON_TYPE'WIDTH;
  TENSE_TYPE_IO.DEFAULT_WIDTH := TENSE_TYPE'WIDTH;
  VOICE_TYPE_IO.DEFAULT_WIDTH := VOICE_TYPE'WIDTH;
  MOOD_TYPE_IO.DEFAULT_WIDTH := MOOD_TYPE'WIDTH;
  NOUN_KIND_TYPE_IO.DEFAULT_WIDTH := NOUN_KIND_TYPE'WIDTH;
  PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH := PRONOUN_KIND_TYPE'WIDTH;
  VERB_KIND_TYPE_IO.DEFAULT_WIDTH := VERB_KIND_TYPE'WIDTH;
  NUMERAL_SORT_TYPE_IO.DEFAULT_WIDTH := NUMERAL_SORT_TYPE'WIDTH;
  AGE_TYPE_IO.DEFAULT_WIDTH := AGE_TYPE'WIDTH;
  FREQUENCY_TYPE_IO.DEFAULT_WIDTH := FREQUENCY_TYPE'WIDTH;

  DECN_RECORD_IO.DEFAULT_WIDTH :=
                                  1 + 1 +   --WHICH_TYPE_IO_DEFAULT_WIDTH + 1 +
                                  1;        --VARIANT_TYPE_IO_DEFAULT_WIDTH;
  TENSE_VOICE_MOOD_RECORD_IO.DEFAULT_WIDTH :=
                   TENSE_TYPE_IO.DEFAULT_WIDTH + 1 +
                   VOICE_TYPE_IO.DEFAULT_WIDTH + 1 +
                   MOOD_TYPE_IO.DEFAULT_WIDTH;
  NOUN_RECORD_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   CASE_TYPE_IO.DEFAULT_WIDTH + 1 +
                   NUMBER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   GENDER_TYPE_IO.DEFAULT_WIDTH;
  PRONOUN_RECORD_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   CASE_TYPE_IO.DEFAULT_WIDTH + 1 +
                   NUMBER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   GENDER_TYPE_IO.DEFAULT_WIDTH;
  PROPACK_RECORD_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   CASE_TYPE_IO.DEFAULT_WIDTH + 1 +
                   NUMBER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   GENDER_TYPE_IO.DEFAULT_WIDTH;
  ADJECTIVE_RECORD_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   CASE_TYPE_IO.DEFAULT_WIDTH + 1 +
                   NUMBER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   GENDER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   COMPARISON_TYPE_IO.DEFAULT_WIDTH;
  ADVERB_RECORD_IO.DEFAULT_WIDTH :=
                   COMPARISON_TYPE_IO.DEFAULT_WIDTH;
  VERB_RECORD_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   TENSE_VOICE_MOOD_RECORD_IO.DEFAULT_WIDTH + 1 +
                   PERSON_TYPE_IO.DEFAULT_WIDTH + 1 +
                   NUMBER_TYPE_IO.DEFAULT_WIDTH;
  VPAR_RECORD_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   CASE_TYPE_IO.DEFAULT_WIDTH + 1 +
                   NUMBER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   GENDER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   TENSE_VOICE_MOOD_RECORD_IO.DEFAULT_WIDTH;
  SUPINE_RECORD_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   CASE_TYPE_IO.DEFAULT_WIDTH + 1 +
                   NUMBER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   GENDER_TYPE_IO.DEFAULT_WIDTH;
  PREPOSITION_RECORD_IO.DEFAULT_WIDTH := CASE_TYPE_IO.DEFAULT_WIDTH;
  CONJUNCTION_RECORD_IO.DEFAULT_WIDTH := 0;
  INTERJECTION_RECORD_IO.DEFAULT_WIDTH := 0;
  NUMERAL_RECORD_IO.DEFAULT_WIDTH :=
                   DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                   CASE_TYPE_IO.DEFAULT_WIDTH + 1 +
                   NUMBER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   GENDER_TYPE_IO.DEFAULT_WIDTH + 1 +
                   NUMERAL_SORT_TYPE_IO.DEFAULT_WIDTH;
  TACKON_RECORD_IO.DEFAULT_WIDTH := 0;
  PREFIX_RECORD_IO.DEFAULT_WIDTH := 0;
  SUFFIX_RECORD_IO.DEFAULT_WIDTH := 0;
  QUALITY_RECORD_IO.DEFAULT_WIDTH := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 +
                                  VPAR_RECORD_IO.DEFAULT_WIDTH; --  Largest
  ENDING_RECORD_IO.DEFAULT_WIDTH := 3 + 1 +
                                    MAX_ENDING_SIZE;
  INFLECTION_RECORD_IO.DEFAULT_WIDTH := QUALITY_RECORD_IO.DEFAULT_WIDTH + 1 +
                                        1  + 1 +
                                        ENDING_RECORD_IO.DEFAULT_WIDTH + 1 +
                                        AGE_TYPE_IO.DEFAULT_WIDTH + 1 +
                                        FREQUENCY_TYPE_IO.DEFAULT_WIDTH;


end INFLECTIONS_PACKAGE;
