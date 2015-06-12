with CONFIG;
package body PREFACE is

  procedure PUT(S : STRING) is
  begin
    if not CONFIG.SUPPRESS_PREFACE  then
      TEXT_IO.PUT(TEXT_IO.CURRENT_OUTPUT, S);
    end if;
  end PUT;

  procedure SET_COL(PC : TEXT_IO.POSITIVE_COUNT) is
  begin
    if not CONFIG.SUPPRESS_PREFACE  then
      TEXT_IO.SET_COL(TEXT_IO.CURRENT_OUTPUT, PC);
    end if;
  end SET_COL; 

  procedure PUT_LINE(S : STRING) is
  begin
    if not CONFIG.SUPPRESS_PREFACE  then
      TEXT_IO.PUT_LINE(TEXT_IO.CURRENT_OUTPUT, S);
    end if;
  end PUT_LINE;

  procedure NEW_LINE(SPACING  : TEXT_IO.POSITIVE_COUNT := 1) is
  begin
    if not CONFIG.SUPPRESS_PREFACE  then
      TEXT_IO.NEW_LINE(TEXT_IO.CURRENT_OUTPUT, SPACING);
    end if;
  end NEW_LINE;

  procedure PUT(N : INTEGER; WIDTH : TEXT_IO.FIELD := INTEGER'WIDTH) is
    package INTEGER_IO is new TEXT_IO.INTEGER_IO(INTEGER);
  begin
    if not CONFIG.SUPPRESS_PREFACE  then
      INTEGER_IO.PUT(TEXT_IO.CURRENT_OUTPUT, N, WIDTH);
    end if;
  end PUT;


end PREFACE;
