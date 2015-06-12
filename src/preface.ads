with TEXT_IO;
package PREFACE is
  procedure PUT(S: STRING);
  procedure SET_COL(PC : TEXT_IO.POSITIVE_COUNT);
  procedure PUT_LINE(S : STRING);
  procedure NEW_LINE(SPACING  : TEXT_IO.POSITIVE_COUNT := 1);
  procedure PUT(N : INTEGER; WIDTH : TEXT_IO.FIELD := INTEGER'WIDTH);
end PREFACE;

