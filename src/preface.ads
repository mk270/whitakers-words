with text_io;
package preface is
   procedure put(s: string);
   procedure set_col(pc : text_io.positive_count);
   procedure put_line(s : string);
   procedure new_line(spacing  : text_io.positive_count := 1);
   procedure put(n : integer; width : text_io.field := integer'width);
end preface;
