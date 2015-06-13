with text_io; use text_io;
with strings_package; use strings_package;
procedure page2htm is
   line, line_out, blank_line : string(1..300) := (others => ' ');
   last, colons : integer := 0;
   
   input, output : file_type;
   
begin 
   put_line("DICTPAGE.RAW (sorted) -> DICTPAGE.HTM");
   put_line("For use in preparing a DICTPAGE.HTM after running DICTPAGE and sorting.");
   
   open(input, in_file, "DICTPAGE.RAW");
   create(output, out_file, "DICTPAGE.HTM");
   
   while not end_of_file(input)  loop
	  get_line(input, line, last);
	  if line(1) /= '#'  then
		 put_line("BAD LINE   >" & line(1..last));
	  end if;
	  for i in 1..last  loop
		 if line(i) = '['  then
			put(output, "<B>" & line(2..i-1) & "</B>  ");
			put_line(output, trim(line(i..i+6) & "<BR>"));
		 end if;
		 if line(i..i+1) = "::"  then
			put_line(output, trim(line(i+2..last)) & "<BR>");
			exit;
		 end if;
	  end loop;  --  On LINE
	  
   end loop;  --  On file
   
end page2htm;


