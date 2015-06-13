with dictionary_package; use dictionary_package;
package tricks_package is
   
   procedure syncope(w : string;
					 pa : in out parse_array; pa_last : in out integer);           

   procedure try_tricks(w : string; 
						pa : in out parse_array; pa_last : in out integer;   
												 line_number : integer; word_number : integer);

   procedure try_slury(w : string;
					   pa : in out parse_array; pa_last : in out integer;
												line_number : integer; word_number : integer);   

   procedure roman_numerals(input_word : string;
							pa : in out parse_array; pa_last : in out integer);
   

end tricks_package;
