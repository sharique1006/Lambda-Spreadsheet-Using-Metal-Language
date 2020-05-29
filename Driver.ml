let m = int_of_string Sys.argv.(2);; 
let n = int_of_string Sys.argv.(3);;

let rec store_sheet i j = function 
[] -> ()
| e::l -> if e = "" then begin Functions.spreadsheet.(i).(j) <- Empty; store_sheet i (j+1) l; end  
		  else 
		  begin 
			Functions.spreadsheet.(i).(j) <- Num(float_of_string e); 
			store_sheet i (j+1) l;
		  end;;


let _ =
  try
    let in_stream = open_in Sys.argv.(1) in
        for i=0 to (m-1) do
          let line = input_line in_stream in
          let split = Str.split (Str.regexp ",") in
          let values = split line in
            if line.[0] =',' then begin Functions.spreadsheet.(i).(0) <- Empty; store_sheet i 1 values; end else
            store_sheet i 0 values;
        done;
        close_in in_stream; 
  with e ->
    Printf.printf "File not found!";
    raise e


let main () =
try
let cin =
     if Array.length Sys.argv > 4
      then open_in Sys.argv.(4)
    else stdin
      in
     let lexbuf = Lexing.from_channel cin in
while true do
	Parser.main Lexer.token lexbuf;
	for i = 0 to (Array.length Functions.spreadsheet - 1) do 
		for j = 0 to (Array.length Functions.spreadsheet.(0) - 1) do 
			match Functions.spreadsheet.(i).(j) with
			| Empty -> Printf.printf "Empty "
			| Num(v) -> Printf.printf "%f " v 
		done;
		Printf.printf "\n"
	done
done;
with Lexer.END_OF_FILE -> exit 0
let _ = Printexc.print main ()
