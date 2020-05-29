(*Defined a type which corresponds to each cell of the sheet. It can be Empty or have a float value*)
type cell = Empty | Num of float
(*If the indices given in the arguments are not within the range of the sheet*)
exception IndexOutofBounds
(*In Binary operations whenever an empty cell is encountered raise this exception because add, subt, mult, div can't be performed on one operator*)
exception Empty_Cells_Found

(*Defined sheet as an array of array of cells*)
type sheet =  cell array array
(*Defined farr as an array of floats*)
type farr = float array
(*index in sheet is float*float*)
type index = int*int
(*range in sheet as an array of float*float*float*float *)
type range = int*int*int*int

(*let spreadsheet = [|[|Empty;Empty;Empty;Empty;Empty;Empty|];[|Empty;Empty;Empty;Empty;Empty;Empty|];[|Empty;Empty;Empty;Empty;Empty;Empty|];[|Empty;Empty;Empty;Empty;Empty;Empty|];[|Empty;Empty;Empty;Empty;Empty;Empty|];[|Empty;Empty;Empty;Empty;Empty;Empty|]|];;*)

(*row_size of initialized spreadsheet*)
let m = int_of_string Sys.argv.(2)
(*col_size of initialized spreadsheet*)
let n = int_of_string Sys.argv.(3)

let spreadsheet = Array.make_matrix m n Empty

(*CheckIndexInBounds*)
let check_bounds r3 c3 r1 c1 r2 c2 : bool = if r3>m||c3>n||r1>m||c1>n||r2>m||c2>n then false else true;;

(*full_count*)

(*count_vec counts all the non-empty cells in an array with index between c1 & c2*)
let rec count_vec arr count index c1 c2 = if index>c2 then count else
											if index<c1 then (count_vec arr count (index+1) c1 c2) else
											match arr.(index) with
											| Empty -> (count_vec arr count (index+1) c1 c2)
											|_ -> (count_vec arr (count+1) (index+1) c1 c2)
										;;

(*helper_count_vec is used to call the count_vec function whenever index is between r1 & r2 because for these corresponding rows count is to be calculated*)
let helper_count_vec r1 c1 r2 c2 index arr = if index<r1||index>r2 then 0 else count_vec arr 0 0 c1 c2;;

(*full_count checks if index are in bounds and counts the number of non-empty cells. Using Array.mapi i found an array which contains the rowwise count of each array*)
let full_count (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														(s.(r3).(c3) <- Num(float_of_int (Array.fold_right (fun x y -> x+y) (Array.mapi (helper_count_vec r1 c1 r2 c2) s) 0)));
														s;;
(*row_count*)
(*Given an array fill_sheet row fills the count of each row at the given index*)
let rec fill_sheet_row r1 c1 r2 c2 r3 c3 s index count arr: sheet = if count = r2-r1+1 then s 
																	else if arr.(index)=0 then fill_sheet_row r1 c1 r2 c2 r3 c3 s (index+1) count arr 
														  			else begin (s.(r3).(c3) <- Num(float_of_int arr.(index))); 
														  			fill_sheet_row r1 c2 r2 c2 (r3+1) c3 s (index+1) (count+1) arr end;;

(*row_count- First using Array.mapi find an array which contains count of each row and then with help of fill_sheet fill the corresponding indices*)
let row_count (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														fill_sheet_row r1 c1 r2 c2 r3 c3 s 0 0 (Array.mapi (helper_count_vec r1 c1 r2 c2) s)
														;;

(*col_count - Iterative approach to count the non-empty cells and print in corresponding indices*)
let rec col_count_helper (s:sheet) i r2 count c = if i > r2 then count
											  else
												match s.(i).(c) with
												| Empty -> col_count_helper s (i+1) r2 count c
												| _ -> col_count_helper s (i+1) r2 (count+1) c
											;;

let col_count (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														for j = c1 to c2 do
															(s.(r3).(c3+j-c1) <- Num(float_of_int (col_count_helper s r1 r2 0 j)))
														done;
														s;;

(*full_sum*)
let rec sum_vec arr (sum:float) index c1 c2 = if index>c2 then sum else
											if index<c1 then (sum_vec arr sum (index+1) c1 c2) else
											match arr.(index) with
											| Empty -> (sum_vec arr sum (index+1) c1 c2)
											|Num(v) -> (sum_vec arr (sum +. v) (index+1) c1 c2)
										;;

let helper_sum_vec r1 c1 r2 c2 index arr = if index<r1||index>r2 then 0 else int_of_float (sum_vec arr 0. 0 c1 c2);;

let full_sum (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														(s.(r3).(c3) <- Num(float_of_int (Array.fold_right (fun x y -> x+y) (Array.mapi (helper_sum_vec r1 c1 r2 c2) s) 0)));
														s;;

(*row_sum*)
let row_sum (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														fill_sheet_row r1 c1 r2 c2 r3 c3 s 0 0 (Array.mapi (helper_sum_vec r1 c1 r2 c2) s)
														;;
(*col_sum*)
let rec col_sum_helper (s:sheet) (sum:float) i r2 c = if i > r2 then sum
											  else
												match s.(i).(c) with
												| Empty -> col_sum_helper s sum (i+1) r2 c
												| Num(v) -> col_sum_helper s (sum+.v) (i+1) r2 c
											;;

let col_sum (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														for j = c1 to c2 do
															(s.(r3).(c3+j-c1) <- Num(col_sum_helper s 0. r1 r2 j))
														done;
														s;; 

(*full_avg*)
let count_in_range (s:sheet) r1 c1 r2 c2 r3 c3 : float = float_of_int (Array.fold_right (fun x y -> x+y) (Array.mapi (helper_count_vec r1 c1 r2 c2) s) 0);;

let full_avg (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														(s.(r3).(c3) <- Num( (float_of_int (Array.fold_right (fun x y -> x+y) (Array.mapi (helper_sum_vec r1 c1 r2 c2) s) 0)/.count_in_range s r1 c1 r2 c2 r3 c3)));
														s;;

(*row_avg*)
let count_in_row (s:sheet) r1 c1 r2 c2 r3 c3 = (Array.mapi (helper_count_vec r1 c1 r2 c2) s);;

let rec fill_sheet_avg r1 c1 r2 c2 r3 c3 s index count rc_arr arr: sheet = if count = r2-r1+1 then s 
																	else if arr.(index)=0 then fill_sheet_avg r1 c1 r2 c2 r3 c3 s (index+1) count rc_arr arr 
														  			else begin (s.(r3).(c3) <- Num(float_of_int arr.(index) /. float_of_int rc_arr.(index))); 
														  			fill_sheet_avg r1 c2 r2 c2 (r3+1) c3 s (index+1) (count+1) rc_arr arr end;;

let row_avg (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														let rc_arr = count_in_row s r1 c1 r2 c2 r3 c3 in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														fill_sheet_avg r1 c1 r2 c2 r3 c3 s 0 0 rc_arr (Array.mapi (helper_sum_vec r1 c2 r2 c2) s)
(*col_avg*)
let rec col_avg_helper (s:sheet) (avg:float) i r2 c = if i > r2 then avg
											  else
												match s.(i).(c) with
												| Empty -> col_avg_helper s avg (i+1) r2 c
												| Num(v) -> col_avg_helper s (avg+.v) (i+1) r2 c
											;;

let col_avg (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														for j = c1 to c2 do
															(s.(r3).(c3+j-c1) <- Num( (col_avg_helper s 0. r1 r2 j) /. float_of_int (col_count_helper s r1 r2 0 j)))
														done;
														s;;   

(*full_min*)
let rec min_arr (min:float) arr index : float = if index >= Array.length arr then min else if arr.(index) < min then min_arr arr.(index) arr (index+1) else min_arr min arr (index+1);;

let rec min_vec arr (min:float) index c1 c2 = if index>c2 then min else
											if index<c1 then (min_vec arr min (index+1) c1 c2) else
											match arr.(index) with
											| Empty -> min_vec arr min (index+1) c1 c2
											|Num(v) -> if v < min then min_vec arr v (index+1) c1 c2 else min_vec arr min (index+1) c1 c2
										;;

let helper_min_vec r1 c1 r2 c2 index arr = if index<r1||index>r2 then 10000. else min_vec arr 10000. 0 c1 c2;;

let full_min (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														(s.(r3).(c3) <- Num(min_arr 10000. (Array.mapi (helper_min_vec r1 c1 r2 c2) s) 0));
														s;;

(*row_min*)	
let rec fill_sheet_min r1 c1 r2 c2 r3 c3 s index count (arr:farr) : sheet = if count = r2-r1+1 then s 
																	else if arr.(index)=0. then fill_sheet_min r1 c1 r2 c2 r3 c3 s (index+1) count arr 
														  			else begin (s.(r3).(c3) <- Num(arr.(index))); 
														  			fill_sheet_min r1 c2 r2 c2 (r3+1) c3 s (index+1) (count+1) arr end;;

let row_min (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														fill_sheet_min r1 c1 r2 c2 r3 c3 s 0 0 (Array.mapi (helper_min_vec r1 c1 r2 c2) s)
														;;
(*col_min*)
let rec col_min_helper (s:sheet) (min:float) i r2 c = if i > r2 then min
											  else
												match s.(i).(c) with
												| Empty -> col_min_helper s min (i+1) r2 c
												| Num(v) -> if v < min then col_min_helper s v (i+1) r2 c else col_min_helper s min (i+1) r2 c
											;;

let col_min (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														for j = c1 to c2 do
															(s.(r3).(c3+j-c1) <- if col_min_helper s (10000.) r1 r2 j = 10000. then Empty else Num(col_min_helper s (10000.) r1 r2 j))
														done;
														s;; 

(*full_max*)
let rec max_arr (max:float) arr index : float = if index >= Array.length arr then max else if arr.(index) > max then max_arr arr.(index) arr (index+1) else max_arr max arr (index+1);;

let rec max_vec arr (max:float) index c1 c2 = if index>c2 then max else
											if index<c1 then (max_vec arr max (index+1) c1 c2) else
											match arr.(index) with
											| Empty -> max_vec arr max (index+1) c1 c2
											|Num(v) -> if v > max then max_vec arr v (index+1) c1 c2 else max_vec arr max (index+1) c1 c2
										;;

let helper_max_vec r1 c1 r2 c2 index arr = if index<r1||index>r2 then (-1.*.10000.) else max_vec arr (-1.*.10000.) 0 c1 c2;;

let full_max (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														(s.(r3).(c3) <- Num(max_arr (-1.*.10000.) (Array.mapi (helper_max_vec r1 c1 r2 c2) s) 0));
														s;;

(*row_max*)
let rec fill_sheet_max r1 c1 r2 c2 r3 c3 s index count (arr:farr) : sheet = if count = r2-r1+1 then s 
																	else if arr.(index)=0. then fill_sheet_max r1 c1 r2 c2 r3 c3 s (index+1) count arr 
														  			else begin (s.(r3).(c3) <- Num(arr.(index))); 
														  			fill_sheet_max r1 c2 r2 c2 (r3+1) c3 s (index+1) (count+1) arr end;;

let row_max (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														fill_sheet_max r1 c1 r2 c2 r3 c3 s 0 0 (Array.mapi (helper_max_vec r1 c1 r2 c2) s)
														;;
(*col_max*)
let rec col_max_helper (s:sheet) (max:float) i r2 c = if i > r2 then max
											  else
												match s.(i).(c) with
												| Empty -> col_max_helper s max (i+1) r2 c
												| Num(v) -> if v > max then col_max_helper s v (i+1) r2 c else col_max_helper s max (i+1) r2 c
											;;

let col_max (s:sheet) (r:range) (i:index) : sheet = let (r3,c3) = i in
														let (r1,c1,r2,c2) = r in
														if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
														for j = c1 to c2 do
															(s.(r3).(c3+j-c1) <- if col_max_helper s (-1.*.10000.) r1 r2 j = (-1.*.10000.) then Empty else Num(col_min_helper s (10000.) r1 r2 j))
														done;
														s;; 

let get_sum x y = x+.y;;
let get_sub x y = x-.y;;
let get_mult x y = x*.y;;
let get_div x y = x/.y;;

let find_num (s:sheet) i j  = match s.(i).(j) with
							| Empty -> raise Empty_Cells_Found
							| Num(v) -> v
						;;

(*add_const - Given a constant and a range add all the cells of that range to given constant and put the result in given indices range*)
let add_const (s:sheet) (r:range) (c:float) (i:index) :sheet = let (r3,c3) = i in
 																	let (r1,c1,r2,c2) = r in
																	if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
																	for j = 0 to c2-c1 do 
																		for i = 0 to r2-r1 do 
																			(s.(r3+i).(c3+j) <- Num(get_sum (find_num s (r1+i) (c1+j)) c))
																		done
																	done;
																	s;;

(*subt_const - Given a constant and a range subtract all the cells of that range to given constant and put the result in given indices range*)
let subt_const (s:sheet) (r:range) (c:float) (i:index) :sheet = let (r3,c3) = i in
 																	let (r1,c1,r2,c2) = r in
																	if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
																	for j = 0 to c2-c1 do 
																		for i = 0 to r2-r1 do 
																			(s.(r3+i).(c3+j) <- Num(get_sub (find_num s (r1+i) (c1+j)) c))
																		done
																	done;
																	s;;

(*mult_const - Given a constant and a range multiply all the cells of that range to given constant and put the result in given indices range*)
let mult_const (s:sheet) (r:range) (c:float) (i:index) :sheet = let (r3,c3) = i in
 																	let (r1,c1,r2,c2) = r in
																	if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
																	for j = 0 to c2-c1 do 
																		for i = 0 to r2-r1 do 
																			(s.(r3+i).(c3+j) <- Num(get_mult (find_num s (r1+i) (c1+j)) c))
																		done
																	done;
																	s;;
(*div_const - Given a constant and a range divide all the cells of that range to given constant and put the result in given indices range*)
let div_const (s:sheet) (r:range) (c:float) (i:index) :sheet = let (r3,c3) = i in
 																	let (r1,c1,r2,c2) = r in
																	if check_bounds r3 c3 r1 c1 r2 c2 = false then raise IndexOutofBounds else
																	for j = 0 to c2-c1 do 
																		for i = 0 to r2-r1 do 
																			(s.(r3+i).(c3+j) <- Num(get_div (find_num s (r1+i) (c1+j)) c))
																		done
																	done;
																	s;;

(*add_index - Given a cell and a range add all the cells of that range to given index cell and put the result in given indices range*)
let add_index (s:sheet) (r:range) (i1:index) (i2:index) : sheet = let (r3,c3) = i2 in
																	let (rc,cc) = i1 in
																	let c = find_num s rc cc in
 																	let (r1,c1,r2,c2) = r in
																	if r1>m||r2>m||r3>m||rc>m||c1>n||c2>n||c3>n then raise IndexOutofBounds else
																	for j = 0 to c2-c1 do 
																		for i = 0 to r2-r1 do 
																			(s.(r3+i).(c3+j) <- Num(get_sum (find_num s (r1+i) (c1+j)) c))
																		done
																	done;
																	s;;

(*subt_index - Given a cell and a range subt all the cells of that range to given index cell and put the result in given indices range*)
let subt_index (s:sheet) (r:range) (i1:index) (i2:index) : sheet = let (r3,c3) = i2 in
																	let (rc,cc) = i1 in
																	let c = find_num s rc cc in
 																	let (r1,c1,r2,c2) = r in
																	if r1>m||r2>m||r3>m||rc>m||c1>n||c2>n||c3>n then raise IndexOutofBounds else
																	for j = 0 to c2-c1 do 
																		for i = 0 to r2-r1 do 
																			(s.(r3+i).(c3+j) <- Num(get_sub (find_num s (r1+i) (c1+j)) c))
																		done
																	done;
																	s;;

(*mult_index - Given a cell and a range multiply all the cells of that range to given index cell and put the result in given indices range*)
let mult_index (s:sheet) (r:range) (i1:index) (i2:index) : sheet = let (r3,c3) = i2 in
																	let (rc,cc) = i1 in
																	let c = find_num s rc cc in
 																	let (r1,c1,r2,c2) = r in
																	if r1>m||r2>m||r3>m||rc>m||c1>n||c2>n||c3>n then raise IndexOutofBounds else
																	for j = 0 to c2-c1 do 
																		for i = 0 to r2-r1 do 
																			(s.(r3+i).(c3+j) <- Num(get_mult (find_num s (r1+i) (c1+j)) c))
																		done
																	done;
																	s;;

(*div_index - Given a cell and a range divide all the cells of that range to given index cell and put the result in given indices range*)
let div_index (s:sheet) (r:range) (i1:index) (i2:index) : sheet = let (r3,c3) = i2 in
																	let (rc,cc) = i1 in
																	let c = find_num s rc cc in
 																	let (r1,c1,r2,c2) = r in
																	if r1>m||r2>m||r3>m||rc>m||c1>n||c2>n||c3>n then raise IndexOutofBounds else
																	for j = 0 to c2-c1 do 
																		for i = 0 to r2-r1 do 
																			(s.(r3+i).(c3+j) <- Num(get_div (find_num s (r1+i) (c1+j)) c))
																		done
																	done;
																	s;;

(*add_range - Given two ranges of cells which can be 1D or 2D add the corresponding cells and put the value in given index*)
let add_range (s:sheet) (r1:range) (r2:range) (i:index) :sheet = let (r3,c3) = i in
																	let (r11,c11,r12,c12) = r1 in
																	let (r21,c21,r22,c22) = r2 in
																	if (c12-c11 != c22-c21||r12-r11 != r22-r21) then raise IndexOutofBounds else 
																	for j = 0 to c12-c11 do
																		for i = 0 to r12-r11 do 
																			(s.(r3+i).(c3+j) <- Num(get_sum (find_num s (r11+i) (c11+j)) (find_num s (r21+i) (c21+j))))
																		done
																	done;
																	s;;

(*subt_range - Given two ranges of cells which can be 1D or 2D subtract the corresponding cells and put the value in given index*)
let subt_range (s:sheet) (r1:range) (r2:range) (i:index) :sheet = let (r3,c3) = i in
																	let (r11,c11,r12,c12) = r1 in
																	let (r21,c21,r22,c22) = r2 in
																	if (c12-c11 != c22-c21||r12-r11 != r22-r21) then raise IndexOutofBounds else 
																	for j = 0 to c12-c11 do
																		for i = 0 to r12-r11 do 
																			(s.(r3+i).(c3+j) <- Num(get_sub (find_num s (r11+i) (c11+j)) (find_num s (r21+i) (c21+j))))
																		done
																	done;
																	s;;
(*mult_range - Given two ranges of cells which can be 1D or 2D multiply the corresponding cells and put the value in given index*)
let mult_range (s:sheet) (r1:range) (r2:range) (i:index) :sheet = let (r3,c3) = i in
																	let (r11,c11,r12,c12) = r1 in
																	let (r21,c21,r22,c22) = r2 in
																	if (c12-c11 != c22-c21||r12-r11 != r22-r21) then raise IndexOutofBounds else 
																	for j = 0 to c12-c11 do
																		for i = 0 to r12-r11 do 
																			(s.(r3+i).(c3+j) <- Num(get_mult (find_num s (r11+i) (c11+j)) (find_num s (r21+i) (c21+j))))
																		done
																	done;
																	s;; 

(*div_range - Given two ranges of cells which can be 1D or 2D divide the corresponding cells and put the value in given index*)
let div_range (s:sheet) (r1:range) (r2:range) (i:index) :sheet = let (r3,c3) = i in
																	let (r11,c11,r12,c12) = r1 in
																	let (r21,c21,r22,c22) = r2 in
																	if (c12-c11 != c22-c21||r12-r11 != r22-r21) then raise IndexOutofBounds else 
																	for j = 0 to c12-c11 do
																		for i = 0 to r12-r11 do 
																			(s.(r3+i).(c3+j) <- Num(get_div (find_num s (r11+i) (c11+j)) (find_num s (r21+i) (c21+j))))
																		done
																	done;
																	s;;