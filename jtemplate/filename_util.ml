
(** removes .. and . from paths *)
(* works on windows, need to check on Unix *)
let resolve_filename dir filename =
	let rec cleanup check ok =
		let right = Filename.basename check in
		let left = Filename.dirname check in
		if (right ="." && Filename.dirname left = left) then
			Filename.concat left ok
		else
			match right with
			| "." -> cleanup left ok
			| ".." -> cleanup (Filename.dirname left) ok
			| "" -> ok
			| _ -> cleanup left (if ok ="" then right else Filename.concat right ok)
	in
	if Filename.is_relative filename then
		cleanup (Filename.concat dir filename) ""
	else
		filename