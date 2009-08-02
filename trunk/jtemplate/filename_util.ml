(**
This program is free software; you can redistribute it and / or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

Filename utilities 

@author Tony BenBrahim < tony.benbrahim at gmail.com >

*)

(** 
converts a relative filename and path into an absolute filename 
@param dir relative of absolute path
@param filename 
@return absolute path of file
*)
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