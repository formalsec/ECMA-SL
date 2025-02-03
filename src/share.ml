(* Copyright (C) 2022-2025 formalsec programmers
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

module Locations = struct
  let interps : string list = Site.Sites.interpreters
  let stdlib : string list = Site.Sites.stdlib
end

let stdlib = List.hd Locations.stdlib

let search (location : string list) (file : string) : string option =
  List.find_map
    (fun dir ->
      let path = Filename.concat dir file in
      if Sys.file_exists path then Some path else None )
    location

let resolve (location : string list) (file : string) : string =
  search location file |> Option.get |> EslBase.Io.read_file |> String.trim

let es5_config () : string = resolve Locations.interps "es5.include"
let es6_config () : string = resolve Locations.interps "es6.include"
let es6_sym_config () : string = resolve Locations.interps "es6-sym.include"
let es6_sym_interp () : string = resolve Locations.interps "es6-sym.cesl"
