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

let fin (process_f : in_channel -> 'a) (file : string) : 'a =
  let ic = open_in file in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> process_f ic)

let fout (process_f : out_channel -> 'a) (file : string) : 'a =
  let oc = open_out file in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> process_f oc)

let read_in_channel (ic : in_channel) : string =
  let in_sz = in_channel_length ic in
  really_input_string ic in_sz

let write_out_channel (data : string) (oc : out_channel) : unit =
  output_string oc data

let read_file (file : string) : string = fin read_in_channel file

let write_file (file : string) (data : string) : unit =
  fout (write_out_channel data) file
