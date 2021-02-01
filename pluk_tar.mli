(*
 * Copyright (C) 2021 Ramil Farkhshatov
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Tar processing base *)

module Header : sig
  (** Create and process TAR headers *)

  (** Entry name is too long exception *)
  exception Name_too_long of string

  (** Type of TAR entity *)
  type type_ =
    | Normal
    | Hard_link
    | Symbolic_link
    | Character
    | Block
    | Directory
    | Fifo
    | Reserved of char

  (** Type representing TAR entity header *)
  type t = {
    name: string;
    mode: int;
    uid: int;
    gid: int;
    size: int64;
    mtime: int64;
    typeflag: type_;
    link: string option;
    dev_major: int;
    dev_minor: int;
    uname: string;
    gname: string;
  }

  (** Create TAR entity header *)
  val create :
    name:string ->
    mode:int ->
    size:int64 ->
    ?mtime:int64 ->
    ?uid:int ->
    ?gid:int ->
    ?typeflag:type_ ->
    ?link:string option ->
    ?dev_major:int ->
    ?dev_minor:int ->
    ?uname:string ->
    ?gname:string ->
    unit ->
    t

  (** Deserialize TAR entity header *)
  val of_bytes : bytes -> t

  (** Serialize TAR entity header calculating its checksum *)
  val to_bytes : t -> bytes
end

(** General I/O error exception *)
exception IO_error

(** Type of IO module for Make functor *)
module type IO = sig
  (** {1:flow Control flow} *) 
  type 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t

  (** {1:io Input output definitions} *)
  type in_channel
  type out_channel
  val write: out_channel -> bytes -> int -> int -> int t
  val read: in_channel -> bytes -> int -> int -> int t
  val close_in: in_channel -> unit t
  val close_out: out_channel -> unit t
end

module type Archive = sig
  (** Type of Make functor result module *)

  module IO: IO

  val extract: IO.in_channel ->
               (Header.t -> IO.out_channel option IO.t) ->
               unit IO.t
  (** [extract input proc] extracts TAR archive from [input] calling [proc]
      for each entry. [proc] is responsible for classifying entry type
      creating directories if required and it returns a channel for files
      for contents to be written to. Can raise IO_error exception. *)

  val create: IO.out_channel ->
              (unit -> (Header.t * IO.in_channel option) IO.t) Seq.t ->
              unit IO.t
  (** [create output seq] creates TAR archive and writes it to [output].
      Can raise IO_error exception. *)

  val copy: ?block_size:int ->
            IO.in_channel ->
            IO.out_channel ->
            int64 ->
            unit IO.t
  (** Utility function for passing data between channels
      [copy input output len] copies [len] bytes from [input] channel to
      [output] using blocks of [block_size] bytes length. Raises IO_error on 
      failures. *)
end

module Make: functor (IO : IO) -> sig
  (** Functor to create module of type Archive specialized by IO module. *)

  module IO: IO with type out_channel = IO.out_channel
                     and type in_channel = IO.in_channel
                     and type 'a t = 'a IO.t

  val extract:
    IO.in_channel ->
    (Header.t -> IO.out_channel option IO.t) -> unit IO.t

  val create:
    IO.out_channel ->
    (unit -> (Header.t * IO.in_channel option) IO.t) Seq.t ->
    unit IO.t

  val copy: ?block_size:int -> IO.in_channel -> IO.out_channel -> int64 ->
            unit IO.t
end
