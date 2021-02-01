[%%cstruct
type header = {
  name: uint8_t [@len 100];
  mode: uint8_t [@len 8];
  uid: uint8_t [@len 8];
  gid: uint8_t [@len 8];
  size: uint8_t [@len 12];
  mtime: uint8_t [@len 12];
  checksum: uint8_t [@len 8];
  typeflag: uint8_t;
  linkname: uint8_t [@len 100];
  magic: uint8_t [@len 6];
  version: uint8_t [@len 2];
  uname: uint8_t [@len 32];
  gname: uint8_t [@len 32];
  devmajor: uint8_t [@len 8];
  devminor: uint8_t [@len 8];
  prefix: uint8_t [@len 155];
  pad: uint8_t [@len 12];
} [@@little_endian]]

module Header = struct
  exception Name_too_long of string

  type type_ =
    | Normal
    | Hard_link
    | Symbolic_link
    | Character
    | Block
    | Directory
    | Fifo
    | Reserved of char

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

  let create ~name ~mode ~size ?(mtime = 0L) ?(uid = 0) ?(gid = 0)
             ?(typeflag = Normal) ?(link = None) ?(dev_major = 0)
             ?(dev_minor = 0) ?(uname = "") ?(gname = "") () =
    { name; mode; uid; gid; size; mtime; typeflag; link; dev_major; dev_minor;
      uname; gname }

  let split_path_opt n name =
    let len = String.length name in
    let rec loop off =
      if len - off > n then
        match String.index_from_opt name (off + 1) '/' with
        | Some p -> loop p
        | None -> None
      else
        Some (String.sub name (off + 1) (len - off - 1),
              String.sub name 0 off) in
    if len < n then
      Some (name, "")
    else
      loop 0

  let split_path_exc n name =
    match split_path_opt n name with
    | Some a -> a
    | None -> raise_notrace (Name_too_long name)
  
  let to_cstruct t =
    let padded n str =
      if String.length str = n then
        str
      else
        let bytes = Bytes.create n in
        let len = String.length str in
        Bytes.blit_string str 0 bytes 0 len;
        Bytes.fill bytes len (n - len) '\000';
        Bytes.to_string bytes in

    let oct n a = Printf.sprintf "%0*o\000" (n - 1) a in

    let oct_long n a = Printf.sprintf "%0*Lo\000" (n - 1) a in

    let char_of_typeflag = function
      | Normal -> '0'
      | Hard_link -> '1'
      | Symbolic_link -> '2'
      | Character -> '3'
      | Block -> '4'
      | Directory -> '5'
      | Fifo -> '6'
      | Reserved c -> c in

    let c = Cstruct.create sizeof_header in
    let path = match t.typeflag with
               | Directory -> t.name ^ "/"
               | _ -> t.name in
    let size = match t.typeflag with
               | Normal -> t.size
               | _ -> 0L in
    let name, prefix = split_path_exc 99 path in
    set_header_name (padded 100 name) 0 c;
    set_header_mode (oct 8 t.mode) 0 c;
    set_header_uid (oct 8 t.uid) 0 c;
    set_header_gid (oct 8 t.gid) 0 c;
    set_header_size (oct_long 12 size) 0 c;
    set_header_mtime (oct_long 12 t.mtime) 0 c;
    set_header_checksum (String.make 8 ' ') 0 c;
    set_header_typeflag c (char_of_typeflag t.typeflag |> Char.code);
    set_header_linkname (Option.value ~default: "" t.link |> padded 100) 0 c;
    set_header_magic "ustar\000" 0 c;
    set_header_version "00" 0 c;
    set_header_uname (padded 32 t.uname) 0 c;
    set_header_gname (padded 32 t.gname) 0 c;
    set_header_devmajor (oct 8 t.dev_major) 0 c;
    set_header_devminor (oct 8 t.dev_minor) 0 c;
    set_header_prefix (padded 155 prefix) 0 c;
    c

  let of_cstruct c =
    let str s =
      match String.index_opt s '\000' with
      | Some p -> String.sub s 0 p
      | None -> s in

    let num s =
      Scanf.sscanf s "%o" Fun.id in

    let num_long s =
      Scanf.sscanf s "%Lo" Fun.id in

    let typeflag_of_char = function
      | '0' -> Normal
      | '1' -> Hard_link
      | '2' -> Symbolic_link
      | '3' -> Character
      | '4' -> Block
      | '5' -> Directory
      | '6' -> Fifo
      | c -> Reserved c in

    let prefix = copy_header_prefix c |> str in
    let name = copy_header_name c |> str in
    let name = match prefix with
               | "" -> name
               | prefix -> Filename.concat prefix name in
    let mode = copy_header_mode c |> num in
    let uid = copy_header_uid c |> num in
    let gid = copy_header_gid c |> num in
    let size = copy_header_size c |> num_long in
    let mtime = copy_header_mtime c |> num_long in
    let typeflag = get_header_typeflag c |> Char.chr |> typeflag_of_char in
    let link = match copy_header_linkname c |> str with
               | "" -> None
               | link -> Some link in
    let dev_major = copy_header_devmajor c |> num in
    let dev_minor = copy_header_devminor c |> num in
    create ~name ~mode ~size ~uid ~gid ~mtime ~typeflag ~link ~dev_major
           ~dev_minor ()

  let of_bytes bytes =
    Cstruct.of_bytes bytes
    |> of_cstruct

  let calculate_checksum header =
    let rec loop ck i =
      if i < sizeof_header then
        loop (ck + Bytes.get_uint8 header i) (i + 1)
      else
        ck in
    loop 0 0

  let to_bytes t =
    let bytes = to_cstruct t
                |> Cstruct.to_bytes in
    let cksum_field = calculate_checksum bytes
                      |> Printf.sprintf "%07o\000" in
    Bytes.blit_string cksum_field 0 bytes 148 8;
    bytes
end

exception IO_error

module type IO = sig
  type in_channel
  type out_channel
  type 'a t

  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
  val write: out_channel -> bytes -> int -> int -> int t
  val read: in_channel -> bytes -> int -> int -> int t
  val close_in: in_channel -> unit t
  val close_out: out_channel -> unit t
end

module type Archive = sig
  module IO: IO

  val extract: IO.in_channel ->
               (Header.t -> IO.out_channel option IO.t) ->
               unit IO.t

  val create: IO.out_channel ->
              (unit -> (Header.t * IO.in_channel option) IO.t) Seq.t ->
              unit IO.t

  val copy: ?block_size:int ->
            IO.in_channel ->
            IO.out_channel ->
            int64 ->
            unit IO.t
end

module Make (IO: IO) : Archive with module IO = IO = struct
  module IO = IO

  let ( let* ) = IO.bind

  let io_exactly proc bytes off len =
    let rec loop off len =
      if len > 0 then
        let* m = proc bytes off len in
        if m = 0 then
          raise IO_error
        else
          loop (off + m) (len - m)
      else
        IO.return () in
    loop off len

  let write_exactly output = io_exactly (IO.write output)

  let read_exactly input = io_exactly (IO.read input)

  let copy ?(block_size = 8 * 1024 * 1024) chan_in chan_out n =
    let block = Bytes.create block_size in
    let rec loop n =
      if n > 0L then
        let bytes_to_read = Int64.(to_int (min (of_int block_size) n)) in
        let* bytes_read = IO.read chan_in block 0 bytes_to_read in
        match bytes_read with
        | 0 -> raise IO_error
        | bytes_read ->
            let* () = write_exactly chan_out block 0 bytes_read in
            loop Int64.(sub n (of_int bytes_read))
      else
        IO.return () in
    loop n

  let skip chan_in n =
    let block_size = 8 * 1024 * 1024 in
    let block = Bytes.create block_size in
    let rec loop n =
      if n > 0L then
        let bytes_to_read = Int64.(to_int (min (of_int block_size) n)) in
        let* bytes_read = IO.read chan_in block 0 bytes_to_read in
        match bytes_read with
        | 0 -> raise IO_error
        | bytes_read -> loop Int64.(sub n (of_int bytes_read))
      else
        IO.return () in
    loop n

  let block_pad n = Int64.(logand (add n 511L) (lognot 511L))

  let extract input proc =
    let process_entry hdr = 
      let* extract = proc hdr in
      match extract with
      | None -> skip input (block_pad hdr.Header.size)
      | Some output ->
          let* () = copy input output hdr.size in
          let* () = IO.close_out output in
          skip input Int64.(sub (block_pad hdr.size) hdr.size) in

    let bytes = Bytes.create 512 in

    let rec loop () =
      let* () = read_exactly input bytes 0 sizeof_header in
      if Bytes.get_uint8 bytes 0 = 0 then
        IO.return ()
      else
        let* () = Header.of_bytes bytes |> process_entry in
        loop () in

    loop ()

  let create output files =
    let block = Bytes.make 512 '\000' in

    let pad size =
      let pad = Int64.(sub (block_pad size) size |> to_int) in
      write_exactly output block 0 pad in

    let pack input hdr =
      let* () = copy input output hdr.Header.size in
      let* () = pad hdr.size in
      IO.close_in input in

    let rec close_and_loop input next =
      match input with
      | Some input ->
          let* () = IO.close_in input in
          loop next
      | None -> loop next

    and loop files =
      match files () with
      | Seq.Nil ->
          let* () = write_exactly output block 0 sizeof_header in
          write_exactly output block 0 sizeof_header
      | Seq.Cons (proc, next) ->
          let* r = proc () in
          let hdr_bytes = r |> fst |> Header.to_bytes in
          let* () = write_exactly output hdr_bytes 0 sizeof_header in
          match r with
          | { typeflag = Normal; _ } as hdr, Some input ->
              let* () = pack input hdr in
              loop next
          | { typeflag = Hard_link; _ } as hdr, Some input ->
              let* () = pack input hdr in
              loop next
          | _, input -> close_and_loop input next in
    loop files
end
