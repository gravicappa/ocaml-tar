module IO = struct
  type in_channel = Unix.file_descr
  type out_channel = Unix.file_descr
  type 'a t = 'a

  let bind a proc = proc a

  let return a = a

  let rec with_restart op fd buf off len =
    try op fd buf off len with
    | Unix.Unix_error (Unix.EINTR,_,_) -> with_restart op fd buf off len
    | _ -> 0

  let write fd bytes off len =
    with_restart Unix.write fd bytes off len

  let read fd bytes off len =
    with_restart Unix.read fd bytes off len

  let close_in fd = Unix.close fd

  let close_out fd = Unix.close fd
end

module Tar = Pluk_tar

module Archive = Tar.Make(IO)

exception Unsupported_file_type

let header_of_file path =
  let stat = Unix.LargeFile.lstat path in
  let typeflag = match stat.st_kind with
                 | S_REG -> Tar.Header.Normal
                 | S_DIR -> Directory
                 | S_CHR -> Character
                 | S_BLK -> Block
                 | S_LNK -> Symbolic_link
                 | S_FIFO -> Fifo
                 | _ -> raise_notrace Unsupported_file_type in
  let link = match stat.st_kind with
             | S_LNK -> Some (Unix.readlink path)
             | _ -> None in
  let uname = try (Unix.getpwuid stat.st_uid).pw_name with
              | Not_found -> "" in
  let gname = try (Unix.getgrgid stat.st_gid).gr_name with
              | Not_found -> "" in
  Tar.Header.create ~name: path
                    ~mode: stat.st_perm
                    ~size: stat.st_size
                    ~uid: stat.st_uid
                    ~gid: stat.st_gid
                    ~uname 
                    ~gname 
                    ~mtime: (Int64.of_float stat.st_mtime)
                    ~typeflag
                    ~link
                    ~dev_major: stat.st_dev
                    ~dev_minor: stat.st_rdev
                    ()

let extract fd proc =

  let mkdir_rec dir permissions =
    let ensure_dir path =
      match Sys.file_exists path && Sys.is_directory path with
      | true -> ()
      | false -> Unix.mkdir path permissions in

    let rec loop i =
      match String.index_from_opt dir i '/' with
      | None -> ensure_dir dir
      | Some pos ->
          let () = String.sub dir 0 pos |> ensure_dir in
          loop (pos + 1) in

    loop 1 in

  let proc (hdr : Tar.Header.t) =
    match hdr.typeflag with
    | Normal ->
        let name, mode = proc hdr.name hdr.mode in
        mkdir_rec (Filename.dirname name) (mode lor 0o111);
        let fd = Unix.(openfile name [O_CREAT; O_WRONLY; O_TRUNC] mode) in
        IO.(return (Some fd))
    | Hard_link ->
        let name, _ = proc hdr.name hdr.mode in
        let _ = Option.map (fun link -> Unix.link link name) hdr.link in
        IO.return None
    | Symbolic_link ->
        let name, _ = proc hdr.name hdr.mode in
        let _ = Option.map (fun link -> Unix.symlink link name) hdr.link in
        IO.return None
    | Directory ->
        let name, mode = proc hdr.name hdr.mode in
        mkdir_rec name mode;
        IO.return None
    | _ -> IO.return None in

  Archive.extract fd proc

let extract_from_file ?(mode_proc = Fun.id) path target =
  let fd = Unix.openfile path [O_RDONLY] 0 in
  let proc path mode = Filename.concat target path, mode_proc mode in
  Fun.protect ~finally: (fun () -> Unix.close fd) @@ fun () ->
    extract fd proc

let create_from_files fd proc files =
  let mk_entry path =
    fun () ->
      let hdr = header_of_file path |> proc in
      let fd = match hdr.Tar.Header.typeflag with
               | Normal -> Some (Unix.openfile path [O_RDONLY] 0)
               | _ -> None in
      hdr, fd in
  
  files
  |> Seq.map mk_entry
  |> Archive.create fd

let chop_prefix ~prefix a =
  let prefix_len = String.length prefix in
  let rec loop i =
    if i >= String.length a then
      a
    else if i >= prefix_len then
      String.sub a i (String.length a - i)
    else if a.[i] <> prefix.[i] then
      a
    else
      loop (i + 1) in
  loop 0

let create_file_from_files ?(strip_prefix = "") ?(convert = Fun.id) tar files =
  let fd = Unix.openfile tar [O_CREAT; O_TRUNC; O_WRONLY] 0o644 in
  let prefix = strip_prefix in
  let proc hdr =
    { hdr with Tar.Header.name = chop_prefix ~prefix hdr.Tar.Header.name }
    |> convert in
  Fun.protect ~finally: (fun () -> Unix.close fd) @@ fun () ->
    create_from_files fd proc files
