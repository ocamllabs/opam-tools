open Bos
open Rresult
open R.Infix

let opam = Cmd.(v "opam" % "exec" % "--")

let run dir =
  OS.Dir.with_current dir
    (fun () ->
      OS.Cmd.run Cmd.(opam % "opam-tools" % "-vv")
      >>= (fun () ->
            OS.Cmd.run Cmd.(opam % "dune" % "build" % "@install" % "@doc")
            >>= fun () -> OS.Cmd.run Cmd.(v "ls" % "-la" % "_opam/bin"))
      |> R.get_ok)
    ()
  |> R.get_ok

let () =
  List.iter
    (fun proj ->
      OS.Dir.with_tmp
        (format_of_string "opamtools%s")
        (fun dir () ->
          Printf.printf "Testing: %s\n%!" proj;
          run dir)
        ()
      |> R.get_ok)
    [ "patdiff"; "mirage"; "irmin" ]
