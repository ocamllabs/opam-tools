open Bos
open Rresult
open R.Infix

let opam = Cmd.(v "opam" % "exec" % "--")

let opam_tools = Sys.getcwd () ^ "/" ^ Sys.argv.(1)

let check = function
  | Ok v -> v
  | Error (`Msg msg) ->
      Printf.printf "Error: %s\n%!" msg;
      exit 1

let run dir =
  OS.Dir.with_current dir
    (fun () ->
      OS.Cmd.run Cmd.(opam % opam_tools % "-vv")
      >>= (fun () ->
            OS.Cmd.run Cmd.(opam % "dune" % "build" % "@install" % "@doc")
            >>= fun () -> OS.Cmd.run Cmd.(v "ls" % "-la" % "_opam/bin"))
      |> check)
    ()
  |> check

let () =
  List.iter
    (fun proj ->
      let dir = Fpath.v "opamtoolstest" in
      OS.Dir.create dir |> check |> fun _ ->
      ();
      OS.Dir.with_current dir
        (fun () ->
          let dir = Fpath.(dir / proj) in
          Printf.printf "\nCloning %s to %s\n%!" proj (Fpath.to_string dir);
          OS.Cmd.run
            Cmd.(v "opam" % "source" % proj % "--debug" % "--dir" % p dir)
          |> check;
          Printf.printf "Testing: %s\n%!" proj;
          run dir)
        ()
      |> check)
    [ "patdiff"; "mirage"; "atdgen" ]
