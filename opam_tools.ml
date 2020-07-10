module OV = Ocaml_version
open Rresult
open R.Infix
open Bos

let opam_root =
  match Exec.run_opam_s Cmd.(v "config" % "var" % "root") with
  | Ok path -> Fpath.v path
  | Error (`Msg msg) ->
      Logs.err (fun l -> l "You do not seem to have a working opam environment. Try running `opam init` first to get all the package descriptions required, and then re-run this command.\n\nError was: %s" msg);
      exit 1

let opam_tools_root = Fpath.(opam_root / "plugins" / "opam-tools")
let opam_tools_src = Fpath.(opam_tools_root / "src")

let opam_file_for_ocaml prefix ov =
  Fmt.strf {|opam-version: "2.0"
version: %S
synopsis: "The OCaml compiler (installed by opam-tools)"
maintainer: "platform@lists.ocaml.org"
depends: [
  "ocaml" {post}
  "base-unix" {post}
  "base-threads" {post}
  "base-bigarray" {post}
]
conflict-class: "ocaml-core-compiler"
flags: compiler
build: ["bin/ocaml" "gen_ocaml_config.ml"]
setenv: [ PATH += "%a/bin" ]
  |} (OV.to_string ov) Fpath.pp prefix

let install_ocaml_in_tools ov =
  let ovs = OV.to_string ov in
  let src = Fpath.(opam_tools_src / ("ocaml."^ovs)) in
  let system_src = Fpath.(opam_tools_src / ("ocaml-system."^ovs)) in
  let prefix = Fpath.(opam_tools_root / ovs) in
  let pkg = Fmt.strf "ocaml-base-compiler.%s" ovs in
  let system_pkg = Fmt.strf "ocaml-system.%s" ovs in
  OS.Path.delete ~recurse:true src >>= fun () ->
  Exec.run_opam Cmd.(v "source" % pkg % "--dir" % p src) >>= fun () ->
  OS.Path.delete ~recurse:true system_src >>= fun () ->
  Exec.run_opam Cmd.(v "source" % system_pkg % "--dir" % p system_src) >>= fun () ->
  (OS.File.exists Fpath.(prefix / "bin" / "ocamlc") >>= function
    | true -> Logs.debug (fun l -> l "Using existing OCaml installation in %a" Fpath.pp prefix); Ok ()
    | false -> Exec.install_ocaml_to ~prefix ~src ()) >>= fun () ->
  OS.Path.link ~force:true ~target:Fpath.(system_src / "gen_ocaml_config.ml") Fpath.(prefix / "gen_ocaml_config.ml") >>= fun () ->
  OS.File.write Fpath.(prefix / "ocaml-system.opam") (opam_file_for_ocaml prefix ov)

let tool_switch_name ov =
  Printf.sprintf "opam-tools-%s" (OV.to_string ov)

let create_tool_switch ov =
  Exec.run_opam_l Cmd.(v "switch" % "list" % "-s")  >>= fun all_sw ->
  let sw = tool_switch_name ov in
  match List.exists ((=) sw) all_sw with
  | true -> Ok ()
  | false ->
      Logs.info (fun l -> l "Creating switch %s to use for tools" sw);
      let sw_compiler = OV.Opam.V2.name ov in
      Exec.run_opam Cmd.(v "switch" % "create" % sw % sw_compiler % "--no-switch")

let ocamlformat_version () =
  match OS.File.read_lines (Fpath.v ".ocamlformat") with
  | Ok f ->
      List.filter_map (Astring.String.cut ~sep:"=") f |>
      List.assoc_opt "version"
  | Error (`Msg _) -> None

let copy_binaries_for_package ~ofv ov pkg =
  let sw = tool_switch_name ov in
  Exec.run_opam_l Cmd.(v "show" % "--list-files" % pkg % "--switch" % sw) >>= fun paths ->
  let tocopy = List.filter_map (fun src ->
    let dir, file = Fpath.v src |> Fpath.split_base in
    let rootdir, dtype = Fpath.split_base dir in
    if Fpath.to_string dtype = "bin/" then
      Some (Fpath.v src, Fpath.(rootdir / "tools" // (if Fpath.to_string file = "ocamlformat" then Fpath.v ofv else file)))
    else None) paths in
  match tocopy with
  | [] -> Logs.err (fun l -> l "Tool %s did not install any binaries for OCaml %a. Internal error." pkg OV.pp ov); exit 1
  | (_,dst)::_ as l ->
     OS.Dir.create ~path:false (Fpath.parent dst) >>= fun _ ->
     Exec.iter (fun (target, dst) ->
       Logs.debug (fun l -> l "Linking %a -> %a" Fpath.pp dst Fpath.pp target);
       OS.Path.link ~force:true ~target dst) l

let install_tools ov =
  let ofpkg, ofv = match ocamlformat_version () with
    | None -> [], "ocamlformat"
    | Some v -> ["ocamlformat."^v], ("ocamlformat-"^v) in
  let tools = ofpkg @ ["merlin"; "mdx"; "dune"; "odoc"; "ocaml-lsp-server"] in
  let args = Cmd.(v "--switch" % tool_switch_name ov) in
  Exec.run_opam Cmd.(v "pin" % "add" % "-ny" % "ocaml-lsp-server" % "https://github.com/ocaml/ocaml-lsp.git" %% args) >>= fun () ->
  Exec.run_opam Cmd.(v "install" % "-y" %% of_list tools %% args) >>= fun () ->
  Exec.iter (copy_binaries_for_package ~ofv ov) tools

let setup_local_switch _ov =
  let local_switch = Fpath.v "_opam" in
  OS.Dir.exists local_switch >>= function
  | false ->
      Logs.info (fun l -> l "Creating local opam switch for project");
      Exec.run_opam Cmd.(v "switch" % "create" % "." % "--empty") 
  | true -> Logs.debug (fun l -> l "Local switch directory exists already");  Ok ()
       
open Cmdliner
let setup_logs () =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ()) in
  let global_option_section = "COMMON OPTIONS" in
  Term.(const setup_log
    $ Fmt_cli.style_renderer ~docs:global_option_section ()
    $ Logs_cli.level ~docs:global_option_section ())

let cmd_term =
  let run () =
    let ov = OV.of_string_exn "4.10.0" in
    install_ocaml_in_tools ov >>= fun () ->
    create_tool_switch ov >>= fun () ->
    install_tools ov
  in
  Term.(pure run $ setup_logs ())

let cmd_info =
  Term.info "opam-tools"
    ~version:"TODO"
    ~doc:"Install the development tools needed for a project in a local switch"
    ~man:[
      `S "DESCRIPTION";
    ]

let () = Term.(exit @@ eval (cmd_term, cmd_info))
