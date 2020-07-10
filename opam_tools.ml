(* Copyright (c) Anil Madhavapeddy <anil@recoil.org>

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

module OV = Ocaml_version
open Rresult
open R.Infix
open Bos

let opam_root =
  match Exec.run_opam_s Cmd.(v "config" % "var" % "root") with
  | Ok path -> Fpath.v path
  | Error (`Msg msg) ->
      Logs.err (fun l ->
          l
            "You do not seem to have a working opam environment. Try running \
             `opam init` first to get all the package descriptions required, \
             and then re-run this command.\n\n\
             Error was: %s" msg);
      exit 1

let opam_tools_root = Fpath.(opam_root / "plugins" / "opam-tools")

let opam_tools_src = Fpath.(opam_tools_root / "src")

let tool_switch_name ov = Fmt.strf "opam-tools-%a" OV.pp ov

let tools =
  [
    "ocamlformat";
    "merlin";
    "mdx";
    "dune";
    "odoc";
    "ocaml-lsp-server";
    "dune-release";
  ]

let calculate_ocaml_compiler_from_project () =
  let version_from_json_exn json =
    let open Ezjsonm in
    from_string json |> get_dict |> List.assoc "solution"
    |> get_list (fun x -> get_dict x |> List.assoc "install")
    |> List.filter_map (fun x ->
           get_dict x |> List.assoc "name" |> get_string |> function
           | "ocaml-base-compiler" ->
               get_dict x |> List.assoc "version" |> fun x ->
               Some (get_string x)
           | _ -> None)
    |> List.hd
  in
  let ocaml_version_from_json json =
    try Ok (version_from_json_exn json)
    with _ ->
      Error
        (`Msg
          "Unable to parse the results of the opam solver. Please report this \
           bug to the opam-tools repository.")
  in
  let json_file = ".opam-tools-solver.json" in
  Exec.run_opam
    Cmd.(v "install" % "." % "--show-actions" % ("--json=" ^ json_file))
  >>= fun () ->
  OS.File.read Fpath.(v json_file) >>= fun json ->
  OS.File.delete Fpath.(v json_file) >>= fun () ->
  ocaml_version_from_json json >>= fun ovraw ->
  Ocaml_version.of_string ovraw >>= fun ov ->
  Logs.info (fun l ->
      l "Selected OCaml %a as the best compiler for this project." OV.pp ov);
  Ok ov

let opam_file_for_ocaml prefix ov =
  Fmt.strf
    {|opam-version: "2.0"
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
  |}
    (OV.to_string ov) Fpath.pp prefix

let install_ocaml_in_tools ov =
  let ovs = OV.to_string ov in
  let src = Fpath.(opam_tools_src / ("ocaml." ^ ovs)) in
  let system_src = Fpath.(opam_tools_src / ("ocaml-system." ^ ovs)) in
  let prefix = Fpath.(opam_tools_root / ovs) in
  let pkg = Fmt.strf "ocaml-base-compiler.%s" ovs in
  let system_pkg = Fmt.strf "ocaml-system.%s" ovs in
  OS.Path.delete ~recurse:true src >>= fun () ->
  Exec.run_opam Cmd.(v "source" % pkg % "--dir" % p src) >>= fun () ->
  OS.Path.delete ~recurse:true system_src >>= fun () ->
  Exec.run_opam Cmd.(v "source" % system_pkg % "--dir" % p system_src)
  >>= fun () ->
  (OS.File.exists Fpath.(prefix / "bin" / "ocamlc") >>= function
   | true ->
       Logs.debug (fun l ->
           l "Using existing OCaml installation in %a" Fpath.pp prefix);
       Ok ()
   | false -> Exec.install_ocaml_to ~prefix ~src ())
  >>= fun () ->
  OS.Path.link ~force:true
    ~target:Fpath.(system_src / "gen_ocaml_config.ml")
    Fpath.(prefix / "gen_ocaml_config.ml")
  >>= fun () ->
  OS.File.write
    Fpath.(prefix / "ocaml-system.opam")
    (opam_file_for_ocaml prefix ov)

let create_tools_switch ov =
  Exec.run_opam_l Cmd.(v "switch" % "list" % "-s") >>= fun all_sw ->
  let sw = tool_switch_name ov in
  match List.exists (( = ) sw) all_sw with
  | true -> Ok ()
  | false ->
      Logs.info (fun l -> l "Creating switch %s to use for tools" sw);
      let sw_compiler = OV.Opam.V2.name ov in
      Exec.run_opam
        Cmd.(v "switch" % "create" % sw % sw_compiler % "--no-switch")

let ocamlformat_version_l =
  lazy
    ( match OS.File.read_lines (Fpath.v ".ocamlformat") with
    | Ok f ->
        List.filter_map (Astring.String.cut ~sep:"=") f
        |> List.assoc_opt "version"
    | Error (`Msg _) -> None )

let ocamlformat_version () = Lazy.force ocamlformat_version_l

let install_tools_in_tools_switch ov =
  let tools =
    match ocamlformat_version () with
    | None -> tools
    | Some v ->
        List.map (function "ocamlformat" -> "ocamlformat." ^ v | x -> x) tools
  in
  let args = Cmd.(v "--switch" % tool_switch_name ov) in
  (* TODO until release of lsp in opam *)
  Exec.run_opam
    Cmd.(
      v "pin" % "add" % "-ny" % "ocaml-lsp-server"
      % "https://github.com/ocaml/ocaml-lsp.git" %% args)
  >>= fun () -> Exec.run_opam Cmd.(v "install" % "-y" %% of_list tools %% args)

let setup_local_switch ov =
  let local_switch = Fpath.v "_opam" in
  OS.Dir.exists local_switch >>= function
  | false ->
      Logs.info (fun l -> l "Creating local opam switch for project");
      Exec.run_opam Cmd.(v "switch" % "create" % "." % "--empty") >>= fun () ->
      ( match ov with
      | Some ov -> Ok ov
      | None -> calculate_ocaml_compiler_from_project () )
      >>= fun ov ->
      let compiler_dir = Fpath.(opam_tools_root / OV.to_string ov) in
      install_ocaml_in_tools ov >>= fun () ->
      Exec.run_opam
        Cmd.(
          v "pin" % "add" % "-y" % "--inplace-build" % "ocaml-system"
          % p compiler_dir)
      >>= fun () -> Ok ov
  | true ->
      Exec.run_opam_s
        Cmd.(
          v "show" % "ocaml" % "-f" % "version" % "--normalise"
          % "--color=never")
      >>= fun ovraw ->
      OV.of_string ovraw >>= fun ov ->
      Logs.debug (fun l ->
          l "Local switch directory exists already with OCaml %a" OV.pp ov);
      Ok ov

let copy_binaries_for_package ov dst pkg =
  let sw = tool_switch_name ov in
  Exec.run_opam_l Cmd.(v "show" % "--list-files" % pkg % "--switch" % sw)
  >>= fun paths ->
  let tocopy =
    List.filter_map
      (fun src ->
        let dir, file = Fpath.v src |> Fpath.split_base in
        let _, dtype = Fpath.split_base dir in
        if Fpath.to_string dtype = "bin/" then
          Some (Fpath.v src, Fpath.(dst // file))
        else None)
      paths
  in
  match tocopy with
  | [] ->
      Logs.err (fun l ->
          l "Tool %s did not install any binaries for OCaml %a. Internal error."
            pkg OV.pp ov);
      exit 1
  | (_, dst) :: _ as l ->
      OS.Dir.create ~path:true (Fpath.parent dst) >>= fun _ ->
      Exec.iter
        (fun (target, dst) ->
          Logs.debug (fun l ->
              l "Linking %a <- %a" Fpath.pp dst Fpath.pp target);
          OS.Path.link ~force:true ~target dst)
        l

let copy_tools_to_local_switch ov =
  create_tools_switch ov >>= fun () ->
  install_tools_in_tools_switch ov >>= fun () ->
  let dstdir = Fpath.(v "_opam" / "bin") in
  Exec.iter (copy_binaries_for_package ov dstdir) tools

let setup_local_switch ov =
  Logs.debug (fun l -> l "Setting up a local switch");
  setup_local_switch ov >>= fun ov ->
  Logs.debug (fun l -> l "Using OCaml version %a for tools" OV.pp ov);
  copy_tools_to_local_switch ov

open Cmdliner

let setup_logs () =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  let global_option_section = "COMMON OPTIONS" in
  Term.(
    const setup_log
    $ Fmt_cli.style_renderer ~docs:global_option_section ()
    $ Logs_cli.level ~docs:global_option_section ())

let ov_term : OV.t option Term.t =
  let ov_conv = Arg.conv ~docv:"OCAML_VERSION" (OV.of_string, OV.pp) in
  let doc = "Version of the OCaml compiler to use for this project" in
  Arg.(
    value
    & opt (some ov_conv) None
    & info [ "c"; "compiler" ] ~docv:"COMPILER" ~doc)

let cmd_term =
  let run ov () = setup_local_switch ov in
  Term.(pure run $ ov_term $ setup_logs ())

let cmd_info =
  Term.info "opam-tools" ~version:"TODO"
    ~doc:"Install the development tools needed for a project in a local switch"
    ~man:[ `S "DESCRIPTION" ]

let () = Term.(exit @@ eval (cmd_term, cmd_info))
