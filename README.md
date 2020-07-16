# opam-tools - initialise development environment for an OCaml project

NAME
       opam-tools - Install development tools within a local switch

SYNOPSIS
       opam-tools [OPTION]... 

DESCRIPTION
       opam-tools installs a local development environment for an OCaml
       project. It first sets up an opam local switch, which is an _opam
       directory that contains all the dependencies required to build your
       code. Since you also need some development tools for building, testing
       and documenting your code, it installs the binaries for those inside
       _opam/bin.

       The opam package manager automatically adds this to your PATH for most
       shells, or else it will be added automatically if you use opam exec --
       to run your commands. Thus, the end result of invoking opam-tools is
       that all the tools and dependencies will be available locally after
       the command completes.

       If you ever need to refresh the versions of the tools, just run opam
       update to get the latest package descriptions, and opam tools again to
       reinstall them.

OPTIONS
       -c COMPILER, --compiler=COMPILER
           Version of the OCaml compiler to use for this project. If omitted,
           this defaults to the most recent version that is compatible with
           the packages used by the project.

       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --no-install=VAL (absent=false)
           When creating a local switch, don't look for any local package
           definitions to install.

       --tools=TOOLS
       (absent=ocamlformat,merlin,mdx,dune,odoc,ocaml-lsp-server,dune-release)
           Tools to install within the local switch. These can be any opam
           packages, but only the binaries will be copied to the local
           switch.

       --version
           Show version information.

COMMON OPTIONS
       --color=WHEN (absent=auto)
           Colorize the output. WHEN must be one of `auto', `always' or
           `never'.

       -q, --quiet
           Be quiet. Takes over -v and --verbosity.

       -v, --verbose
           Increase verbosity. Repeatable, but more than twice does not bring
           more.

       --verbosity=LEVEL (absent=warning)
           Be more or less verbose. LEVEL must be one of `quiet', `error',
           `warning', `info' or `debug'. Takes over -v.

SEE ALSO
       dune(1), dune-release(1), mdx(1), merlin(1), ocaml-lsp-server(1),
       ocamlformat(1), odoc(1), opam(1)

# Contact

- https://discuss.ocaml.org/ in the Ecosystem category for queries
  or general comments about this plugin.
- https://github.com/ocaml-opam/opam-tools/issues for any bug reports
  or PRs.

