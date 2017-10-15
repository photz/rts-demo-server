# rts-demo-server

Server for a simple real-time strategy game played in the browser.
The client can be found [here](https://github.com/photz/rts-demo-client).

## Setup

In order to compile and run the server OPAM, the OCaml Package Manager, is required.  Instructions on how to install OPAM can be found [here](https://opam.ocaml.org/doc/Install.html).

After having installed OPAM, switch to a less recent compiler version as some libraries we use may not yet be compatible.

```sh
opam switch 4.04.2
```

```sh
opam install ocamlbuild
opam install ocamlfind
opam install websocket-lwt
opam install sqlite3
opam install core
opam install yojson
```
