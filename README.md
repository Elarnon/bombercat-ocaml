Bombercat OCaml implementation
======================

Dependencies
------------

The bombercat OCaml implementation depends on OCaml (version 4 or more) and ocamlfind,
as well as the Lwt (http://ocsigen.org/lwt/) and lambda-term
(https://github.com/diml/lambda-term) libraries.

You can use OPAM (http://opam.ocamlpro.com/) to install these dependencies.

Compilation
-----------

To compile the bombercat OCaml implementation, use the following command :

  make

This will produce three executables in the directory bin. They should be
executed while in the main directory of the bombercat OCaml client.

To compile the bombercat OCaml implementation with debug symbols, use :

  make debug

This will produce executables in the directory debug, that should be executed
with environnement variable LWT\_LOG set to debug.

To compile the bombercat OCaml client documentation, use :

  make doc

This will produce documentation in the doc directory.

Usage
-----

  ./bin/meta address[:port]

Creates a meta server listening on the given address and port.

  ./bin/game --meta address[:port] address[:port]

Creates a game server connected to the meta server on given address and port.

  ./bin/client address[:port] pseudo

Creates a client that connects to the meta server on given address and port,
with the given pseudo.

