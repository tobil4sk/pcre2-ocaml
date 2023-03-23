(lang dune 2.7)
(name pcre2)

(generate_opam_files true)

(source (github tobil4sk/pcre2-ocaml))
(license "LGPL-2.1-or-later with OCaml-LGPL-linking-exception")
(homepage "https://mmottl.github.io/pcre-ocaml")
(documentation "https://mmottl.github.io/pcre-ocaml/api")

(maintainers "Markus Mottl <markus.mottl@gmail.com>")
(authors "Markus Mottl <markus.mottl@gmail.com>")

(package
  (name pcre2)
  (synopsis "Bindings to the Perl Compatibility Regular Expressions library")
  (description "\
pcre2-ocaml offers library functions for string pattern matching and
substitution, similar to the functionality offered by the Perl language.")
  (depends
    (ocaml (>= 4.12))
    dune-configurator
    (conf-libpcre2-8 :build)
    base-bytes
  )
)
