(lang dune 3.16)

(name dlink_control)

(generate_opam_files true)

(source
 (github andersfugmann/dlink-control))

(authors "Anders Fugmann")

(maintainers "Anders Fugmann")

(license LICENSE)

;(documentation https://url/to/documentation)

(package
 (name dlink_control)
 ;(synopsis "A short synopsis")
 ;(description "A longer description")
 (depends ocaml dune ppx_protocol_conv_json)
 ;(tags
  ; (topics "to describe" your project))
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
