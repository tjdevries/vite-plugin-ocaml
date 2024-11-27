(lang dune 3.17)

(name vite-plugin-ocaml)

;; Use Melange for Javascript Compilation
(using melange 0.1)
(using directory-targets 0.1)

(package
 (name vite-plugin-ocaml)
 (allow_empty)
 (depends ocaml dune melange)) 
