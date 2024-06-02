(lang dune 3.7)

(name bproc)

(generate_opam_files true)

; (source (github camlspotter/bsky_ml))

(authors "Jun FURUSE <jun.furuse@dailambda.jp>")

(maintainers "Jun FURUSE <jun.furuse@dailambda.jp>")

(package
 (name bproc)
 (synopsis "Business process")
 (depends (ocaml (>= "5.1.0"))
;	   lwt_domain

 	   ; ocaml (>= "4.14.1"))

 	   dune

	   yojson
	   (ppx_yojson_conv (>= "v0.16.0"))
           ppx_yojson_conv_lib
	   cmdliner

;           (hex (>= "1.5.0"))
;	   cstruct
;	   ppx_cstruct
;	   bigstring
;
;	   mirage-crypto
;	   mirage-crypto-ec
;	   mirage-crypto-rng
;	   hacl-star
;	   base64
;           (lwt (>= "5.4.1"))
           (mtime (>= "2.0.0"))

	   js_of_ocaml
           js_of_ocaml-ppx
;	   promise_jsoo

           (alcotest (>= "1.5.0"))

;	   dream
;	   dream-httpaf
;	   dream-pure
;
;           httpaf-lwt-unix
;	   memtrace
	   posix-uname

           ISO8601
	   monaco_jsoo
	   )
 (version "1.0.0"))
