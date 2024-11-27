module ViteCallback = struct
  type t = < meta : meta > Js.t
  and meta = < watchMode : bool > Js.t
  and cb = (t -> unit -> unit[@mel.this])

  let empty : cb = fun [@mel.this] _ _ -> ()
  let watch_mode (t : t) : bool = t##meta##watchMode

  type 'a vite_error = 'a -> unit

  external error : t -> 'a vite_error = "error" [@@mel.get]
end

type ('a, 'b) plugin =
  { name : string
  ; enforce : string
  ; version : string
  ; resolveId : string -> string -> 'a -> 'b Js.Promise.t
  ; buildStart : ViteCallback.cb
  }

type configuration =
  { root : string
  ; target : string
  ; alias : string option
  ; prefix : string option
  }

let deps_directory ~config ~dune =
  match dune with
  | true -> Node.Path.join [| "_build"; "default"; config.root; config.target; "node_modules" |]
  | false -> Node.Path.join [| config.root; config.target; "node_modules" |]
;;

let find_file dir source =
  let path = Node.Path.join [| dir; source |] in
  if Node.Fs.existsSync path then Some path else None
;;

let find_src ~prefix config source =
  let prefix_len = String.length prefix in
  let stripped = String.sub source (prefix_len + 1) (String.length source - prefix_len - 1) in
  Js.Console.log ("STRIPPED", stripped);
  let src_path = Node.Path.join [| config.root; config.target |] in
  find_file src_path stripped
;;

let resolve_sources ~config ~dune =
  let prefix = Option.value config.prefix ~default:"@melange" in
  let melange_deps = deps_directory ~config ~dune in
  fun source _ _ ->
    Js.Promise.make (fun ~resolve ~reject:_ ->
      let resolved =
        match source with
        | source when String.starts_with ~prefix source -> find_src ~prefix config source
        | source -> find_file melange_deps source
      in
      let resolved =
        resolved
        |> Option.map (fun path -> [%mel.obj { id = path; moduleSideEffects = Js.null }])
        |> Js.Nullable.fromOption
      in
      (resolve resolved [@u]))
;;

let melangeWithoutDune (config : configuration) =
  { name = "vite-plugin-ocaml-no-dune"
  ; enforce = "pre"
  ; version = "0.1"
  ; buildStart = ViteCallback.empty
  ; resolveId = resolve_sources ~config ~dune:false
  }
;;

external spawnSync : string -> string array -> < .. > Js.t = "spawnSync"
[@@mel.module "child_process"]

let melangeWithDune (config : configuration) =
  let alias = Option.value config.alias ~default:"@melange" in
  { name = "vite-plugin-ocaml"
  ; enforce = "pre"
  ; version = "0.1"
  ; buildStart =
      (fun [@mel.this] vite () ->
        match ViteCallback.watch_mode vite with
        | true -> Js.Console.log "======= Watching ======"
        | false ->
          let child = spawnSync "dune" [| "build"; alias |] in
          let code = child##status in
          begin
            match code with
            | 0 -> ()
            | _ -> ViteCallback.error vite (child##stderr##toString ())
          end)
  ; resolveId = resolve_sources ~config ~dune:true
  }
;;

(* let is_melange_source_file (id : string) = *)
(*   String.ends_with id ~suffix:".ml" *)
(*   || String.ends_with id ~suffix:".mlx" *)
(*   || String.ends_with id ~suffix:".re" *)
(*   || String.ends_with id ~suffix:".res" *)
(* ;; *)
