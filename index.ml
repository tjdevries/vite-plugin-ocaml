module ViteCallback = struct
  type t = < meta : meta > Js.t
  and meta = < watchMode : bool > Js.t
  and cb = (t -> unit -> unit[@mel.this])

  let empty : cb = fun [@mel.this] _ _ -> ()
  let watch_mode (t : t) : bool = t##meta##watchMode

  (* TODO: Would be nice to pass non-strings here, it can work I think *)
  (* Could be useful to do `mel.unwrap` *)
  let info : t -> string -> unit = [%raw "(x, a) => { x.info(a) }"]
  let warn : t -> string -> unit = [%raw "(x, a) => { x.warn(a) }"]
  let error : t -> string -> unit = [%raw "(x, a) => { x.error(a) }"]
end

type watch_change = (ViteCallback.t -> string -> string -> unit[@mel.this])

type ('a, 'b) plugin =
  { name : string
  ; enforce : string
  ; version : string
  ; resolveId : string -> string -> 'a -> 'b Js.Promise.t
  ; buildStart : ViteCallback.cb
  ; watchChange : watch_change
  }

type configuration =
  { root : string
  ; target : string
  ; alias : string option
  ; prefix : string option
  }

let src_directory ~config ~dune dirs =
  let prefix =
    match dune with
    | true -> [| "_build"; "default"; config.root; config.target |]
    | false -> [| config.root; config.target |]
  in
  Node.Path.join (Array.concat [ prefix; dirs ])
;;

let deps_directory ~config ~dune = src_directory ~config ~dune [| "node_modules" |]

let find_file dir source =
  let path = Node.Path.join [| dir; source |] in
  if Node.Fs.existsSync path then Some path else None
;;

external absolute : string -> string = "resolve" [@@mel.module "path"]

let find_src ~prefix ~config ~dune source =
  let prefix_len = String.length prefix in
  let stripped = String.sub source (prefix_len + 1) (String.length source - prefix_len - 1) in
  let src_path = src_directory ~config ~dune [| config.root |] in
  find_file src_path stripped |> Option.map absolute
;;

let resolve_sources ~config ~dune =
  let prefix = Option.value config.prefix ~default:"@melange" in
  let melange_deps = deps_directory ~config ~dune in
  fun source _ _ ->
    Js.Promise.make (fun ~resolve ~reject:_ ->
      let resolved =
        match source with
        | source when String.starts_with ~prefix source -> find_src ~prefix ~config ~dune source
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
  ; watchChange = (fun [@mel.this] vite _ _ -> ())
  }
;;

external spawn : string -> string array -> < .. > Js.t = "spawn" [@@mel.module "child_process"]

external spawnSync : string -> string array -> < .. > Js.t = "spawnSync"
[@@mel.module "child_process"]

let is_melange_source_file (id : string) =
  String.ends_with id ~suffix:".ml"
  || String.ends_with id ~suffix:".mlx"
  || String.ends_with id ~suffix:".re"
  || String.ends_with id ~suffix:".res"
;;

let melangeWithDune (config : configuration) =
  (* Disable dune lock, so that we can run this without conflicting with other stuff *)
  let _ = Node.Process.putEnvVar "DUNE_CONFIG__GLOBAL_LOCK" "disabled" in
  (* Start the plugin *)
  let alias = Option.value config.alias ~default:"@melange" in
  { name = "vite-plugin-ocaml-yes-dune"
  ; enforce = "pre"
  ; version = "0.1"
  ; buildStart =
      (fun [@mel.this] vite () ->
        match ViteCallback.watch_mode vite with
        | true ->
          ViteCallback.info vite "Starting Watch Mode";
          let dune = spawn "dune" [| "build"; "--passive-watch-mode" |] in
          dune##stderr##on "data" (fun data -> ViteCallback.warn vite ("DATA:" ^ data));
          dune##stdout##on "data" (fun data -> ViteCallback.warn vite ("DATA: " ^ data));
          dune##on "close" (fun code ->
            match code with
            | 0 -> ViteCallback.error vite "QUIT WITHOUT ERROR?"
            | _ -> ViteCallback.error vite "CLOSED UNEXPECTEDLY")
          (* let code = child##status in *)
          (* begin *)
          (*   match code with *)
          (*   | 0 -> () *)
          (*   | _ -> ViteCallback.error vite (child##stderr##toString ()) *)
          (* end *)
        | false ->
          let child = spawnSync "dune" [| "build"; alias |] in
          let code = child##status in
          begin
            match code with
            | 0 -> ()
            | _ -> ViteCallback.error vite (child##stderr##toString ())
          end)
  ; resolveId = resolve_sources ~config ~dune:true
  ; watchChange =
      (fun [@mel.this] vite id evt ->
        Js.Console.log ("watchChange", id, evt);
        match is_melange_source_file id with
        | true ->
          ()
          (* let child = spawn "dune" [| "rpc"; "build"; "." |] in *)
          (* match child##status with *)
          (* | 0 -> () *)
          (* | _ ->  *)
        | false -> ())
  }
;;
