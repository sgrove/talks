open Async;

type scheme =
  | HTTP
  | HTTPS
  | WebSocket
  | WebSocketSecure
  | Other(string);

let stringOfScheme =
  fun
  | HTTP => "http"
  | HTTPS => "https"
  | WebSocket => "socket"
  | WebSocketSecure => "secure"
  | Other(string) => string;

type respContentType =
  | Json
  | PlainText
  | HTML
  | Missing
  | Other(string);

let stringOfContentType =
  fun
  | Json => "JSON"
  | PlainText => "TEXT"
  | HTML => "HTML"
  | Other(other) => "OTHER-" ++ other
  | Missing => "MISSING";

type event = {
  scheme: option(scheme),
  host: option(string),
  path: string,
  query: option(list((string, string))),
  reqBody: option(string),
  reqHeaders: list(string),
  respHeaders: list(string),
  status: int,
  respContentType,
};

let process = uri => {
  let scheme =
    uri
    |> Uri.scheme
    |> Ooption.map(
         _,
         fun
         | "http" => HTTP
         | "https" => HTTPS
         | "ws" => WebSocket
         | "wss" => WebSocketSecure
         | other => Other(other),
       );

  Cohttp_async.Client.get(uri)
  >>| (
    ((resp, body)) => {
      let headers = resp.headers;
      let status = resp.status |> Cohttp.Code.code_of_status;
      let respContentType =
        headers
        |> Cohttp.Header.get(_, "content-type")
        |> (
          fun
          | None => Missing
          | Some("application/json; charset=utf-8") => Json
          | Some("text/html") => HTML
          | Some("text/plain") => PlainText
          | Some(other) => Other(other)
        );

      let respHeaders =
        headers
        |> Cohttp.Header.to_list
        |> List.map(((name, _value)) => name);

      let event = {
        scheme,
        host: Uri.host(uri),
        path: Uri.path(uri),
        query: None,
        reqHeaders: [],
        reqBody: None,
        respHeaders,
        status,
        respContentType,
      };

      event;
    }
  );
};

let uris =
  [
    "https://api.npmjs.org/downloads/point/last-day",
    "https://api.npmjs.org/downloads/point/last-day",
    "https://api.npmjs.org/downloads/point/last-day",
    "https://api.npmjs.org/downloads/point/last-day",
    "https://api.npmjs.org/downloads/point/last-week",
    "https://api.npmjs.org/downloads/point/last-day",
    "https://api.npmjs.org/downloads/point/last-day",
    "https://api.npmjs.org/downloads/point/last-day",
    "https://api.npmjs.org/downloads/point/last-month",
    "https://api.npmjs.org/downloads/point/last-month/graphql",
    "https://api.npmjs.org/downloads/point/last-month/graphql",
    "https://api.npmjs.org/downloads/point/last-month/graphql",
    "https://api.npmjs.org/downloads/point/what",
    "https://api.npmjs.org/downloads/point/last-day",
    "https://api.npmjs.org/downloads/point/last-day",
  ]
  |> List.map(Uri.of_string);

module CodeSet =
  Set.Make({
    let compare = Pervasives.compare;
    type t = int;
  });

module ContentTypeSet =
  Set.Make({
    let compare = Pervasives.compare;
    type t = respContentType;
  });

type respRecords = {
  codes: CodeSet.t,
  contentTypes: ContentTypeSet.t,
};

type pathRecords = {resps: respRecords};

type hostRecords = {paths: Hashtbl.t(string, ref(pathRecords))};

type records = {hosts: Hashtbl.t(string, hostRecords)};

let records = {hosts: Hashtbl.create(17)};

let makeEmptyResps = () =>
  ref({
    resps: {
      codes: CodeSet.empty,
      contentTypes: ContentTypeSet.empty,
    },
  });

let start = () => {
  uris
  |> List.mapi((idx, uri) => process(uri))
  |> Deferred.all
  >>| (
    events => {
      events
      |> List.mapi((idx, event) => {
           let recs = records;
           let host = event.host |> Ooption.default("placeholder");
           let path = event.path;

           let (isNovel, reason) =
             switch (Hashtbl.find_opt(recs.hosts, host)) {
             | None =>
               let paths = Hashtbl.create(17);
               Hashtbl.add(paths, path, makeEmptyResps());
               Hashtbl.add(recs.hosts, host, {paths: paths});
               (true, Some("host"));

             | Some({paths}) =>
               switch (Hashtbl.find_opt(paths, path)) {
               | None =>
                 Hashtbl.add(paths, path, makeEmptyResps());
                 (true, Some("path"));

               | Some(respsRef) =>
                 let {resps} = respsRef^;
                 let statusNovel = CodeSet.mem(event.status, resps.codes);
                 let contentTypeNovel =
                   ContentTypeSet.mem(
                     event.respContentType,
                     resps.contentTypes,
                   );
                 switch (statusNovel, contentTypeNovel) {
                 | (false, false) => (false, None)
                 | _ =>
                   respsRef :=
                     {
                       resps: {
                         codes: CodeSet.add(event.status, resps.codes),
                         contentTypes:
                           ContentTypeSet.add(
                             event.respContentType,
                             resps.contentTypes,
                           ),
                       },
                     };
                   (true, Some("statusOrContent"));
                 };
               }
             };

           Format.sprintf(
             "%d. Novel ? %b%s -> Called %s://%s%s with %s type response [%d]",
             idx,
             isNovel,
             reason
             |> Ooption.fmapDefault("", _, reason => " (" ++ reason ++ ")"),
             event.scheme |> Ooption.fmapDefault("unknown", _, stringOfScheme),
             event.host |> Ooption.default("[no host]"),
             event.path,
             event.respContentType |> stringOfContentType,
             event.status,
           );
         });
    }
  )
  >>| (items => print_endline(String.concat("\n", items)));
};

let () =
  Command.async(
    ~summary="Echo POST requests",
    Command.Param.(
      map(
        flag(
          "-p",
          optional_with_default(9255, int),
          ~doc="int Source port to listen on",
        ),
        ~f=(_, ()) =>
        start()
      )
    ),
  )
  |> Command.run;
