let write_file filename string => {
  let outchan = open_out filename;
  Printf.fprintf outchan "%s" string;
  close_out outchan
};

let run_command command outchan => {
  let process_inchan = Unix.open_process_in command;
  try (
    while true {
      let s = input_line process_inchan;
      output_string outchan (s ^ "\n");
      flush outchan
    }
  ) {
  | End_of_file => close_in process_inchan
  | exn => raise exn
  }
};

let get_addr () => Unix.((gethostbyname @@ gethostname ()).h_addr_list.(0));

let establish_server server addr => {
  let domain = Unix.domain_of_sockaddr addr;
  let sock = Unix.socket domain Unix.SOCK_STREAM 0;
  switch addr {
  | Unix.ADDR_UNIX s => print_endline s
  | Unix.ADDR_INET s i => print_endline @@ Unix.string_of_inet_addr s
  };
  Unix.bind sock addr;
  Unix.listen sock 1;
  while true {
    let (s, caller) = Unix.accept sock;
    switch (Unix.fork ()) {
    | 0 =>
      if (Unix.fork () != 0) {
        exit 0
      };
      let inchan = Unix.in_channel_of_descr s
      and outchan = Unix.out_channel_of_descr s;
      server inchan outchan;
      exit 0
    | id =>
      Unix.close s;
      ignore @@ Unix.waitpid [] id
    }
  }
};

let do_thing inchan outchan =>
  switch (input_line inchan) {
  | "exec" =>
    let filename = "./_build/scripts/" ^ input_line inchan ^ ".native";
    if (Sys.file_exists filename) {
      run_command filename outchan
    }
  | "load" =>
    let filename = input_line inchan;
    let length = input_line inchan;
    let lengthToRead = int_of_string length;
    let s = really_input_string inchan lengthToRead;
    write_file ("./scripts/" ^ filename ^ ".re") s;
    run_command
      (
        "eval $(./node_modules/.bin/dependencyEnv) && ./node_modules/.bin/nopam && ./node_modules/reason/src/rebuild.sh ./scripts/" ^
        filename ^ ".native"
      )
      outchan
  | _ =>
    output_string outchan "I got it! *Snap*! You're retarded\n";
    flush outchan
  };

establish_server do_thing (Unix.ADDR_INET (get_addr ()) 5001);
