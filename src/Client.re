
let read_file filename => {
  let inchan = open_in filename;
  let length = in_channel_length inchan;
  let input = really_input_string inchan length;
  close_in inchan;
  (input, length)
};

let open_connection addr => {
  let domain = Unix.domain_of_sockaddr addr;
  let sock = Unix.socket domain Unix.SOCK_STREAM 0;
  try {
    Unix.connect sock addr;
    (Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock)
  } {
  | exn =>
    Unix.close sock;
    raise exn
  }
};

let shutdown_connection inchan =>
  Unix.shutdown (Unix.descr_of_in_channel inchan) Unix.SHUTDOWN_SEND;

let do_thing addr client_fun => {
  let server_addr = Unix.inet_addr_of_string addr;
  let port = 5001;
  let sockaddr = Unix.ADDR_INET server_addr port;
  let (ic, oc) = open_connection sockaddr;
  client_fun ic oc;
  shutdown_connection ic;
  ()
};

let client_fun command filename inchan outchan =>
  try {
    flush stdout;
    switch command {
    | "exec" =>
      output_string outchan "exec\n";
      output_string outchan (filename ^ "\n");
      flush outchan;
      while true {
        let r = input_line inchan;
        Printf.printf "%s\n" r;
        flush stdout
      }
    | "load" =>
      output_string outchan "load\n";
      output_string outchan (filename ^ "\n");
      let (filecontent, length) = read_file @@ (filename ^ ".re");
      output_string outchan (string_of_int length ^ "\n");
      output_string outchan filecontent;
      flush outchan;
      while true {
        let r = input_line inchan;
        Printf.printf "%s\n" r;
        flush stdout
      }
    | _ => print_endline "Not a command"
    }
  } {
  | End_of_file => ()
  | exn =>
    shutdown_connection inchan;
    raise exn
  };

if (Array.length Sys.argv >= 4) {
  do_thing Sys.argv.(1) (client_fun Sys.argv.(2) Sys.argv.(3))
} else {
  print_endline "Client.native [ip-address] [exec | load] [filename]"
};
