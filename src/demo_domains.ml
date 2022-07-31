open Domainslib

let run () =
  let pool = Task.setup_pool ~num_additional_domains:3 () in
  ignore pool