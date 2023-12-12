
type ticket =  Prize of string | Ticket of ticket Lazy.t ;;
let table = [
   Ticket(lazy (Prize "kot"));
   Ticket(lazy (Ticket (lazy(Prize "lody"))));
   Ticket(lazy (Ticket (lazy( Ticket (lazy( Prize "wodka"))))));
   Ticket(lazy (Ticket (lazy (Prize "nic"))));
   Ticket(lazy (Prize "maliny"))
];;

let buyTicket tbl n =
  if n < 1 then tbl
  else
    let rec inner skipped ls m = 
    match ls with
    [] -> List.rev skipped
    | h::t ->
      if m = 0 then
        match h with
          Ticket(smth) -> List.rev_append skipped ( Lazy.force smth :: t)
        | Prize(_) -> List.rev_append skipped (h::t)
      else inner (h::skipped) t (m-1)    
  in
  inner [] tbl (n-1)
;;

buyTicket table 3;;
buyTicket table 1;;
buyTicket table 0;;
buyTicket table 6;;
buyTicket table 3;;



buyTicket (buyTicket (buyTicket (buyTicket table 2) 2) 5) 2;;

buyTicket (buyTicket (buyTicket (buyTicket table 2) 2) 5) 1;;

buyTicket (buyTicket (buyTicket (buyTicket (buyTicket table 2) 2) 5) 3) 3;;

buyTicket (buyTicket (buyTicket (buyTicket table 6) 0) 5) 2;;

buyTicket (buyTicket (buyTicket (buyTicket (buyTicket table 5) 6) 0) 5) 2;;

