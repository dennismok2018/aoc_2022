module Position = {
  type t = {
    x: int,
    y: int,
  };

  let compare = (t1, t2) =>
    switch (Stdlib.compare(t1.x, t2.x)) {
    | 0 => Stdlib.compare(t1.y, t2.y)
    | c => c
    };

  /* todo */
  let pp = t =>
    "{x:" ++ string_of_int(t.x) ++ " y:" ++ string_of_int(t.y) ++ "}";
};

module PositionSet = Set.Make(Position);

type head =
  | Head(Position.t);
type tail =
  | Tail(Position.t);

let as_head = node =>
  switch (node) {
  | Tail(p) => Head(p)
  };

let footprint_store = ref(PositionSet.singleton({x: 0, y: 0}));

let footprint_with = (tail: list(tail), footprint: ref(PositionSet.t)) =>
  footprint :=
    List.fold_left(
      (set, tail) =>
        switch (tail) {
        | Tail(position) => PositionSet.add(position, set)
        },
      footprint^,
      tail,
    );

module State = {
  type t = {
    head,
    tail: list(tail),
  };

  let pp = t =>
    switch (t.head, t.tail) {
    | (Head(head), tail) =>
      Position.pp(head)
      ++ List.fold_right(
           (e, acc) =>
             switch (e) {
             | Tail(e) => Position.pp(e) ++ acc
             },
           tail,
           "",
         )
    };

  type instruction =
    | Up(int)
    | Right(int)
    | Down(int)
    | Left(int);

  type reaction =
    | NoAct
    | MoveStraight({
        dis_x: int,
        dis_y: int,
      })
    | MoveDiagonal({
        dis_x: int,
        dis_y: int,
      });

  let followup = (reaction, tail, set_ref: ref(PositionSet.t)) =>
    switch (reaction) {
    | NoAct => tail
    | MoveStraight({dis_x, dis_y}) =>
      switch (
        abs(dis_x),
        abs(dis_y),
        dis_x,
        dis_y,
        dis_x > 0,
        dis_y > 0,
        tail,
      ) {
      | (0, range, _, dis_y, _, positivity, Tail(ori)) =>
        let coeff = if (positivity) {1} else {(-1)};
        let stepped =
          List.init(range, n => Tail({x: ori.x, y: ori.y + (n + 1) * coeff}));

        footprint_with(stepped, set_ref);
        Tail({x: ori.x, y: ori.y + dis_y});
      | (range, 0, dis_x, _, positivity, _, Tail(ori)) =>
        let coeff = if (positivity) {1} else {(-1)};
        let stepped =
          List.init(range, n => Tail({x: ori.x + (n + 1) * coeff, y: ori.y}));

        footprint_with(stepped, set_ref);
        Tail({x: ori.x + dis_x, y: ori.y});
      | (_, _, _, _, _, _, _) => invalid_arg("invalid case")
      }
    | MoveDiagonal({dis_x, dis_y}) =>
      switch (
        abs(dis_x),
        abs(dis_y),
        dis_x,
        dis_y,
        dis_x > 0,
        dis_y > 0,
        tail,
      ) {
      | (1, range, dis_x, dis_y, _, positivity, Tail(ori)) =>
        let coeff = if (positivity) {1} else {(-1)};
        let stepped =
          List.init(range, n =>
            Tail({x: ori.x + dis_x, y: ori.y + (n + 1) * coeff})
          );

        footprint_with(stepped, set_ref);
        Tail({x: ori.x + dis_x, y: ori.y + dis_y});
      | (range, 1, dis_x, dis_y, positivity, _, Tail(ori)) =>
        let coeff = if (positivity) {1} else {(-1)};
        let stepped =
          List.init(range, n =>
            Tail({x: ori.x + (n + 1) * coeff, y: ori.y + dis_y})
          );

        footprint_with(stepped, set_ref);
        Tail({x: ori.x + dis_x, y: ori.y + dis_y});
      | (_, _, _, _, _, _, _) => invalid_arg("invalid case")
      }
    };

  let follow = (head, tail, set_ref) =>
    switch (tail, head) {
    | (Tail(t), Head(h)) =>
      let diff_x = h.x - t.x;
      let diff_y = h.y - t.y;
      let reaction =
        switch (abs(diff_x), abs(diff_y), diff_x, diff_y) {
        | (0, 0, _, _) => NoAct
        | (1, 1, _, _) => NoAct
        | (0, 1, _, _) => NoAct
        | (1, 0, _, _) => NoAct
        | (0, _, _, diff_y) =>
          MoveStraight({
            dis_x: 0,
            dis_y:
              if (diff_y > 0) {
                diff_y - 1;
              } else {
                diff_y + 1;
              },
          })
        | (_, 0, diff_x, _) =>
          MoveStraight({
            dis_x:
              if (diff_x > 0) {
                diff_x - 1;
              } else {
                diff_x + 1;
              },
            dis_y: 0,
          })
        | (1, _, diff_x, diff_y) =>
          MoveDiagonal({
            dis_x: diff_x,
            dis_y:
              if (diff_y > 0) {
                diff_y - 1;
              } else {
                diff_y + 1;
              },
          })
        | (_, 1, diff_x, diff_y) =>
          MoveDiagonal({
            dis_x:
              if (diff_x > 0) {
                diff_x - 1;
              } else {
                diff_x + 1;
              },
            dis_y: diff_y,
          })
        | (_, _, _, _) =>
          invalid_arg(
            "Invalid case: h" ++ Position.pp(h) ++ "t" ++ Position.pp(t),
          )
        };

      followup(reaction, tail, set_ref);
    };

  let get_new = n_tail => {
    let tail = List.init(n_tail, _ => Tail({x: 0, y: 0}));
    {head: Head({x: 0, y: 0}), tail};
  };

  let act_on = (instuction, node) =>
    switch (node, instuction) {
    | (Head(p), Up(m)) => List.init(m, n => Head({x: p.x, y: p.y + (n + 1)}))
    | (Head(p), Right(m)) =>
      List.init(m, n => Head({x: p.x + (n + 1), y: p.y}))
    | (Head(p), Down(m)) =>
      List.init(m, n => Head({x: p.x, y: p.y - (n + 1)}))
    | (Head(p), Left(m)) =>
      List.init(m, n => Head({x: p.x - (n + 1), y: p.y}))
    };

  let chain_react_with = (head, tail: list(tail), set_ref) =>
    List.rev(
      List.fold_left(
        (acc, tail) =>
          switch (acc) {
          | [] => [follow(head, tail, set_ref)]
          | [last_tail, ..._] =>
            let head = as_head(last_tail);
            [follow(head, tail, set_ref), ...acc];
          },
        [],
        tail,
      ),
    );

  let mutate_with = (instruction, state_ref, set_ref) => {
    let head = act_on(instruction, state_ref^.head);
    List.fold_left(
      (_, head) => {
        let tail = chain_react_with(head, state_ref^.tail, set_ref);
        state_ref := {head, tail};
      },
      (),
      head,
    );
  };

  let instruction_of = str =>
    switch (String.split_on_char(' ', str)) {
    | ["R", multitude] => Right(int_of_string(multitude))
    | ["L", multitude] => Left(int_of_string(multitude))
    | ["U", multitude] => Up(int_of_string(multitude))
    | ["D", multitude] => Down(int_of_string(multitude))
    | _ => invalid_arg("unrecognized input")
    };
};

let consume_input_text_line_by_line = (~filepath, ~consumer) => {
  let rec consume_with = (~producer) =>
    switch (producer()) {
    | None => consumer(None)
    | Some(line) =>
      consumer(Some(line));
      consume_with(~producer);
    };

  In_channel.with_open_text(filepath, channel =>
    consume_with(~producer=() => In_channel.input_line(channel))
  );
};

let state_store = ref(State.get_new(2));
let line_count = ref(0);

let () =
  consume_input_text_line_by_line(~filepath="./input", ~consumer=opt
    /* consume_input_text_line_by_line ~filepath:"./input" ~consumer:(fun opt -> */
    =>
      switch (opt) {
      | Some(str) =>
        print_endline("ok: " ++ str);
        State.mutate_with(
          State.instruction_of(str),
          state_store,
          footprint_store,
        );
        line_count := line_count^ + 1;
        print_endline(
          string_of_int(line_count^) ++ ": " ++ State.pp(state_store^),
        );
      | None =>
        print_endline("End of file");
        let set_size = ref(0);
        PositionSet.iter(_ => set_size := set_size^ + 1, footprint_store^);
        print_endline(string_of_int(set_size^));
      }
    );
