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

module State = {
  type t = {
    mutable head,
    mutable tail: list(tail),
  };

  type instruction =
    | Up(int)
    | Right(int)
    | Down(int)
    | Left(int);

  type reaction =
    | NoAct
    | MoveBy({
        dis_x: int,
        dis_y: int,
      });

  let followup = (reaction, tail) =>
    switch (reaction) {
    | NoAct => tail
    | MoveBy({dis_x, dis_y}) =>
      switch (tail) {
      | Tail(ori) => Tail({x: ori.x + dis_x, y: ori.y + dis_y})
      }
    };

  let follow = (head, tail) =>
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
          MoveBy({
            dis_x: 0,
            dis_y:
              if (diff_y > 0) {
                diff_y - 1;
              } else {
                diff_y + 1;
              },
          })
        | (_, 0, diff_x, _) =>
          MoveBy({
            dis_x:
              if (diff_x > 0) {
                diff_x - 1;
              } else {
                diff_x + 1;
              },
            dis_y: 0,
          })
        | (1, _, diff_x, diff_y) =>
          MoveBy({
            dis_x: diff_x,
            dis_y:
              if (diff_y > 0) {
                diff_y - 1;
              } else {
                diff_y + 1;
              },
          })
        | (_, 1, diff_x, diff_y) =>
          MoveBy({
            dis_x:
              if (diff_x > 0) {
                diff_x - 1;
              } else {
                diff_x + 1;
              },
            dis_y: diff_y,
          })
        | (_, _, _, _) => invalid_arg("Invalid case")
        };

      followup(reaction, tail);
    };

  let get_new = n_tail => {
    let tail = List.init(n_tail, _ => Tail({x: 0, y: 0}));
    {head: Head({x: 0, y: 0}), tail};
  };

  let act_on = (node, instuction) =>
    switch (node, instuction) {
    | (Head(p), Up(m)) => Head({x: p.x, y: p.y + m})
    | (Head(p), Right(m)) => Head({x: p.x + m, y: p.y})
    | (Head(p), Down(m)) => Head({x: p.x, y: p.y - m})
    | (Head(p), Left(m)) => Head({x: p.x - m, y: p.y})
    };

  let chain_react_with = (head, tail) => tail;

  let mutate_with = (instruction, state) => {
    let head = act_on(state.head, instruction);
    let tail = chain_react_with(head, state.tail);
    state.head = head;
    state.tail = tail;
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

let produce_instruction_with = str =>
  State.(
    switch (String.split_on_char(' ', str)) {
    | ["R", multitude] => Right(int_of_string(multitude))
    | ["L", multitude] => Left(int_of_string(multitude))
    | ["U", multitude] => Up(int_of_string(multitude))
    | ["D", multitude] => Down(int_of_string(multitude))
    | _ => invalid_arg("unrecognized input")
    }
  );

let state = State.get_new(1);

let () =
  consume_input_text_line_by_line(~filepath="./input", ~consumer=o =>
    switch (o) {
    | Some(s) =>
      State.mutate_with(produce_instruction_with(s), state);
      ();
    | None => print_endline("End of file")
    }
  );
