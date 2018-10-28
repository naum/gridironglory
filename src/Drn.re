
Random.self_init();

let rec roll = () => {
  let r = Random.int(6);
  switch (r) {
    | 5 => 5 + roll()
    | _ => r
  };
}

let drn = () => {
  roll() + roll();  
};

let randSkill = () => {
  1 + roll() + roll() + roll();
};
