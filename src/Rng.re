Random.self_init();

let rollDie = () =>
    Random.int(6) + 1;

let rollDice = () =>
    rollDie() + rollDie();
