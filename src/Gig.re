let citypool = [|
  "New York",
  "Los Angeles",
  "Chicago",
  "Dallas",
  "Houston",
  "Washington",
  "Miami",
  "Philadelphia",
  "Atlanta",
  "Boston",
  "Phoenix",
  "San Francisco",
  "Riverside",
  "Detroit",
  "Seattle",
  "Minneapolis",
  "San Diego",
  "Tampa",
  "Denver",
  "Baltimore",
  "St. Louis",
  "Charlotte",
  "Orlando",
  "San Antonio",
  "Portland",
  "Pittsburgh",
  "Sacramento",
  "Las Vegas",
  "Cincinnati",
  "Kansas City",
  "Austin",
  "Columbus",
|];

let draw2Cities = () => {
  let shuffled = Belt.Array.shuffle(citypool);
  (shuffled[0], shuffled[1]);
};

let playsPerHalf = 24;

type play = (char, int);

type scoreboard = {
  gameover: bool,
  teams: array(string),
  score: array(int),
  poss: int,
  half: int,
  playcount: int,
  los: int,
  down: int,
  ytg: int,
};

let initScoreboard = (vteam, hteam) => {
  gameover: false,
  teams: [|vteam, hteam|],
  score: [|0, 0|],
  poss: 0,
  half: 0,
  playcount: 0,
  los: 20,
  down: 1,
  ytg: 10,
};

let generatePlayMatrix = diceroll =>
  switch (diceroll) {
  | 2 => ('C', 100)
  | 3 => ('I', 10)
  | 4 => ('R', 20)
  | 5 => ('S', (-10))
  | 6 => ('R', 5)
  | 7 => ('X', 0)
  | 8 => ('C', 5)
  | 9 => ('C', 25)
  | 10 => ('R', 0)
  | 11 => ('F', 0)
  | 12 => ('C', 30)
  | _ => failwith("Diceroll out of range!")
  };

let tallySafety = sb => {
  let newScore = sb.score;
  let scoringSide = 1 - sb.poss;
  newScore[scoringSide] = newScore[scoringSide] + 2;
  {...sb, score: newScore, poss: scoringSide, down: 1, ytg: 10, los: 20};
};

let downCheck = (yg, newLos, sb) =>
  switch (yg) {
  | _ when yg >= sb.ytg => {...sb, down: 1, ytg: 10, los: newLos}
  | _ when sb.down == 4 => {
      ...sb,
      down: 1,
      ytg: 10,
      los: 100 - newLos,
      poss: 1 - sb.poss,
    }
  | _ => {...sb, down: sb.down + 1, ytg: sb.ytg - yg, los: newLos}
  };

let touchback = sb => {...sb, poss: 1 - sb.poss, down: 1, ytg: 10, los: 20};

let tallyTouchdown = sb => {
  let newScore = sb.score;
  newScore[sb.poss] = newScore[sb.poss] + 6;
  if (Rng.rollDice() >= 5) {
    newScore[sb.poss] = newScore[sb.poss] + 1;
  };
  {...sb, score: newScore, poss: 1 - sb.poss, down: 1, ytg: 10, los: 20};
};

let advanceBall = (yg, sb) => {
  let newLos = sb.los + yg;
  switch (newLos) {
  | _ when newLos <= 0 => tallySafety(sb)
  | _ when newLos < 100 => downCheck(yg, newLos, sb)
  | _ when newLos >= 100 => tallyTouchdown(sb)
  | _ => failwith("WTF?")
  };
};

let turnoverBall = (yg, sb) => {
  let newLos = sb.los + yg;
  switch (newLos) {
  | _ when newLos >= 100 => touchback(sb)
  | _ =>
    let newLos = 100 - newLos;
    let newPoss = 1 - sb.poss;
    {...sb, poss: newPoss, los: newLos};
  };
};

let resolvePlayMatrix = (pm, sb) => {
  let (pc, yg) = pm;
  switch (pc) {
  | 'C'
  | 'R'
  | 'S'
  | 'X' => advanceBall(yg, sb)
  | 'I'
  | 'F' => turnoverBall(yg, sb)
  | _ => failwith("Invalid play matrix!")
  };
};

let advanceBall = (yg, sb) => {
  let newLos = sb.los + yg;
  switch (newLos) {
  | _ when newLos <= 0 => tallySafety(sb)
  | _ when newLos < 100 => downCheck(yg, newLos, sb)
  | _ when newLos >= 100 => tallyTouchdown(sb)
  | _ => failwith("Where are we?")
  };
};

let tickPlayClock = sb => {
  let newPlaycount = sb.playcount + 1;
  switch (sb.half) {
  | 0 when newPlaycount >= playsPerHalf => {
      ...sb,
      playcount: 0,
      half: 1,
      poss: 1,
      down: 1,
      ytg: 10,
      los: 20,
    }
  | 1 when newPlaycount >= playsPerHalf => {
      ...sb,
      playcount: newPlaycount,
      gameover: true,
    }
  | _ => {...sb, playcount: newPlaycount}
  };
};

let displayPlayResult = (pm, sb) => {
  let (pc, yg) = pm;
  Printf.sprintf(
    "%2d %2d %-12s %3d %2d %2d   %c %3d", 
    sb.half, sb.playcount, sb.teams[sb.poss], sb.los, sb.down, sb.ytg, pc, yg
    ) |> Js.log
}

let displayScore = (sb) => {
  Js.log4(sb.teams[0], sb.score[0], sb.teams[1], sb.score[1]);
}

let kickoff = (vteam, hteam) => {
  let sb = initScoreboard(vteam, hteam);
  let rec whistle = sb => {
    let pm = generatePlayMatrix(Rng.rollDice());
    displayPlayResult(pm, sb);
    let newboard = resolvePlayMatrix(pm, sb);
    let newboard = tickPlayClock(newboard);
    displayScore(newboard);
    switch (newboard.gameover) {
    | true => newboard
    | _ => whistle(newboard)
    };
  };
  whistle(sb);
};

let (vteam, hteam) = draw2Cities();
let resultboard = kickoff(vteam, hteam);