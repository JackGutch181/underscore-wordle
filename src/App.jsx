import { useState, useEffect, useCallback, useRef, useMemo } from "react";

// ─── Answer Pools ───
const FIVE_LETTER_ANSWERS = [
  "about","above","abuse","actor","acute","admit","adopt","adult","after","again",
  "agent","agree","ahead","alarm","album","alert","alien","align","alive","alley",
  "allow","alone","along","alter","ample","angel","anger","angle","angry","ankle",
  "apart","apple","apply","arena","argue","arise","armor","aroma","aside","asset",
  "atlas","attic","audio","audit","avoid","awake","award","aware","awful","bacon",
  "badge","badly","baker","basic","basin","basis","batch","beach","beard","beast",
  "began","begin","begun","being","belly","below","bench","berry","birth","black",
  "blade","blame","bland","blank","blast","blaze","bleak","bleed","blend","bless",
  "blind","block","blood","bloom","blown","board","boast","bonus","boost","booth",
  "bound","brain","brand","brave","bread","break","breed","brick","bride","brief",
  "bring","broad","broke","brook","brown","brush","buddy","build","bunch","burst",
  "buyer","cabin","cable","candy","cargo","carry","catch","cause","cease","chain",
  "chair","chalk","chaos","charm","chart","chase","cheap","check","cheek","cheer",
  "chess","chest","chief","child","chill","china","chunk","civic","claim","clash",
  "class","clean","clear","clerk","cliff","climb","cling","clock","clone","close",
  "cloth","cloud","coach","coast","color","comet","comic","coral","couch","could",
  "count","court","cover","crack","craft","crane","crash","crazy","cream","crime",
  "cross","crowd","crown","cruel","crush","curve","cycle","daily","dance","death",
  "debut","delay","demon","depth","derby","devil","diary","dirty","donor","doubt",
  "dough","draft","drain","drake","drama","drank","drawn","dream","dress","dried",
  "drift","drink","drive","drown","dying","eager","early","earth","eight","elder",
  "elect","elite","empty","enemy","enjoy","enter","equal","error","essay","event",
  "every","exact","exile","exist","extra","faint","fairy","faith","false","fancy",
  "fatal","fault","feast","fiber","field","fifth","fifty","fight","final","flame",
  "flash","flesh","float","flood","floor","fluid","flush","focal","focus","force",
  "forge","forth","forum","found","frame","frank","fraud","fresh","front","frost",
  "fruit","fully","ghost","giant","given","glass","globe","glory","glove","going",
  "grace","grade","grain","grand","grant","graph","grasp","grass","grave","great",
  "greed","green","greet","grief","grill","grind","gross","group","grove","grown",
  "guard","guess","guest","guide","guilt","habit","happy","harsh","haven","heart",
  "heavy","hence","honey","honor","horse","hotel","house","human","humor","ideal",
  "image","imply","index","inner","input","irony","issue","ivory","jewel","joint",
  "joker","judge","juice","knife","knock","known","label","labor","large","laser",
  "later","laugh","layer","learn","lease","legal","lemon","level","light","limit",
  "linen","liver","local","logic","loose","lover","lower","loyal","lucky","lunar",
  "lunch","lyric","magic","major","maker","manor","march","marry","match","maybe",
  "mayor","medal","media","mercy","merit","messy","metal","might","minor","minus",
  "mixed","model","money","month","moral","motor","mount","mouse","mouth","moved",
  "movie","music","naive","naked","nerve","never","newly","night","noble","noise",
  "north","noted","novel","nurse","occur","ocean","offer","often","olive","orbit",
  "order","organ","other","ought","outer","oxide","ozone","paint","panel","panic",
  "patch","peace","pearl","penny","phase","phone","photo","piano","piece","pilot",
  "pitch","pixel","pizza","place","plain","plane","plant","plate","plaza","plead",
  "plumb","point","poker","polar","pound","power","press","price","pride","prime",
  "print","prior","prize","prone","proof","proud","prove","proxy","pulse","punch",
  "pupil","queen","query","quest","queue","quick","quiet","quota","quote","radar",
  "radio","raise","rally","range","rapid","ratio","reach","react","realm","rebel",
  "reign","relax","repay","reply","rider","ridge","right","rigid","risky","rival",
  "river","robot","rocky","rouge","rough","round","route","royal","rugby","rural",
  "saint","salad","sauce","scale","scene","scope","score","sense","serve","setup",
  "seven","shade","shaft","shame","shape","share","shark","sharp","sheep","sheer",
  "sheet","shelf","shell","shift","shine","shirt","shock","shore","short","shout",
  "sight","sixth","sixty","skirt","skull","slave","sleep","slice","slide","slope",
  "smart","smell","smile","smoke","snake","solar","solid","solve","sorry","south",
  "space","spare","speak","speed","spend","spice","spine","spite","split","spoke",
  "sport","spray","squad","stack","staff","stage","stain","stake","stall","stamp",
  "stand","stark","start","state","steal","steam","steel","steep","steer","stick",
  "stiff","still","stock","stone","stood","store","storm","story","stove","strip",
  "stuck","study","stuff","style","sugar","suite","super","surge","swamp","swear",
  "sweet","swept","swift","swing","sword","sworn","swung","table","taste","teach",
  "teeth","tempo","thick","thing","think","third","those","three","threw","throw",
  "thumb","tight","timer","tired","title","today","token","total","touch","tough",
  "towel","tower","toxic","trace","track","trade","trail","train","trait","trash",
  "treat","trend","trial","tribe","trick","tried","troop","truck","truly","trunk",
  "trust","truth","tumor","tutor","twice","twist","ultra","uncle","under","unify",
  "union","unite","unity","until","upper","upset","urban","usage","usual","valid",
  "value","valve","verse","video","vigor","viral","virus","visit","vista","vital",
  "vivid","vocal","voice","voter","wages","waste","watch","water","weave","weigh",
  "weird","wheat","wheel","where","which","while","white","whole","whose","wider",
  "witch","woman","women","world","worry","worse","worst","worth","would","wound",
  "wrath","write","wrong","wrote","yacht","yield","young","youth","zones"
];

const SIX_LETTER_ANSWERS = [
  "absorb","accent","accept","access","across","acting","action","active","actual",
  "admire","advise","afford","agency","agenda","almost","always","amount","anchor",
  "animal","annual","answer","anyway","appeal","appear","arctic","around","artist",
  "assert","assign","assist","assume","attach","attack","attend","august","autumn",
  "backup","badges","ballet","bamboo","banana","banker","barely","barrel","basket",
  "battle","beacon","beauty","become","before","behalf","behave","behind","belief",
  "belong","beside","better","beyond","bigger","bishop","bitter","blanks","blazer",
  "blends","blocks","blonde","boards","boldly","borrow","bother","bottle","bottom",
  "bounce","bounty","branch","brands","breach","breath","breeds","bricks","bridge",
  "bright","brings","broken","broker","bronze","brutal","bubble","bucket","budget",
  "buffet","bundle","burden","bureau","buried","butter","bypass","cactus","campus",
  "cancel","candid","candle","cannot","canvas","canyon","carbon","career","carpet",
  "casino","castle","casual","caught","cement","center","chains","chairs","chance",
  "change","chapel","charge","charts","cheese","cherry","chosen","chrome","chunks",
  "church","circle","cities","claims","classy","clever","client","cliffs","climbs",
  "clocks","closed","closer","clouds","clumsy","clutch","cobalt","coding","coffee",
  "colony","colors","column","combat","comedy","comics","coming","commit","common",
  "comply","convey","cookie","copied","copper","corner","costly","cotton","county",
  "couple","coupon","course","cousin","covers","cracks","crafts","crazed","create",
  "credit","crisis","critic","crowds","cruise","custom","cycles","dagger","damage",
  "dancer","dances","danger","daring","dealer","deaths","debate","debris","decade",
  "decent","deeply","defeat","defend","degree","delays","delete","demand","denial",
  "denied","deploy","deputy","derive","desert","design","desire","detail","detect",
  "device","devote","dialog","differ","digest","dinner","direct","divide","divine",
  "domain","donate","double","dozens","dragon","drawer","dreams","drinks","driven",
  "driver","drives","during","duties","easily","eating","editor","effect","effort",
  "eighth","either","eleven","emerge","empire","employ","enable","ending","energy",
  "engage","engine","enough","ensure","entire","entity","equity","escape","estate",
  "ethnic","evolve","exceed","except","excess","excite","excuse","exempt","expand",
  "expect","expert","export","expose","extend","extent","fabric","facing","factor",
  "fairly","family","famous","farmer","father","faucet","fellow","female","fender",
  "fierce","figure","filter","finger","fiscal","flavor","flight","floral","flower",
  "flying","forced","forest","forget","formal","format","former","fossil","foster",
  "fourth","freeze","french","friend","fright","frozen","fusion","future","galaxy",
  "gamble","gaming","garage","garden","garlic","gather","gender","gentle","gifted",
  "global","govern","gravel","growth","guards","guilty","guitar","handed","handle",
  "happen","harbor","hardly","hazard","health","heaven","height","hereby","hidden",
  "holder","hollow","honest","horror","humble","hunger","hunter","hybrid","ignore",
  "immune","impact","import","impose","income","indeed","infant","inform","injury",
  "inline","insect","insert","inside","insist","insure","intact","intend","intent",
  "invest","invite","island","itself","jacket","jungle","junior","karate","kernel",
  "launch","lawyer","layout","leader","league","legacy","lender","length","lesson",
  "letter","lifted","lights","likely","limits","linear","linked","liquid","listen",
  "little","lively","living","locate","locked","lonely","lookup","lovely","luxury",
  "magnet","mainly","making","manage","manner","marble","margin","marine","marker",
  "market","master","matter","medium","member","memory","mental","mentor","merely",
  "merger","method","middle","mighty","mining","minute","mirror","mobile","modern",
  "modest","module","moment","mostly","motion","moving","murder","muscle","museum",
  "mutual","namely","narrow","nature","nearby","neatly","nicely","nobody","normal",
  "notice","notion","number","object","obtain","occupy","offend","office","online",
  "option","orange","origin","outfit","outlet","output","oxygen","palace","parade",
  "parent","parish","partly","patent","patrol","patron","pencil","people","pepper",
  "period","permit","person","phrase","pickup","pillar","planet","player","please",
  "pledge","plenty","pocket","poetry","poison","police","policy","polish","poorly",
  "portal","poster","potato","potent","powder","praise","prayer","prefer","prince",
  "prison","profit","prompt","proper","proven","public","punish","purple","pursue",
  "puzzle","rabbit","racial","radius","random","rather","rating","reader","really",
  "reason","reboot","recall","recent","record","reduce","reform","regard","regime",
  "region","reject","relate","relief","remain","remedy","remote","render","rental",
  "repair","repeat","report","rescue","resign","resist","resort","result","retail",
  "retain","retire","return","reveal","review","revise","revolt","reward","ribbon",
  "ritual","robust","rocket","roster","rotate","ruling","runway","sacred","safari",
  "safely","salary","salmon","sample","saving","school","screen","script","search",
  "season","second","secret","sector","secure","select","senate","senior","sequel",
  "series","server","settle","severe","shadow","shield","shiver","signal","silent",
  "silver","simple","simply","singer","single","sketch","slight","slowly","smooth",
  "soccer","social","solely","solemn","sought","source","speech","spirit","splash",
  "spread","spring","square","stable","stance","statue","status","steady","stolen",
  "strain","strand","streak","stream","street","stress","strict","strike","string",
  "stripe","stroke","strong","studio","submit","subtle","sudden","suffer","summer",
  "summit","supply","surely","survey","switch","symbol","tackle","talent","target",
  "temple","tenant","tender","tennis","terror","thanks","theory","thirty","thorny",
  "though","thread","thrill","thrive","throat","throne","threat","thrown","trends",
  "tribal","troops","trucks","timber","tissue","tongue","toward","travel","treaty",
  "tundra","tunnel","turtle","twelve","twenty","unfair","unique","unless","unlike",
  "unlock","unrest","update","uphold","upside","urgent","useful","vacuum","valley",
  "verbal","verify","versus","victim","viewer","violet","virgin","vision","visual",
  "volume","voters","wander","warmth","wealth","weapon","weekly","weight","wicked",
  "wildly","window","winner","winter","wisdom","within","wonder","wooden","worker",
  "worthy","writer","yellow","zombie"
];

const MAX_GUESSES = 17;
const NUM_BOARDS = 9;
const WORD_LENGTH = 6;
const TOTAL_ALPHABET = 27; // A-Z + space

function isValidGuess(guess) {
  if (guess.length !== WORD_LENGTH) return false;
  const isSpaceGuess = guess[0] === " ";
  const wordPart = isSpaceGuess ? guess.slice(1) : guess;
  return /^[A-Z]+$/.test(wordPart) && wordPart.length === (isSpaceGuess ? 5 : 6);
}

function pickAnswers() {
  const answers = [];
  const used = new Set();
  for (let i = 0; i < NUM_BOARDS - 1; i++) {
    const useFive = Math.random() < 0.35;
    const pool = useFive ? FIVE_LETTER_ANSWERS : SIX_LETTER_ANSWERS;
    let word;
    do { word = pool[Math.floor(Math.random() * pool.length)].toUpperCase(); } while (used.has(word));
    used.add(word);
    answers.push(useFive ? " " + word : word);
  }
  const decoyIndex = Math.floor(Math.random() * NUM_BOARDS);
  answers.splice(decoyIndex, 0, null);
  return { answers, decoyIndex };
}

function evaluateGuess(guess, answer) {
  const result = Array(WORD_LENGTH).fill("absent");
  const answerArr = answer.split("");
  const guessArr = guess.split("");
  const used = Array(WORD_LENGTH).fill(false);
  for (let i = 0; i < WORD_LENGTH; i++) {
    if (guessArr[i] === answerArr[i]) { result[i] = "correct"; used[i] = true; }
  }
  for (let i = 0; i < WORD_LENGTH; i++) {
    if (result[i] === "correct") continue;
    for (let j = 0; j < WORD_LENGTH; j++) {
      if (!used[j] && guessArr[i] === answerArr[j]) { result[i] = "present"; used[j] = true; break; }
    }
  }
  return result;
}

function mulberry32(a) {
  return function() {
    a |= 0; a = a + 0x6D2B79F5 | 0;
    var t = Math.imul(a ^ a >>> 15, 1 | a);
    t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
    return ((t ^ t >>> 14) >>> 0) / 4294967296;
  };
}

function generateDecoyFeedback(guess, guessIndex, seed) {
  const rng = mulberry32(seed + guessIndex * 7);
  const result = Array(WORD_LENGTH).fill("absent");
  const numYellows = Math.floor(rng() * 2) + 1;
  const numGreens = rng() > 0.7 ? 1 : 0;
  const positions = [0, 1, 2, 3, 4, 5];
  for (let i = positions.length - 1; i > 0; i--) {
    const j = Math.floor(rng() * (i + 1));
    [positions[i], positions[j]] = [positions[j], positions[i]];
  }
  let idx = 0;
  for (let g = 0; g < numGreens && idx < positions.length; g++, idx++) result[positions[idx]] = "correct";
  for (let y = 0; y < numYellows && idx < positions.length; y++, idx++) result[positions[idx]] = "present";
  return result;
}

// ─── Components ───

function Tile({ letter, state, delay }) {
  const stateClass = state === "correct" ? "tile-correct" :
    state === "present" ? "tile-present" :
    state === "absent" ? "tile-absent" :
    letter ? "tile-active" : "tile-empty";
  return (
    <div className={`tile ${stateClass}`} style={state ? { animationDelay: `${delay * 80}ms` } : {}}>
      {letter === " " ? "␣" : letter}
    </div>
  );
}

function MiniBoard({ boardIndex, guesses, evaluations, solved, active, currentGuess, isGameOver, isDecoy, gameEnded }) {
  const rows = [];
  const allGuesses = active && !isGameOver
    ? [...guesses, (currentGuess || "").padEnd(WORD_LENGTH, "")]
    : guesses;
  const allEvals = active && !isGameOver
    ? [...evaluations, null]
    : evaluations;
  for (let r = 0; r < allGuesses.length; r++) {
    const cells = [];
    for (let c = 0; c < WORD_LENGTH; c++) {
      cells.push(<Tile key={c} letter={allGuesses[r][c] || ""} state={allEvals[r] ? allEvals[r][c] : null} delay={c} />);
    }
    rows.push(<div key={r} className="tile-row">{cells}</div>);
  }
  const decoyRevealed = isDecoy && gameEnded;
  return (
    <div className={`mini-board ${solved ? "board-solved" : ""} ${decoyRevealed ? "board-decoy" : ""}`}>
      <div className="board-header">
        <span className="board-number">#{boardIndex + 1}</span>
        {solved && <span className="board-check">✓</span>}
        {decoyRevealed && <span className="board-decoy-label">DECOY</span>}
      </div>
      <div className="board-grid">{rows}</div>
    </div>
  );
}

function Keyboard({ keyStates, onKey, onEnter, onBackspace, onSpace }) {
  const rows = [
    ["Q","W","E","R","T","Y","U","I","O","P"],
    ["A","S","D","F","G","H","J","K","L"],
    ["ENTER","Z","X","C","V","B","N","M","⌫"],
  ];
  return (
    <div className="keyboard">
      <div className="keyboard-row">
        <button className="key key-space" onClick={onSpace}>␣ SPACE</button>
      </div>
      {rows.map((row, ri) => (
        <div key={ri} className="keyboard-row">
          {row.map(k => {
            const state = keyStates[k] || "";
            const cls = k === "ENTER" || k === "⌫" ? "key key-wide" : `key key-${state || "unused"}`;
            const handler = k === "ENTER" ? onEnter : k === "⌫" ? onBackspace : () => onKey(k);
            return <button key={k} className={cls} onClick={handler}>{k}</button>;
          })}
        </div>
      ))}
    </div>
  );
}

function StatsBar({ solvedCount, totalReal, guessCount, maxGuesses, alphabetPct }) {
  const solvedPct = totalReal > 0 ? Math.round((solvedCount / totalReal) * 100) : 0;
  return (
    <div className="stats-row">
      {/* Boards Solved */}
      <div className="stat-block">
        <div className="stat-header">
          <span className="stat-label">Boards</span>
          <span className="stat-numbers">
            <span className={`stat-big ${solvedCount > 0 ? "stat-green" : ""}`}>{solvedCount}</span>
            <span className="stat-dim">/{totalReal}</span>
            <span className="stat-pct">{solvedPct}%</span>
          </span>
        </div>
        <div className="stat-bar-track">
          <div className="stat-bar-fill stat-bar-green" style={{ width: `${solvedPct}%` }} />
        </div>
      </div>

      {/* Alphabet Coverage */}
      <div className="stat-block">
        <div className="stat-header">
          <span className="stat-label">Alphabet</span>
          <span className="stat-numbers">
            <span className="stat-big stat-purple">{alphabetPct}%</span>
          </span>
        </div>
        <div className="stat-bar-track">
          <div className="stat-bar-fill stat-bar-purple" style={{ width: `${alphabetPct}%` }} />
        </div>
      </div>

      {/* Guesses */}
      <div className="stat-block">
        <div className="stat-header">
          <span className="stat-label">Guesses</span>
          <span className="stat-numbers">
            <span className="stat-big">{guessCount}</span>
            <span className="stat-dim">/{maxGuesses}</span>
          </span>
        </div>
        <div className="stat-bar-track">
          <div
            className="stat-bar-fill stat-bar-amber"
            style={{ width: `${Math.round((guessCount / maxGuesses) * 100)}%` }}
          />
        </div>
      </div>
    </div>
  );
}

function RulesModal({ onClose }) {
  return (
    <div className="modal-overlay" onClick={onClose}>
      <div className="modal" onClick={e => e.stopPropagation()}>
        <h2>How to Play</h2>
        <p>Guess all the words in <strong>17 tries</strong>. Each guess fills all 9 boards simultaneously.</p>
        <div className="rules-section">
          <h3>⚡ The Space Twist</h3>
          <p>Some answers are <strong>5-letter words with a leading space</strong> in the first slot.
          Use the <span className="key-hint">␣ SPACE</span> button to start your guess with a space.</p>
        </div>
        <div className="rules-section">
          <h3>🎭 The Decoy</h3>
          <p><strong>One of the 9 boards is fake.</strong> It has no real answer and can never be solved.
          Its colors look plausible but will mislead you. Solve the 8 real boards to win.</p>
        </div>
        <div className="rules-section">
          <h3>Colors</h3>
          <div className="color-example">
            <div className="tile tile-correct mini-example">A</div>
            <span>Correct letter, correct spot</span>
          </div>
          <div className="color-example">
            <div className="tile tile-present mini-example">B</div>
            <span>Correct letter, wrong spot</span>
          </div>
          <div className="color-example">
            <div className="tile tile-absent mini-example">C</div>
            <span>Letter not in word</span>
          </div>
        </div>
        <button className="btn-play" onClick={onClose}>Play</button>
      </div>
    </div>
  );
}

// ─── Main App ───
export default function WordleNine() {
  const [gameData, setGameData] = useState(() => pickAnswers());
  const [guesses, setGuesses] = useState([]);
  const [currentGuess, setCurrentGuess] = useState("");
  const [message, setMessage] = useState("");
  const [showRules, setShowRules] = useState(true);
  const [shakeBoard, setShakeBoard] = useState(false);
  const [gameOver, setGameOver] = useState(false);
  const [focusedBoard, setFocusedBoard] = useState(null);
  const [decoySeed] = useState(() => Math.floor(Math.random() * 100000));
  const msgTimeout = useRef(null);

  const { answers, decoyIndex } = gameData;

  const solvedBoards = answers.map((answer, bi) => {
    if (bi === decoyIndex) return false;
    return guesses.some(g => g === answer);
  });

  const realSolvedCount = solvedBoards.filter((s, i) => i !== decoyIndex && s).length;
  const allRealSolved = realSolvedCount === NUM_BOARDS - 1;
  const outOfGuesses = guesses.length >= MAX_GUESSES;
  const isGameOver = gameOver || allRealSolved || outOfGuesses;

  // Alphabet coverage: unique characters guessed (A-Z + space) out of 27
  const alphabetCoverage = useMemo(() => {
    const usedChars = new Set();
    for (const guess of guesses) {
      for (const ch of guess) {
        usedChars.add(ch);
      }
    }
    return Math.round((usedChars.size / TOTAL_ALPHABET) * 100);
  }, [guesses]);

  const showMessage = useCallback((msg, duration = 1500) => {
    setMessage(msg);
    if (msgTimeout.current) clearTimeout(msgTimeout.current);
    msgTimeout.current = setTimeout(() => setMessage(""), duration);
  }, []);

  const getBoardEval = useCallback((guess, boardIndex, guessIndex) => {
    if (boardIndex === decoyIndex) return generateDecoyFeedback(guess, guessIndex, decoySeed);
    return evaluateGuess(guess, answers[boardIndex]);
  }, [answers, decoyIndex, decoySeed]);

  const submitGuess = useCallback(() => {
    if (isGameOver) return;
    if (currentGuess.length !== WORD_LENGTH) {
      showMessage("Not enough letters");
      setShakeBoard(true); setTimeout(() => setShakeBoard(false), 500);
      return;
    }
    if (!isValidGuess(currentGuess)) {
      showMessage("Invalid guess");
      setShakeBoard(true); setTimeout(() => setShakeBoard(false), 500);
      return;
    }
    const newGuesses = [...guesses, currentGuess];
    setGuesses(newGuesses);
    setCurrentGuess("");
    const newAllRealSolved = answers.every((answer, bi) => {
      if (bi === decoyIndex) return true;
      return newGuesses.some(g => g === answer);
    });
    if (newAllRealSolved) { showMessage("Incredible! 🎉", 5000); setGameOver(true); }
    else if (newGuesses.length >= MAX_GUESSES) { showMessage("Game over!", 5000); setGameOver(true); }
  }, [currentGuess, guesses, answers, decoyIndex, isGameOver, showMessage]);

  const addLetter = useCallback((l) => {
    if (isGameOver || currentGuess.length >= WORD_LENGTH) return;
    setCurrentGuess(prev => prev + l);
  }, [currentGuess, isGameOver]);

  const addSpace = useCallback(() => {
    if (isGameOver || currentGuess.length !== 0) return;
    setCurrentGuess(" ");
  }, [currentGuess, isGameOver]);

  const removeLetter = useCallback(() => {
    if (isGameOver) return;
    setCurrentGuess(prev => prev.slice(0, -1));
  }, [isGameOver]);

  useEffect(() => {
    const handler = (e) => {
      if (showRules) return;
      if (e.key === "Enter") { e.preventDefault(); submitGuess(); }
      else if (e.key === "Backspace") { e.preventDefault(); removeLetter(); }
      else if (e.key === " ") { e.preventDefault(); addSpace(); }
      else if (/^[a-zA-Z]$/.test(e.key)) addLetter(e.key.toUpperCase());
    };
    window.addEventListener("keydown", handler);
    return () => window.removeEventListener("keydown", handler);
  }, [showRules, submitGuess, removeLetter, addLetter, addSpace]);

  const keyStates = {};
  for (const [gi, guess] of guesses.entries()) {
    for (let ci = 0; ci < WORD_LENGTH; ci++) {
      const letter = guess[ci];
      if (letter === " ") continue;
      for (let bi = 0; bi < NUM_BOARDS; bi++) {
        if (bi === decoyIndex) continue;
        const state = evaluateGuess(guess, answers[bi])[ci];
        const current = keyStates[letter] || "unused";
        if (state === "correct") keyStates[letter] = "correct";
        else if (state === "present" && current !== "correct") keyStates[letter] = "present";
        else if (state === "absent" && current === "unused") keyStates[letter] = "absent";
      }
    }
  }

  const newGame = () => {
    setGameData(pickAnswers());
    setGuesses([]); setCurrentGuess(""); setMessage("");
    setGameOver(false); setFocusedBoard(null);
  };

  return (
    <div className="app">
      <style>{`
        @import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;600;700;800&family=Outfit:wght@400;500;600;700;800;900&display=swap');
        * { margin: 0; padding: 0; box-sizing: border-box; }
        :root {
          --bg: #0a0a0f; --bg2: #12121a; --bg3: #1a1a26;
          --text: #e8e6f0; --text-dim: #6b6880;
          --correct: #2d8a4e; --correct-bright: #3dba68;
          --present: #b59f3b; --present-bright: #d4bc3a;
          --absent: #3a3a4a; --border: #2a2a3a;
          --accent: #7c5cfc;
          --decoy: #c0392b; --decoy-bright: #e74c3c;
        }
        .app {
          min-height: 100vh; background: var(--bg); color: var(--text);
          font-family: 'Outfit', sans-serif;
          display: flex; flex-direction: column; align-items: center;
          overflow-x: hidden;
        }

        /* ── Header ── */
        .header {
          width: 100%; padding: 10px 20px;
          display: flex; align-items: center; justify-content: space-between;
          border-bottom: 1px solid var(--border); background: var(--bg2);
        }
        .header h1 {
          font-family: 'JetBrains Mono', monospace;
          font-size: 24px; font-weight: 800; letter-spacing: -0.5px;
          white-space: nowrap;
        }
        .underscore { color: var(--accent); }
        .wordle-text {
          background: linear-gradient(135deg, #e8e6f0, #a8a4c0);
          -webkit-background-clip: text; -webkit-text-fill-color: transparent;
        }
        .header-right { display: flex; gap: 10px; align-items: center; }
        .btn-icon {
          background: var(--bg3); border: 1px solid var(--border); color: var(--text);
          width: 34px; height: 34px; border-radius: 8px; cursor: pointer; font-size: 15px;
          display: flex; align-items: center; justify-content: center; transition: all 0.15s;
        }
        .btn-icon:hover { border-color: var(--accent); background: rgba(124,92,252,0.1); }

        /* ── Stats Row (below header) ── */
        .stats-row {
          width: 100%; max-width: 920px;
          display: grid; grid-template-columns: 1fr 1fr 1fr;
          gap: 12px; padding: 10px 16px;
          background: var(--bg);
          border-bottom: 1px solid var(--border);
        }
        .stat-block { display: flex; flex-direction: column; gap: 4px; }
        .stat-header {
          display: flex; justify-content: space-between; align-items: baseline;
        }
        .stat-label {
          font-family: 'JetBrains Mono', monospace;
          font-size: 10px; text-transform: uppercase;
          letter-spacing: 1px; color: var(--text-dim); font-weight: 600;
        }
        .stat-numbers { display: flex; align-items: baseline; gap: 2px; }
        .stat-big {
          font-family: 'JetBrains Mono', monospace;
          font-weight: 800; font-size: 16px; color: var(--text);
        }
        .stat-dim {
          font-family: 'JetBrains Mono', monospace;
          font-size: 12px; color: var(--text-dim);
        }
        .stat-pct {
          font-family: 'JetBrains Mono', monospace;
          font-size: 11px; color: var(--text-dim);
          margin-left: 4px;
        }
        .stat-green { color: var(--correct-bright); }
        .stat-purple { color: var(--accent); }
        .stat-bar-track {
          height: 6px; border-radius: 3px;
          background: var(--bg3); overflow: hidden;
        }
        .stat-bar-fill {
          height: 100%; border-radius: 3px;
          transition: width 0.4s ease;
        }
        .stat-bar-green { background: linear-gradient(90deg, var(--correct), var(--correct-bright)); }
        .stat-bar-purple { background: linear-gradient(90deg, #5a3fd4, var(--accent)); }
        .stat-bar-amber { background: linear-gradient(90deg, #b59f3b, var(--present-bright)); }

        /* ── Message ── */
        .message-container { height: 32px; display: flex; align-items: center; justify-content: center; }
        .message {
          background: var(--text); color: var(--bg); padding: 5px 14px; border-radius: 6px;
          font-weight: 600; font-size: 13px; animation: fadeIn 0.15s ease;
        }

        /* ── Boards ── */
        .boards-container { flex: 1; width: 100%; max-width: 920px; padding: 6px 12px; overflow-y: auto; }
        .boards-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; width: 100%; }
        .mini-board {
          background: var(--bg2); border: 1px solid var(--border); border-radius: 10px;
          padding: 8px; transition: all 0.2s; cursor: pointer;
        }
        .mini-board:hover { border-color: var(--accent); }
        .mini-board.board-solved { border-color: var(--correct); box-shadow: 0 0 12px rgba(45,138,78,0.2); }
        .mini-board.board-decoy {
          border-color: var(--decoy); box-shadow: 0 0 16px rgba(192,57,43,0.3);
          animation: decoyReveal 0.6s ease;
        }
        @keyframes decoyReveal { 0%{transform:scale(1)} 30%{transform:scale(1.03)} 60%{transform:scale(0.98)} 100%{transform:scale(1)} }
        .board-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px; padding: 0 2px; }
        .board-number { font-family: 'JetBrains Mono', monospace; font-size: 11px; color: var(--text-dim); font-weight: 600; }
        .board-check { color: var(--correct-bright); font-size: 14px; }
        .board-decoy-label {
          font-family: 'JetBrains Mono', monospace; font-size: 10px; font-weight: 800;
          color: var(--decoy-bright); background: rgba(231,76,60,0.15);
          padding: 2px 6px; border-radius: 4px; letter-spacing: 1px;
        }
        .board-grid { display: flex; flex-direction: column; gap: 3px; }
        .tile-row { display: flex; gap: 3px; }

        .tile {
          width: 100%; aspect-ratio: 1;
          display: flex; align-items: center; justify-content: center;
          font-family: 'JetBrains Mono', monospace;
          font-weight: 700; font-size: clamp(10px, 2vw, 14px);
          border-radius: 4px; text-transform: uppercase; transition: all 0.1s;
        }
        .tile-empty { background: var(--bg3); border: 1px solid var(--border); }
        .tile-active { background: var(--bg3); border: 1.5px solid var(--text-dim); }
        .tile-correct { background: var(--correct); border: 1px solid var(--correct-bright); color: white; animation: flipIn 0.3s ease; }
        .tile-present { background: var(--present); border: 1px solid var(--present-bright); color: white; animation: flipIn 0.3s ease; }
        .tile-absent { background: var(--absent); border: 1px solid transparent; color: var(--text-dim); animation: flipIn 0.3s ease; }

        @keyframes flipIn { 0%{transform:scaleY(0)} 50%{transform:scaleY(0)} 100%{transform:scaleY(1)} }
        @keyframes fadeIn { from{opacity:0;transform:translateY(-5px)} to{opacity:1;transform:translateY(0)} }
        @keyframes shake { 0%,100%{transform:translateX(0)} 20%{transform:translateX(-4px)} 40%{transform:translateX(4px)} 60%{transform:translateX(-4px)} 80%{transform:translateX(4px)} }
        .shake { animation: shake 0.4s ease; }

        /* ── Keyboard ── */
        .keyboard { width: 100%; max-width: 660px; padding: 10px 8px 18px; }
        .keyboard-row { display: flex; gap: 6px; justify-content: center; margin-bottom: 6px; }
        .key {
          height: 62px; min-width: 42px; border: none; border-radius: 8px;
          font-family: 'JetBrains Mono', monospace; font-weight: 700; font-size: 18px;
          cursor: pointer; transition: all 0.1s; flex: 1; max-width: 56px;
          user-select: none; -webkit-tap-highlight-color: transparent;
        }
        .key:active { transform: scale(0.93); }
        .key-wide { max-width: 92px; font-size: 13px; letter-spacing: 0.5px; }
        .key-space {
          max-width: 240px; flex: 1; font-size: 15px; font-weight: 600;
          background: var(--bg3); border: 1.5px dashed var(--text-dim);
          color: var(--text-dim); height: 50px; letter-spacing: 1px;
        }
        .key-space:hover { border-color: var(--accent); color: var(--accent); }
        .key-space:active { transform: scale(0.96); }
        .key-unused { background: #2d2d3d; color: var(--text); border: 1px solid var(--border); }
        .key-unused:hover { background: #383850; }
        .key-correct { background: var(--correct); color: white; }
        .key-correct:hover { filter: brightness(1.1); }
        .key-present { background: var(--present); color: white; }
        .key-present:hover { filter: brightness(1.1); }
        .key-absent { background: #1e1e28; color: #4a4860; border: 1px solid #252535; }

        /* ── Game Over ── */
        .game-over-bar {
          width: 100%; max-width: 660px; background: var(--bg2); border: 1px solid var(--border);
          border-radius: 10px; padding: 16px; margin: 8px 12px;
          display: flex; align-items: center; justify-content: space-between; gap: 12px;
        }
        .game-over-text { font-weight: 700; font-size: 16px; }
        .game-over-sub { font-size: 13px; color: var(--text-dim); margin-top: 2px; }
        .btn-new {
          background: var(--accent); color: white; border: none;
          padding: 10px 20px; border-radius: 8px; font-family: 'Outfit', sans-serif;
          font-weight: 700; font-size: 14px; cursor: pointer;
          white-space: nowrap; transition: all 0.15s;
        }
        .btn-new:hover { filter: brightness(1.15); transform: scale(1.02); }

        .answers-grid {
          display: grid; grid-template-columns: repeat(3, 1fr);
          gap: 6px; margin-top: 10px;
          font-family: 'JetBrains Mono', monospace; font-size: 12px;
        }
        .answer-item { background: var(--bg3); padding: 5px 8px; border-radius: 4px; text-align: center; text-transform: uppercase; }
        .answer-item.was-solved { color: var(--correct-bright); }
        .answer-item.was-missed { color: #e05555; }
        .answer-item.was-decoy { color: var(--decoy-bright); background: rgba(231,76,60,0.1); border: 1px solid rgba(231,76,60,0.3); }

        .focused-overlay {
          position: fixed; inset: 0; background: rgba(0,0,0,0.85);
          display: flex; align-items: center; justify-content: center; z-index: 50; cursor: pointer;
        }
        .focused-board-large {
          background: var(--bg2); border: 1px solid var(--border);
          border-radius: 14px; padding: 16px; min-width: 320px; max-width: 380px;
        }
        .focused-board-large .tile { font-size: 20px; height: 42px; aspect-ratio: auto; }
        .focused-board-large .tile-row { gap: 5px; }
        .focused-board-large .board-grid { gap: 5px; }
        .focused-hint { font-size: 12px; color: var(--text-dim); text-align: center; margin-top: 10px; }

        .modal-overlay {
          position: fixed; inset: 0; background: rgba(0,0,0,0.85);
          display: flex; align-items: center; justify-content: center; z-index: 100;
        }
        .modal {
          background: var(--bg2); border: 1px solid var(--border);
          border-radius: 14px; padding: 28px; max-width: 420px; width: 90%;
        }
        .modal h2 { font-size: 24px; font-weight: 800; margin-bottom: 12px; }
        .modal p { font-size: 14px; color: var(--text-dim); line-height: 1.6; margin-bottom: 10px; }
        .modal strong { color: var(--text); }
        .rules-section { margin: 16px 0; }
        .rules-section h3 { font-size: 14px; font-weight: 700; margin-bottom: 8px; color: var(--accent); }
        .color-example { display: flex; align-items: center; gap: 10px; margin: 6px 0; font-size: 13px; color: var(--text-dim); }
        .mini-example { width: 34px !important; height: 34px !important; aspect-ratio: auto !important; font-size: 15px !important; flex-shrink: 0; }
        .key-hint {
          background: var(--bg3); padding: 2px 8px; border-radius: 4px;
          font-family: 'JetBrains Mono', monospace; font-size: 12px; border: 1px dashed var(--text-dim);
        }
        .btn-play {
          width: 100%; padding: 14px; background: var(--accent); color: white; border: none;
          border-radius: 8px; font-family: 'Outfit', sans-serif;
          font-weight: 700; font-size: 16px; cursor: pointer; margin-top: 18px; transition: all 0.15s;
        }
        .btn-play:hover { filter: brightness(1.15); }

        @media (max-width: 600px) {
          .boards-grid { gap: 6px; }
          .mini-board { padding: 5px; }
          .tile { font-size: 9px; }
          .key { height: 52px; font-size: 15px; min-width: 32px; }
          .key-wide { font-size: 11px; }
          .keyboard { max-width: 100%; padding: 8px 4px 14px; }
          .stats-row { gap: 8px; padding: 8px 12px; }
          .stat-big { font-size: 14px; }
          .header { padding: 8px 12px; }
        }
      `}</style>

      {/* Header */}
      <div className="header">
        <h1><span className="underscore">_</span><span className="wordle-text">Wordle</span></h1>
        <div className="header-right">
          <button className="btn-icon" onClick={() => setShowRules(true)}>?</button>
          <button className="btn-icon" onClick={newGame}>↻</button>
        </div>
      </div>

      {/* Stats Bar */}
      <StatsBar
        solvedCount={realSolvedCount}
        totalReal={NUM_BOARDS - 1}
        guessCount={guesses.length}
        maxGuesses={MAX_GUESSES}
        alphabetPct={alphabetCoverage}
      />

      <div className="message-container">
        {message && <div className="message">{message}</div>}
      </div>

      <div className="boards-container">
        <div className={`boards-grid ${shakeBoard ? "shake" : ""}`}>
          {answers.map((answer, bi) => {
            const isDecoy = bi === decoyIndex;
            const boardGuesses = (!isDecoy && solvedBoards[bi])
              ? guesses.slice(0, guesses.findIndex(g => g === answer) + 1)
              : guesses;
            const boardEvals = boardGuesses.map((g, gi) => getBoardEval(g, bi, gi));
            return (
              <div key={bi} onClick={() => setFocusedBoard(bi)}>
                <MiniBoard
                  boardIndex={bi} guesses={boardGuesses} evaluations={boardEvals}
                  solved={solvedBoards[bi]} active={!solvedBoards[bi]}
                  currentGuess={currentGuess} isGameOver={isGameOver}
                  isDecoy={isDecoy} gameEnded={isGameOver}
                />
              </div>
            );
          })}
        </div>
        {isGameOver && (
          <div style={{ maxWidth: 660, margin: "12px auto 0" }}>
            <div className="answers-grid">
              {answers.map((a, i) => {
                if (i === decoyIndex) return <div key={i} className="answer-item was-decoy">🎭 DECOY</div>;
                const display = a[0] === " " ? `␣${a.trim()}` : a;
                return <div key={i} className={`answer-item ${solvedBoards[i] ? "was-solved" : "was-missed"}`}>{display}</div>;
              })}
            </div>
          </div>
        )}
      </div>

      {isGameOver && (
        <div className="game-over-bar">
          <div>
            <div className="game-over-text">{allRealSolved ? "🎉 Incredible!" : `${realSolvedCount}/${NUM_BOARDS - 1} solved`}</div>
            <div className="game-over-sub">
              {allRealSolved
                ? `Solved in ${guesses.length} guesses — Board #${decoyIndex + 1} was the decoy`
                : `Board #${decoyIndex + 1} was the decoy`}
            </div>
          </div>
          <button className="btn-new" onClick={newGame}>New Game</button>
        </div>
      )}

      {!isGameOver && (
        <Keyboard keyStates={keyStates} onKey={addLetter} onEnter={submitGuess} onBackspace={removeLetter} onSpace={addSpace} />
      )}

      {showRules && <RulesModal onClose={() => setShowRules(false)} />}

      {focusedBoard !== null && (
        <div className="focused-overlay" onClick={() => setFocusedBoard(null)}>
          <div className="focused-board-large" onClick={e => e.stopPropagation()}>
            {(() => {
              const bi = focusedBoard;
              const isDecoy = bi === decoyIndex;
              const boardGuesses = (!isDecoy && solvedBoards[bi])
                ? guesses.slice(0, guesses.findIndex(g => g === answers[bi]) + 1)
                : guesses;
              const boardEvals = boardGuesses.map((g, gi) => getBoardEval(g, bi, gi));
              return (
                <MiniBoard boardIndex={bi} guesses={boardGuesses} evaluations={boardEvals}
                  solved={solvedBoards[bi]} active={!solvedBoards[bi]}
                  currentGuess={currentGuess} isGameOver={isGameOver}
                  isDecoy={isDecoy} gameEnded={isGameOver} />
              );
            })()}
            <div className="focused-hint">Click outside to close</div>
          </div>
        </div>
      )}
    </div>
  );
}
