/*
プログラムの実行手順：
1. ターミナル / コマンドプロンプトを開く
2. build.sbt が置かれた場所で sbt と入力し、return を押す
3. project othello と入力し、return を押す
4. run と入力し、return を押す
5. プログラムを変更後、もう一度実行したいときは run と入力し、return を押す
*/

package othello

import scala.math.max
import scala.math.min
import scala.math.random

object OthelloLib {

  // マス目
  trait Square

  // 空のマス目
  case object Empty extends Square

  // プレイヤー
  trait Player extends Square

  // 黒・白のプレイヤー
  case object Black extends Player
  case object White extends Player

  // xy 座標の点
  type Position = (Int, Int)

  // ゲームの盤面
  type Board = List[List[Square]]

  // 盤面の初期値
  val initBoard: Board =
    List(List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         List(Empty, Empty, Empty, White, Black, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Black, White, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         List(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty))

  // マス目の座標のリスト
  val posList: List[Position] =
    List((1, 1), (2, 1), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1), (8, 1),
      (1, 2), (2, 2), (3, 2), (4, 2), (5, 2), (6, 2), (7, 2), (8, 2),
      (1, 3), (2, 3), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3),
      (1, 4), (2, 4), (3, 4), (4, 4), (5, 4), (6, 4), (7, 4), (8, 4),
      (1, 5), (2, 5), (3, 5), (4, 5), (5, 5), (6, 5), (7, 5), (8, 5),
      (1, 6), (2, 6), (3, 6), (4, 6), (5, 6), (6, 6), (7, 6), (8, 6),
      (1, 7), (2, 7), (3, 7), (4, 7), (5, 7), (6, 7), (7, 7), (8, 7),
      (1, 8), (2, 8), (3, 8), (4, 8), (5, 8), (6, 8), (7, 8), (8, 8))

  // ゲームの状態
  type Game = (Board, Player)

  // ゲームの初期値
  val newGame = (initBoard, Black)

  // board の pos にあるマス目の情報を返す
  def boardRef(board: Board, pos: Position): Square = {
    val (x, y) = pos
    board(y - 1)(x - 1)
  }

  // board の pos に player が石を置いたときに、相手の石を取り囲むかを判定する
  def outflanks(board: Board, player: Player, pos: Position): Boolean = {
    val (x, y) = pos
    (boardRef(board, pos) == Empty) && (!(flippedPositions(board, player, pos) == Nil))
  }

  // player の敵を返す
  def opponent(player: Player): Player = {
    if (player == Black) White else Black
  }

  // board 上の player の石の数を返す
  def countPieces(game: Game): Int = {
    val (board, player) = game
    board.foldRight(0)((row, r) => row.foldRight(0)((s, r) => if (s == player) r + 1 else r) + r)
  }

  // マス目の中身を文字列に変換する
  def squareToString(s: Square): String = {
    s match {
      case Empty => "."
      case Black => "x"
      case White => "o"
    }
  }

  // 盤面の描画
  def drawBoard(board: Board): Unit = {

    def drawRow(row: List[Square]): Unit = {
      row match {
        case Nil     => printf("\n")
        case s :: ss => printf("%s ", squareToString(s)); drawRow(ss)
      }
    }

    board match {
      case Nil     => printf("\n"); printf("---------------\n")
      case r :: rs => drawRow(r); drawBoard(rs)
    }
  }

  // ゲームオーバかどうかを判定する
  def gameOver(game: Game): Boolean = {
    val (board, player) = game
    !(posList.foldRight(false)((p, b) => b || outflanks(board, player, p)) ||
      posList.foldRight(false)((p, b) => b || outflanks(board, opponent(player), p)))
  }

  // board の pos に player が石を置いたときに、色が反転するマス目の座標を返す
  def flippedPositions(board: Board, player: Player, pos: Position): List[Position] = {

    // Position の足し算
    def posPlus(pos: Position, x: Int, y: Int): Position = {
      val (x0, y0) = pos
      (x0 + x, y0 + y)
    }

    // pos から (x, y) 方向に向かって反転する点を探す
    def trim(pos: Position, x: Int, y: Int): List[Position] = {

      def trimAux(pos: Position, x: Int, y: Int, list: List[Position]): List[Position] = {
        val nextPos = posPlus(pos, x, y)
        nextPos match {
          case (nextx, nexty) =>
            if ((nextx < 1) || (nextx > 8) || (nexty < 1) || (nexty > 8)) Nil
            else if (boardRef(board, nextPos) == Empty) Nil
            else if (boardRef(board, nextPos) == player) list
            else trimAux(nextPos, x, y, nextPos :: list)
        }
      }

      trimAux(pos, x, y, Nil)
    }

    trim(pos, 0, 1) ++ trim(pos, 1, 1) ++ trim(pos, 1, 0) ++ trim(pos, 1, -1) ++
    trim(pos, 0, -1) ++ trim(pos, -1, -1) ++ trim(pos, -1, 0) ++ trim(pos, -1, 1)
  }

  // player が石を置ける board 上の座標のリストを返す
  def validMoves(board: Board, player: Player): List[Position] = {
    posList.filter(outflanks(board, player, _)).filter(boardRef(board, _) == Empty)
  }

  // board の pos に player が石を置いた結果、得られる状態を返す
  def applyMove(board: Board, player: Player, pos: Position): Game = {

    // 行数の分だけ makeRow を行い、その結果を使って盤面を作る
    def makeBoard(board: Board, flipList: List[Position], y: Int): Board = {
      board match {
        case Nil => Nil
        case r :: rs =>
          // y 行目の座標のうち、色が反転するもののリスト
          val flipListY = flipList.filter(p => p match { case (px, py) => py == y })
          makeRow(r, flipListY, 1, y) :: makeBoard(rs, flipList, y + 1)
      }
    }

    // 反転後の行を作る
    def makeRow(row: List[Square], flipListY: List[Position], x: Int, y: Int): List[Square] = {
        row match {
          case Nil => Nil
          case square :: squares => {
            val newSquare =
              // 反転リストに入っている座標は player
              if (contain(x, flipListY)) player
              // player が石を置く場所は player
              else if (pos == (x, y)) player
              // それ以外はそのまま
              else square
            newSquare :: makeRow(squares, flipListY, x + 1, y)
          }
        }
    }

    // (x, y) が flipListY に含まれるかを判定
    def contain(x: Int, flipListY: List[Position]): Boolean = {
      flipListY match {
        case Nil => false
        case (px, py) :: ps => if (px == x) true else contain(x, ps)
      }
    }

    if (!(outflanks(board, player, pos))) {
      throw new Exception("not a valid move")
    }
    else {
      // 反転する座標のリスト
      val flipList = flippedPositions(board, player, pos)
      // 反転後の盤面
      val newboard = makeBoard(board, flipList, 1)
      // 次のプレイヤー
      val nextplayer = opponent(player)
      (newboard, nextplayer)
    }
  }

  // 戦略
  type Strategy = Game => Position

  // ランダムに手を選ぶ
  def randomMove: Strategy = {

    def nth[A](list: List[A], n: Int): A = {
      (list, n) match {
        case (a :: as, 0) => a
        case (a :: as, n) => nth(as, n - 1)
      }
    }

    game =>
      val (board, player) = game
      val positions = validMoves(board, player)
      nth(positions, (random() * positions.length).toInt)
    }

  // ヒューリスティック
  type Heuristic = Game => Int

  // 黒 - 白 の値を返す
  def countDiff: Heuristic = {
    game => countPieces(game._1, Black) - countPieces(game._1, White)
  }

  // 戦略の適用
  def applyStrategy(game: Game, strategy: Strategy): Game = {
    val (board, player) = game
    val nextPlayer = opponent(player)
    if (!(posList.foldRight(false)((p, b) => b || outflanks(board, player, p)))) {
       printf("skip!\n");
       (board, nextPlayer)
    }
    else {
      val pos = strategy(game)
      if (!(outflanks(board, player, pos))) {
        throw new Exception(f"invalid move $pos")
      }
      else {
        applyMove(board, player, pos)
      }
    }
  }

  // ゲームの開始
  def playLoop(game: Game, strat1: Strategy, strat2: Strategy): Game = {
    val (board, player) = game
    if (gameOver(game)) {
      val blackscore = countPieces(board, Black)
      val whitescore = countPieces(board, White)
      val winner =
        if (blackscore > whitescore) "Black"
        else if (whitescore > blackscore) "White"
        else "None"
      drawBoard(board);
      printf("Black: %d, White: %d, Winner: %s\n", blackscore, whitescore, winner);
      sys.exit()
    }
    else {
      drawBoard(board);
      val newgame = applyStrategy(game, strat1)
      playLoop(newgame, strat2, strat1)
    }
  }

  //////////////////
  // オリジナルの戦略 //
  //////////////////

  // 空白の数を返す
  // 手数の確認として使える
  def countEmpty(game: Game): Int = {
    game._1.flatten.count(s => s==Empty)
  }

  // 序盤の定石を返す
  def initialPhase(turn: Int, game: Game): Position = {
    def opening1(board: Board): Position = {
      //白の番
      //縦取りを返す
      if(board(4)(5) == Black) (4, 6) //(6, 5)に打たれていたとき
      else if(board(5)(4) == Black) (6, 4) //転置
      else if(board(3)(2) == Black) (5, 3) //反転
      else if(board(2)(3) == Black) (3, 5) //反転かつ転置
      else throw new Exception() //到達しないはず
    }
    def opening2(board: Board): Position = {
      //黒の番
      //assert(board(4)(5) == Black) //初手は(6, 5)に打っていたとする
      //if(board(5)(3) == White) (3, 3) //縦取りには虎定石
      if(board(5)(3) == White) (3, 5) //縦取りには兎定石
      else if(board(5)(5) == White) (5, 6) //斜め取りには牛定石
      else if(board(3)(5) == White) (5, 3) //並び取りには鼠定石
      else throw new Exception() //到達しないはず
    }

    val next = applyMove(initBoard, Black, (6, 5))._1
    val vertical = applyMove(next, White, (4, 6))._1
    val rabbit = applyMove(vertical, Black, (3, 5))._1
    val tiger = applyMove(vertical, Black, (3, 3))._1
    def opening3(board: Board): Position = {
      //白の番
      //縦取りしていたとする
      //初手の黒の対称性も考えながら
      //兎定石と虎定石に対応する
      val rabbitT = rabbit.transpose
      val rabbitR = rabbit.map(_.reverse).reverse
      val rabbitRT = rabbitR.transpose
      val tigerT = tiger.transpose
      val tigerR = tiger.map(_.reverse).reverse
      val tigerRT = tigerR.transpose
      board match {
        case `rabbit` => (6, 4)
        case `rabbitT` => (4, 6)
        case `rabbitR` => (3, 5)
        case `rabbitRT` => (5, 3)
        case `tiger` => (4, 3)
        case `tigerT` => (3, 4)
        case `tigerR` => (5, 6)
        case `tigerRT` => (6, 5)
        case _ => throw new Exception() //定石を外れた場合
      }
    }

    val diagonal = applyMove(next, White, (6, 6))._1
    val ox = applyMove(diagonal, Black, (5, 6))._1
    def opening4(board: Board): Position = {
      //黒の番
      //初手は(6, 5)に打っていたとする
      //兎, 牛, 虎に対応する
      //牛定石のみ相手に対称な返し方がある
      val rabbit3 = applyMove(rabbit, White, (6, 4))._1
      val ox3 = applyMove(ox, White, (6, 4))._1
      val ox3T = ox3.transpose
      val tiger3 = applyMove(tiger, White, (4, 3))._1
      board match {
        case `rabbit3` => (5, 3)
        case `ox3` => (5, 3)
        case `ox3T` => (3, 5)
        case `tiger3` => (3, 4)
        case _ => throw new Exception() //定石を外れた場合
      }
    }

    val (board, player) = game
    turn match {
      case 0 => {
        assert(player == Black)
        //対称なので全ての手が同じ
        (6, 5) //ここに打つのが基本らしい
      }
      case 1 => {
        assert(player == White)
        opening1(board)
      }
      case 2 => {
        assert(player == Black)
        opening2(board)
      }
      case 3 => {
        assert(player == White)
        opening3(board)
      }
      case 4 => {
        assert(player == Black)
        opening4(board)
      }
      case _ => throw new Exception() //到達しないはず
    }
  }

  //静的なマスの重み
  //参考にした https://uguisu.skr.jp/othello/5-1.html
  //負の値が多いことでとりすぎないようになっている
  val staticWeight = List(
     30,-12,  0, -1, -1,  0,-12, 30,
    -12,-15, -3, -3, -3, -3,-15,-12,
      0, -3,  0, -1, -1,  0, -3,  0,
     -1, -3, -1, -1, -1, -1, -3, -1,
     -1, -3, -1, -1, -1, -1, -3, -1,
      0, -3,  0, -1, -1,  0, -3,  0,
    -12,-15, -3, -3, -3, -3,-15,-12,
     30,-12,  0, -1, -1,  0,-12, 30
  )

  // 静的なマスの重みから評価値を求める
  def staticValue(board: Board): Int = {
    board.flatten.zip(staticWeight).map(
      sw => sw._1 match {
        case Black => sw._2
        case Empty => 0
        case White => -sw._2
      }
    ).reduce(_ + _)
  }

  //(角の周りの)確定石の数を大雑把に計算して評価値を求める
  def cornerValue(board: Board): Int = {
    def cornerValueSub(x: Boolean, side1: List[Boolean], side2: List[Boolean]): Int = {
      //角corner,斜め内側のマスx,2つの辺sideを受け取り確定石の数を返す
      var sum = 1 //角は確定
      if(x && side1.head && side2.head && (side1.tail.head || side2.tail.head)) sum += 1
      //角と隣両方と隣どちらかを確保すれば斜め内側が確定
      var flag = true //連続しているかを保存する
      for(b <- side1){
        if(flag)
          if(b)
            sum += 1
          else
            flag = false
      } //角から連続する辺上の石は確定
      if(flag) sum -= 4
      //もし辺上を全てとっていたら2重に数えることになるので減らす
      flag = true
      for(b <- side2){
        if(flag)
          if(b)
            sum += 1
          else
            flag = false
      }
      if(flag) sum -= 4
      sum
    }

    val sideU = board.head //上の辺 (左から右)
    val sideL = board.map(_.head) //左の辺 (上から下)
    val sideR = board.map(_.last) //右の辺 (上から下)
    val sideD = board.last //下の辺 (左から右)

    val blackNum = //黒の確定石の数
      (if(board(0)(0)==Black)
        cornerValueSub(board(1)(1)==Black,
        sideU.tail.map(_==Black), sideL.tail.map(_==Black))
      else 0) +
      (if(board(0)(7)==Black)
        cornerValueSub(board(1)(6)==Black,
        sideU.reverse.tail.map(_==Black), sideR.tail.map(_==Black))
      else 0) +
      (if(board(7)(0)==Black)
        cornerValueSub(board(6)(1)==Black,
        sideD.tail.map(_==Black), sideL.reverse.tail.map(_==Black))
      else 0) +
      (if(board(7)(7)==Black)
        cornerValueSub(board(6)(6)==Black,
        sideD.reverse.tail.map(_==Black), sideR.reverse.tail.map(_==Black))
      else 0)

    val whiteNum =
      (if(board(0)(0)==White)
        cornerValueSub(board(1)(1)==White,
        sideU.tail.map(_==White), sideL.tail.map(_==White))
      else 0) +
      (if(board(0)(7)==White)
        cornerValueSub(board(1)(6)==White,
        sideU.reverse.tail.map(_==White), sideR.tail.map(_==White))
      else 0) +
      (if(board(7)(0)==White)
        cornerValueSub(board(6)(1)==White,
        sideD.tail.map(_==White), sideL.reverse.tail.map(_==White))
      else 0) +
      (if(board(7)(7)==White)
        cornerValueSub(board(6)(6)==White,
        sideD.reverse.tail.map(_==White), sideR.reverse.tail.map(_==White))
      else 0)

    blackNum - whiteNum
  }

  // 評価関数
  def calcValue: Heuristic = {
    game =>
      val (board, player) = game
      staticValue(board) + 
        (if(board(0)(0)==Empty && board(0)(7)==Empty && board(7)(0)==Empty && board(7)(7)==Empty)
          0 else 10*cornerValue(board))
      // 静的評価 + (もし角をとっていたら)角の周りの評価
  }

  //勝敗に応じてほぼ最大,ほぼ最小の値を返す
  //alphabetaは(Int.MaxValue, Int.MinValue)の中で探すため1つ内側にする
  def winLose: Heuristic = {
    game =>
      val diff = countDiff(game) 
      if(diff > 0) Int.MaxValue-1
      else if(diff < 0) Int.MinValue+1
      else 0
  }

  //posListに対して
  //最初から石が置いてある中央の場所を外し
  //静的評価が高い順に並び替えておく
  val posListSorted = posList
    .zip(staticWeight).zip(initBoard.flatten)
    .collect{case (posWeight, Empty) => posWeight}
    .sortWith(_._2 > _._2).map(_._1)

  //println(posListSorted)

  // player が石を置ける board 上の座標のリストを返す
  // 改良した
  // Emptyの判定はoutflanks内で行われるため不要
  // あらかじめ静的な評価の高いマスを前に置いている
  def myValidMoves(board: Board, player: Player): List[Position] = {
    //posList.filter(outflanks(board, player, _)).filter(boardRef(board, _) == Empty)
    posListSorted.filter(outflanks(board, player, _))
  }

  // 中盤の評価値をalpha-beta法で探索する
  def alphabetaEval(heuristic: Heuristic, depth: Int, a: Int, b: Int, game: Game): Int = {
    if(gameOver(game)) return winLose(game)
    if(depth<=0) return heuristic(game)
    val (board, player) = game
    val moves = myValidMoves(board, player)
    if(moves == Nil) return alphabetaEval(heuristic, depth-1, a, b, (board, opponent(player)))
    /*
      skipが発生したときdepthの変化量によって
        -1: 最後に打つ人が同じ 読む深さが同じ
         0: 空白の数(つまり石の数の合計)が同じになる 裾野の広さが同じくらいになる
        +1: 最後に打つ人が同じ skipを重視し実質2手深く読む
      どれも一長一短なので悩む
    */
    if(player == Black){
      var alpha = a //aがそのまま返ってきた場合aを書き換えた場所がありそこで捨てられる
      for(move <- moves){
        alpha = max(alpha, alphabetaEval(heuristic, depth-1, alpha, b, applyMove(board, player, move)))
        if(alpha >= b) return alpha //実際は外側のminで捨てられる
      }
      return alpha
    }else{
      var beta = b
      for(move <- moves){
        beta = min(beta, alphabetaEval(heuristic, depth-1, a, beta, applyMove(board, player, move)))
        if(beta <= a) return beta
      }
      return beta
    }
  }

  // posListの並び順になっているか判定する
  // 検証用
  // 先に計算した手を優先するため探索する順番を変えると結果に違いが出る
  // 評価値が同じだった場合posListが前な方を優先することで結果を同じにする
  def compPos(pos1: Position, pos2: Position): Boolean = {
    pos1._2<pos2._2 || (pos1._2==pos2._2 && pos1._1<pos2._1)
  }

  // 中盤の手を返す
  def middlePhase(depth: Int, game: Game): Position = {
    val (board, player) = game
    val moves = myValidMoves(board, player)
    if(player == Black){
      val b = Int.MaxValue
      var alpha = Int.MinValue //tempScore
      var tempMove = (0, 0) //1回目(-∞, ∞)は必ず値が返ってくるので大丈夫
      for(move <- moves){
        val chalScore = alphabetaEval(calcValue, depth-1, alpha, b, applyMove(board, player, move))
        if(alpha < chalScore){// || (alpha == chalScore && compPos(move, tempMove))){
          alpha = chalScore
          tempMove = move
        }
      }
      tempMove
    }else{
      val a = Int.MinValue
      var beta = Int.MaxValue
      var tempMove = (0, 0)
      for(move <- moves){
        val chalScore = alphabetaEval(calcValue, depth-1, a, beta, applyMove(board, player, move))
        if(beta > chalScore){// || (beta == chalScore && compPos(move, tempMove))){
          beta = chalScore
          tempMove = move
        }
      }
      tempMove
    }
  }

  // 終盤の評価値をalpha-beta法で探索する
  // heuristicやdepthが省略されている
  def endPhaseEval(a: Int, b: Int, game: Game): Int = {
    if(gameOver(game)) return countDiff(game)
    val (board, player) = game
    val moves = myValidMoves(board, player)
    if(moves == Nil) return endPhaseEval(a, b, (board, opponent(player)))
    if(player == Black){
      var alpha = a //aがそのまま返ってきた場合aを書き換えた場所がありそこで捨てられる
      for(move <- moves){
        alpha = max(alpha, endPhaseEval(alpha, b, applyMove(board, player, move)))
        if(alpha >= b) return alpha //実際は外側のminで捨てられる
      }
      return alpha
    }else{
      var beta = b
      for(move <- moves){
        beta = min(beta, endPhaseEval(a, beta, applyMove(board, player, move)))
        if(beta <= a) return beta
      }
      return beta
    }
  }

  // 終盤の手を返す
  def endPhase(game: Game): Position = {
    val (board, player) = game
    val moves = myValidMoves(board, player)
    if(player == Black){
      val b = 65
      var alpha = -65 //tempScore
      var tempMove = (0, 0) //1回目(-∞, ∞)は必ず値が返ってくるので大丈夫
      for(move <- moves){
        val chalScore = endPhaseEval(alpha, b, applyMove(board, player, move))
        if(alpha < chalScore){
          alpha = chalScore
          tempMove = move
        }
      }
      tempMove
    }else{
      val a = -65
      var beta = 65
      var tempMove = (0, 0)
      for(move <- moves){
        val chalScore = endPhaseEval(a, beta, applyMove(board, player, move))
        if(beta > chalScore){
          beta = chalScore
          tempMove = move
        }
      }
      tempMove
    }
  }

  def saikyoAIpos(depth: Int, depthEnd: Int, game: Game): Position = {
    val remainEmpty = countEmpty(game)
    if(remainEmpty >= (60 - 4)) {
      return try{ initialPhase(60 - remainEmpty, game) }
      catch{ case e: Exception => 
        //println("Unexpected Opening")
        middlePhase(depth, game)
      } //定石を外れたときは中盤として探索
    }
    if(remainEmpty <= depthEnd) return endPhase(game)
    middlePhase(depth, game)
  }

  def saikyoAI(depth: Int, depthEnd: Int): Strategy = {
    game =>
      saikyoAIpos(depth, depthEnd, game)
  }

  // 人間のキー入力を受け取る
  // (0, depth)でAIの手を確認する
  def human: Strategy = {
    game =>
      val (board, player) = game
      val strMove = io.StdIn.readLine().split(' ')
      val move = (strMove(0).toInt, strMove(1).toInt)
      if(move._1 == 0) saikyoAIpos(move._2, 0, game)
      else if (!(outflanks(board, player, move))) {
        println("Not a valid move! Please try again.");
        human(game)
      }
      else move
  }

}

object OthelloMain extends App {
  import OthelloLib._

  // 1つ目の randomMove を自分の戦略に変更
  //playLoop(newGame, randomMove, randomMove)
  playLoop(newGame, saikyoAI(6, 12), randomMove)
  //playLoop(newGame, randomMove, saikyoAI(6, 12))
  //playLoop(newGame, saikyoAI(6, 12), saikyoAI(6, 12))
  //playLoop(newGame, saikyoAI(6, 12), human)
  //playLoop(newGame, human, saikyoAI(6, 12))
}
