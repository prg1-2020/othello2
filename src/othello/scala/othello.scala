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

  def countP(arBoard: Array[Array[Square]], player: Player): (Int,Int) = {
    var cM = 0
    var cO = 0
    val oppo = opponent(player)

    for(i <- 0 until 8){
      for(j <- 0 until 8){
        if(arBoard(i)(j) == player) cM+=1
        else if(arBoard(i)(j) == oppo) cO += 1
      }
    }

    (cM, cO)
  }

  def printArBoard(arBoard: Array[Array[Square]]){
    
    for(i <- 0 until 8){
      for(j <- 0 until 8){
        printf("%s ", squareToString(arBoard(i)(j)))
      }
      printf("\n")
    }
    printf("---------------\n")
  }

  def countFreedom(arBoard: Array[Array[Square]], player: Player) : (Int,Int) = {
    var fM = 0
    var fO = 0
    var dir = Array((-1,-1), (-1,0), (-1,1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

    for(i <- 0 until 8){
      for(j <- 0 until 8){
        val now = arBoard(i)(j)
        if(now != Empty){
          dir.foreach(d => {
            val a = i + d._1
            val b = j + d._2
            if(a >= 0 && a <= 7 && b >= 0 && b <= 7 && arBoard(a)(b) == Empty){
              if(player == now) fM += 1
              else fO += 1
            }
          })
        }
      }
    }
    (fM, fO)
  }

  def countSettle(arBoard: Array[Array[Square]], player: Player): (Int,Int) = {
    var kakuBoard:Array[Array[Square]] =
    Array(Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
         Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty))

    var cM = 0
    var cO = 0
    

    for(i <- 0 until 8){
      for(j <- 0 until 8){
        val now = arBoard(i)(j)
        if(now != Empty && kakuBoard(i)(j) == Empty){
          val oppo = if(now == White) Black else White

          var u:Square = if(i == 0 || kakuBoard(i-1)(j) == now) now 
                          else if(kakuBoard(i-1)(j) == Empty) Empty
                          else oppo
          var l:Square = if(j == 0 || kakuBoard(i)(j-1) == now) now 
                          else if(kakuBoard(i)(j-1) == Empty) Empty
                          else oppo
          var d:Square = if(i == 7 || kakuBoard(i+1)(j) == now) now 
                          else if(kakuBoard(i+1)(j) == Empty) Empty
                          else oppo
          var r:Square = if(j == 7 || kakuBoard(i)(j+1) == now) now 
                          else if(kakuBoard(i)(j+1) == Empty) Empty
                          else oppo
          if((u==now || d==now || (u != Empty && d != Empty)) && (l==now || r==now || (l != Empty && r != Empty))){
            kakuBoard(i)(j) = now
            if(now == player) cM+=1
            else cO +=1
          }
        }
      }
    }

    for(i <- 7 to 0 by -1){
      for(j <- 7 to 0 by -1){
        val now = arBoard(i)(j)
        if(now != Empty && kakuBoard(i)(j) == Empty){
          val oppo = if(now == White) Black else White
          var u:Square = if(i == 0 || kakuBoard(i-1)(j) == now) now 
                          else if(kakuBoard(i-1)(j) == Empty) Empty
                          else oppo
          var l:Square = if(j == 0 || kakuBoard(i)(j-1) == now) now 
                          else if(kakuBoard(i)(j-1) == Empty) Empty
                          else oppo
          var d:Square = if(i == 7 || kakuBoard(i+1)(j) == now) now 
                          else if(kakuBoard(i+1)(j) == Empty) Empty
                          else oppo
          var r:Square = if(j == 7 || kakuBoard(i)(j+1) == now) now 
                          else if(kakuBoard(i)(j+1) == Empty) Empty
                          else oppo
          if((u==now || d==now || (u != Empty && d != Empty)) && (l==now || r==now || (l != Empty && r != Empty))){
            kakuBoard(i)(j) = now
            if(now == player) cM+=1
            else cO +=1
          }
        }
      }
    }

      // printArBoard(kakuBoard)
      // printf("%d %d", cM,cO)
    (cM, cO)
  }

  def countCand(game: Game): Int = {
    val (board, player) = game
    posList.foldLeft(0)(
      (c, p) => {
        if(outflanks(board, player, p)) c + 1
        else c
      }
    )
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

  // posList を前から順番に見ていき、可能な手を見つける
  def firstMove(game: Game): Position = {

    def firstMoveAux(list: List[Position]): Position = {
      val (board, player) = game
      list match {
        case Nil => throw new Exception("no valid move")
        case p :: ps => if (outflanks(board, player, p)) p else firstMoveAux(ps)
      }
    }

    firstMoveAux(posList)
  }

  // 人間のキー入力を受け取る
  def human: Strategy = {
    game =>
      val (board, player) = game
      val strMove = io.StdIn.readLine().split(' ')
      val move = (strMove(0).toInt, strMove(1).toInt)
      if (!(outflanks(board, player, move))) {
        println("Not a valid move! Please try again.");
        human(game)
      }
      else move
  }

  // ヒューリスティック
  type Heuristic = Game => Int

  // 黒 - 白 の値を返す
  def countDiff: Heuristic = {
    game => {
      val (board, player) = game
      countPieces(board, player) - countPieces(board, opponent(player))
    }
  }

  def convAr(board:Board):Array[Array[Square]] = {

      var arBoard = Array.ofDim[Square](8,8)
      var i = 0
      var j = 0
      board.foreach(r => {
        r.foreach(p => {
          arBoard(i)(j) = p
          j += 1
        })
        j = 0
        i += 1
      }) 
      arBoard
  }

  def staticEval(sCoeff:Float, cCoeff:Float, kCoeff:Float, iCoeff:Float): Heuristic = {
    game => {
      val (board, player) = game

      var arBoard = convAr(board)

      val (isim,isio) = countP(arBoard, player)
      val isi = isim - isio
      if(isim +isio == 64 || isim == 0 || isio == 0){
        if(isim < isio) -30000
        else 30000
      }
      else {
      // if(64 - isim + isio <= 6) isi
      // else {
      val (kM,kO) = countFreedom(arBoard, player)
      val cand = countCand(game) - countCand((board, opponent(player)))
      val (sM,sO) = countSettle(arBoard, player)
      ((sM - sO) * sCoeff  + cand * cCoeff + (kM - kO) * kCoeff  + isi * iCoeff).toInt
      // }
      }
    }
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
        throw new Exception("invalid move")
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

      val arBoard = convAr(board)
      val s=countSettle(arBoard, Black)
      val f=countFreedom(arBoard, Black)
      printf("s:%d,%d k:%d,%d\n",s._1,s._2, f._1, f._2)

      val newgame = applyStrategy(game, strat1)
      playLoop(newgame, strat2, strat1)
    }
  }

  /////////
  // 課題 //
  /////////

  // 1. minimaxEval
  // 目的：
  def minimaxEval(heuristic: Heuristic, depth: Int, game: Game): Int = {
    val (board, player) = game
    val oppo = if(player == Black) White else Black

    if(depth == 0) {
      heuristic(game)
    } else {
      val (s,p) = posList.foldLeft((-1000, (-1,-1):Position))(
        (s, p) => {
          val (cMax, cHand) = s
          if(outflanks(board, player, p)) {
            val cs = -minimaxEval(heuristic, depth - 1, applyMove(board, player, p))
            if(cs > cMax) (cs, p)
            else s
          }
          else s
        }
      )

      if(s == -1000) {
        return -minimaxEval(heuristic, depth - 1, (board, oppo))
      } else {
        s
      }
    }
  }

  // 2. minimax
  // 目的：
  def minimax(heuristic: Heuristic, depth: Int): Strategy = {
    game => {
      val (board, player) = game
      posList.foldLeft((-1000, (-1,-1):Position))(
        (s, p) => {
          val (cMax, cHand) = s
          if(outflanks(board, player, p)) {
            val cs = -minimaxEval(heuristic, depth, applyMove(board, player, p))
            if(cs > cMax) (cs, p)
            else s
          }
          else s
        }
      )._2
    }
  }

  // 3. alphabetaEval
  // 目的：
  def alphabetaEval(heuristic: Heuristic, depth: Int, a: Int, b: Int, game: Game): Int = {
    val (board, player) = game
    val oppo = if(player == Black) White else Black

    if(depth == 0) {
      heuristic(game)
    } else {
      val (s,p) = posList.foldLeft((-100000, (-1,-1):Position))(
        (s, p) => {
          val (cMax, cHand) = s
          if(cMax < b && outflanks(board, player, p)) {
            val cs = -alphabetaEval(heuristic, depth - 1, -b, -max(a,cMax), applyMove(board, player, p))
            if(cs > cMax) (cs, p)
            else s
          }
          else s
        }
      )

      if(s == -100000) {
        return -alphabetaEval(heuristic, depth - 1, -b, -a, (board, oppo))
      } else {
        s
      }
    }
  }

  // 4. alphabeta
  // 目的：
  def alphabeta(heuristic: Heuristic, depth: Int): Strategy = {
    game =>{
      val (board, player) = game
      posList.foldLeft((-100000, (-1,-1):Position))(
        (s, p) => {
          val (cMax, cHand) = s
          if(outflanks(board, player, p)) {
            val cs = -alphabetaEval(heuristic, depth, -10000, -cMax, applyMove(board, player, p))
            // assert(minimaxEval(heuristic, depth, applyMove(board, player, p)) == alphabetaEval(heuristic, depth, -1000, 1000, applyMove(board, player, p)))
            if(cs > cMax) (cs, p)
            else s
          }
          else s
        }
      )._2
    }
  }
}

object OthelloMain extends App {
  import OthelloLib._

  playLoop(newGame, alphabeta(staticEval(15,6,-4,1), 8),firstMove)
}
