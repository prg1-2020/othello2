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

  // 板面の石の総数を返す
  def countPlus: Heuristic = {
    game => countPieces(game._1, Black) + countPieces(game._1, White)
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
      val newgame = applyStrategy(game, strat1)
      playLoop(newgame, strat2, strat1)
    }
  }

  //////////////////
  // オリジナルの戦略 //
  //////////////////

  def hugou(game:Game,pos:Position):Int={
    if(boardRef(game._1,pos)==Black) 1
    else if (boardRef(game._1,pos)==White) -1
    else 0
  }

  val appraiseList :List[(Position,Int)]=
    List(((1, 1),30), ((2, 1),-12), ((3, 1),0), ((4, 1),0), ((5, 1),0), ((6, 1),0), ((7, 1),-12), ((8, 1),30),
      ((1, 2),-12), ((2, 2),-15), ((3, 2),-3), ((4, 2),-3), ((5, 2),-3), ((6, 2),-3), ((7, 2),-15), ((8, 2),-12),
      ((1, 3),0), ((2, 3),-3), ((3, 3),0), ((4, 3),-1), ((5, 3),-1), ((6, 3),0), ((7, 3),-3), ((8, 3),0),
      ((1, 4),0), ((2, 4),-3), ((3, 4),-1), ((4, 4),-1), ((5, 4),-1), ((6, 4),-1), ((7, 4),-3), ((8, 4),0),
      ((1, 5),0), ((2, 5),-3), ((3, 5),-1), ((4, 5),-1), ((5, 5),-1), ((6, 5),-1), ((7, 5),-3), ((8, 5),0),
      ((1, 6),0), ((2, 6),-3), ((3, 6),0), ((4, 6),-1), ((5, 6),-1), ((6, 6),0), ((7, 6),-3), ((8, 6),0),
      ((1, 7),-12), ((2, 7),-15), ((3, 7),-3), ((4, 7),-3), ((5, 7),-3), ((6, 7),-3), ((7, 7),-15), ((8, 7),-12),
      ((1, 8),30), ((2, 8),-12), ((3, 8),0), ((4, 8),0), ((5, 8),0), ((6, 8),0), ((7, 8),-12), ((8, 8),30))
  
  def saikyou:Heuristic={
    game =>
      if (countPlus(game)<=44)appraiseList.foldLeft(0){(acc,x) => acc + hugou(game,x._1)*x._2}
      else countDiff(game)
  }

  def alphabetaEval(heuristic: Heuristic, depth: Int, a: Int, b: Int, game: Game): Int = {
    if(depth == 0 || gameOver(game)) return heuristic(game)
    else {
      val validList = validMoves(game._1, game._2)
      if (validList == Nil) alphabetaEval(heuristic, depth, a, b, (game._1, opponent(game._2)))
      else { 
        game._2 match {
          case Black => {
            var alpha = a
            for(pos <- validList){
              val v = alphabetaEval(heuristic, depth - 1, alpha, b, applyMove(game._1, game._2, pos))
              if(v >= b) return v 
              alpha = max(alpha, v)
            }
            return alpha
          }
          case White => {
            var beta = b
            for(pos <- validList){
              val v = alphabetaEval(heuristic, depth - 1, a, beta, applyMove(game._1, game._2, pos))
              if(v <= a) return v 
              beta = min(beta, v)
            }
            return beta
          }
        }
      }
    }
  }

  def alphabeta(heuristic: Heuristic, depth: Int): Strategy = {
    game =>
      var startposition:Position = (0, 0)
        game._2 match {
          case Black => {
            var setvalue = Int.MinValue
            for(pos <- validMoves(game._1, game._2)){
              val appraiseEval = alphabetaEval(heuristic, depth - 1, Int.MinValue, Int.MaxValue, applyMove(game._1, Black, pos))
              if(setvalue < appraiseEval){
                setvalue = appraiseEval
                startposition = pos
              }
            }
          }
          case White => {
            var setvalue = Int.MaxValue
            for(pos <- validMoves(game._1, game._2)){
              val appraiseEval = alphabetaEval(heuristic, depth - 1, Int.MinValue, Int.MaxValue, applyMove(game._1, White, pos))
              if(setvalue > appraiseEval){
                setvalue = appraiseEval
                startposition = pos
              }
            }
          }
        }
        startposition 
  }
}

object OthelloMain extends App {
  import OthelloLib._

  // 1つ目の randomMove を自分の戦略に変更
  playLoop(newGame, alphabeta(saikyou,5), randomMove)
}
