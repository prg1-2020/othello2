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
import spire.math.ULong
import spire.math.UByte

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
  type Board = (ULong, ULong)

  // 盤面の初期値
  val initBoard: Board =
    (new ULong(0x0000000810000000L), new ULong(0x0000001008000000L))

  // マス目の座標のリスト
  val posList: List[Position] =
    List(
      (1, 1),
      (2, 1),
      (3, 1),
      (4, 1),
      (5, 1),
      (6, 1),
      (7, 1),
      (8, 1),
      (1, 2),
      (2, 2),
      (3, 2),
      (4, 2),
      (5, 2),
      (6, 2),
      (7, 2),
      (8, 2),
      (1, 3),
      (2, 3),
      (3, 3),
      (4, 3),
      (5, 3),
      (6, 3),
      (7, 3),
      (8, 3),
      (1, 4),
      (2, 4),
      (3, 4),
      (4, 4),
      (5, 4),
      (6, 4),
      (7, 4),
      (8, 4),
      (1, 5),
      (2, 5),
      (3, 5),
      (4, 5),
      (5, 5),
      (6, 5),
      (7, 5),
      (8, 5),
      (1, 6),
      (2, 6),
      (3, 6),
      (4, 6),
      (5, 6),
      (6, 6),
      (7, 6),
      (8, 6),
      (1, 7),
      (2, 7),
      (3, 7),
      (4, 7),
      (5, 7),
      (6, 7),
      (7, 7),
      (8, 7),
      (1, 8),
      (2, 8),
      (3, 8),
      (4, 8),
      (5, 8),
      (6, 8),
      (7, 8),
      (8, 8)
    )

  // ゲームの状態
  type Game = (Board, Player)

  // ゲームの初期値
  val newGame = (initBoard, Black)

  // board の pos にあるマス目の情報を返す
  def boardRef(board: Board, pos: Position): Square = {
    val (x, y) = pos
    val (black, white) = board
    var mask: ULong = posToLong(pos)
    if ((black & mask) != ULong(0)) Black
    else if ((white & mask) != ULong(0)) White
    else Empty
  }

  /** pos(x,y)だけが1になるULongを返す
    * @param pos
    * @return
    */
  def posToLong(pos: Position): ULong = {
    val (x, y) = pos
    new ULong(0x8000000000000000L) >> ((x - 1) + (y - 1) * 8)
  }

  /** bit列の転置を返す
    *
    * @param a
    */
  def transpose(a: ULong): ULong = {
    var b = a
    var t: ULong = (b ^ (b >> 7)) & ULong(0x00aa00aa00aa00aaL)
    b = b ^ t ^ (t << 7);
    t = (b ^ (b >> 14)) & ULong(0x0000cccc0000ccccL)
    b = b ^ t ^ (t << 14);
    t = (b ^ (b >> 28)) & ULong(0x00000000f0f0f0f0L)
    b = b ^ t ^ (t << 28);
    b
  }

  /** UByteの逆列を返す
    *
    * @param byteBoard
    */
  def reverseByte(byteBoard: UByte): UByte = {
    var reverse = (byteBoard >> 4) | (byteBoard << 4)
    reverse = ((reverse & UByte(204)) >> 2) | ((reverse & UByte(51)) << 2)
    reverse = ((reverse & UByte(170)) >> 1) | ((reverse & UByte(85)) << 1)
    reverse
  }
  def longToPosList(board: ULong): List[Position] = {
    var mask = new ULong(0x8000000000000000L)
    var list = List.empty[Position]
    for (i <- 1 to 8) {
      for (j <- 1 to 8) {
        if ((board & (mask >> ((j - 1) + (i - 1) * 8))) != ULong(0)) {
          list = (j, i) :: list
        }
      }
    }
    list
  }

  // board の pos に player が石を置いたときに、相手の石を取り囲むかを判定する
  def outflanks(board: Board, player: Player, pos: Position): Boolean = {
    val (x, y) = pos
    (boardRef(board, pos) == Empty) && (!(flippedPositions(
      board,
      player,
      pos
    ) == ULong(0)))
  }

  // player の敵を返す
  def opponent(player: Player): Player = {
    if (player == Black) White else Black
  }

  // bitを数える
  def popcnt(v: ULong): ULong = {
    var count =
      (v & ULong(0x5555555555555555L)) + ((v >> 1) & ULong(0x5555555555555555L))
    count = (count & ULong(0x3333333333333333L)) + ((count >> 2) & ULong(
      0x3333333333333333L
    ))
    count = (count & ULong(0x0f0f0f0f0f0f0f0fL)) + ((count >> 4) & ULong(
      0x0f0f0f0f0f0f0f0fL
    ))
    count = (count & ULong(0x00ff00ff00ff00ffL)) + ((count >> 8) & ULong(
      0x00ff00ff00ff00ffL
    ))
    count = (count & ULong(0x0000ffff0000ffffL)) + ((count >> 16) & ULong(
      0x0000ffff0000ffffL
    ))
    return (count & ULong(0x00000000ffffffffL)) + ((count >> 32) & ULong(
      0x00000000ffffffffL
    ));
  }

  // board 上の player の石の数を返す
  def countPieces(game: Game): Int = {
    val (board, player) = game
    val (black, white) = board
    player match {
      case Black => popcnt(black).toInt
      case White => popcnt(white).toInt
    }
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

    // def drawRow(row: List[Square]): Unit = {
    //   row match {
    //     case Nil     => printf("\n")
    //     case s :: ss => printf("%s ", squareToString(s)); drawRow(ss)
    //   }
    // }

    // board match {
    //   case Nil     => printf("\n"); printf("---------------\n")
    //   case r :: rs => drawRow(r); drawBoard(rs)
    // }
    for (i <- 0 to 7) {
      for (j <- 0 to 7) {
        var (black, white) = board
        var mask = ULong(0x8000000000000000L) >> (j + i * 8)
        if ((black & mask) != ULong(0)) printf("%s ", squareToString(Black))
        else if ((white & mask) != ULong(0))
          printf("%s ", squareToString(White))
        else if (((black | white) & mask) == ULong(0))
          printf("%s ", squareToString(Empty))
      }
      printf("\n")
    }
    printf("\n"); printf("---------------\n")

  }

  // ゲームオーバかどうかを判定する
  def gameOver(game: Game): Boolean = {
    val (board, player) = game
    // !(posList.foldRight(false)((p, b) => b || outflanks(board, player, p)) ||
    //   posList.foldRight(false)((p, b) =>
    //     b || outflanks(board, opponent(player), p)
    //   ))
    (validMoves(board, player) == ULong(0)) && (validMoves(
      board,
      opponent(player)
    ) == ULong(0))
  }

  /** board の pos に player が石を置いたときに、色が反転するマス目のLongを返す
    * @param board
    * @param player
    * @param pos
    * @return
    */
  def flippedPositions( //reverse
      board: Board,
      player: Player,
      pos: Position
  ): ULong = {
    def transfer(put: ULong, k: Int): ULong = {
      k match {
        case 0 => (put << 8) & ULong(0xffffffffffffff00L)
        case 1 => (put << 7) & ULong(0x7f7f7f7f7f7f7f00L)
        case 2 => (put >> 1) & ULong(0x7f7f7f7f7f7f7f7fL)
        case 3 => (put >> 9) & ULong(0x007f7f7f7f7f7f7fL)
        case 4 => (put >> 8) & ULong(0x00ffffffffffffffL)
        case 5 => (put >> 7) & ULong(0x00fefefefefefefeL)
        case 6 => (put << 1) & ULong(0xfefefefefefefefeL)
        case 7 => (put << 9) & ULong(0xfefefefefefefe00L)
        case _ => ULong(0)
      }
    }
    // // Position の足し算
    // def posPlus(pos: Position, x: Int, y: Int): Position = {
    //   val (x0, y0) = pos
    //   (x0 + x, y0 + y)
    // }

    // // pos から (x, y) 方向に向かって反転する点を探す
    // def trim(pos: Position, x: Int, y: Int): Long = {

    //   def trimAux(
    //       pos: Position,
    //       x: Int,
    //       y: Int,
    //       list: List[Position]
    //   ): List[Position] = {
    //     val nextPos = posPlus(pos, x, y)
    //     nextPos match {
    //       case (nextx, nexty) =>
    //         if ((nextx < 1) || (nextx > 8) || (nexty < 1) || (nexty > 8)) Nil
    //         else if (boardRef(board, nextPos) == Empty) Nil
    //         else if (boardRef(board, nextPos) == player) list
    //         else trimAux(nextPos, x, y, nextPos :: list)
    //     }
    //   }

    //   trimAux(pos, x, y, Nil)
    // }

    // trim(pos, 0, 1) ++ trim(pos, 1, 1) ++ trim(pos, 1, 0) ++ trim(pos, 1, -1) ++
    //   trim(pos, 0, -1) ++ trim(pos, -1, -1) ++ trim(pos, -1, 0) ++ trim(
    //     pos,
    //     -1,
    //     1
    //   )
    val (black, white) = board
    val (playerBoard, opponentBoard) =
      if (player == Black) (black, white) else (white, black)
    var rev: ULong = ULong(0)
    for (i <- 0 to 7) {
      var rev_1: ULong = ULong(0)
      var mask: ULong = transfer(posToLong(pos), i)
      while ((mask != ULong(0)) && ((mask & opponentBoard) != ULong(0))) {
        rev_1 |= mask
        mask = transfer(mask, i)
      }
      if ((mask & playerBoard) != ULong(0)) rev |= rev_1
    }
    rev
  }

  // player が石を置ける board 上の座標のリストを返す
  def validMoves(board: Board, player: Player): ULong = {
    // def valid(playerBoard: ULong, opponentBoard: ULong): ULong = {
    //   var returnBoard = ULong(0)
    //   for (i <- 0 to 7) {
    //     var playerByte = new UByte((playerBoard >> ((7 - i) * 8)).toByte)
    //     var opponentByte = new UByte((opponentBoard >> ((7 - i) * 8)).toByte)
    //     var mask = playerByte << 1
    //     var ans = (~(mask | opponentByte)) & (mask + opponentByte)
    //     returnBoard = returnBoard | ULong(ans.toLong) << ((7 - i) * 8)

    //     playerByte = reverseByte(playerByte)
    //     opponentByte = reverseByte(opponentByte)
    //     mask = playerByte << 1
    //     ans = reverseByte((~(mask | opponentByte)) & (mask + opponentByte))
    //     returnBoard = returnBoard | ULong(ans.toLong) << ((7 - i) * 8)
    //   }
    //   returnBoard
    // }
    // val (black, white) = board
    // var (playerBoard, opponentBoard) =
    //   if (player == Black) (black, white) else (white, black)
    // var validBoard = ULong(0)
    // validBoard = validBoard | valid(playerBoard, opponentBoard)
    // validBoard =
    //   validBoard | valid(transpose(playerBoard), transpose(opponentBoard))

    // validBoard

    // posList
    //   .filter(outflanks(board, player, _))
    //   .filter(boardRef(board, _) == Empty)

    val (black, white) = board
    val (playerBoard, opponentBoard) =
      if (player == Black) (black, white) else (white, black)

    //左右端の番人
    val horizontalWatchBoard: ULong = opponentBoard & ULong(0x7e7e7e7e7e7e7e7eL)
    //上下端の番人
    val verticalWatchBoard: ULong = opponentBoard & ULong(0x00ffffffffffff00L)
    //全辺の番人
    val allSideWatchBoard: ULong = opponentBoard & ULong(0x007e7e7e7e7e7e00L)
    //空きマスのみにビットが立っているボード
    val blankBoard: ULong = ~(playerBoard | opponentBoard)
    //隣に相手の色があるかを一時保存する
    var tmp: ULong = ULong(0)
    //返り値
    var legalBoard: ULong = ULong(0)

    tmp = horizontalWatchBoard & (playerBoard << 1)
    tmp |= horizontalWatchBoard & (tmp << 1)
    tmp |= horizontalWatchBoard & (tmp << 1)
    tmp |= horizontalWatchBoard & (tmp << 1)
    tmp |= horizontalWatchBoard & (tmp << 1)
    tmp |= horizontalWatchBoard & (tmp << 1)
    legalBoard = blankBoard & (tmp << 1)

    //右
    tmp = horizontalWatchBoard & (playerBoard >> 1)
    tmp |= horizontalWatchBoard & (tmp >> 1)
    tmp |= horizontalWatchBoard & (tmp >> 1)
    tmp |= horizontalWatchBoard & (tmp >> 1)
    tmp |= horizontalWatchBoard & (tmp >> 1)
    tmp |= horizontalWatchBoard & (tmp >> 1)
    legalBoard |= blankBoard & (tmp >> 1)

    //上
    tmp = verticalWatchBoard & (playerBoard << 8)
    tmp |= verticalWatchBoard & (tmp << 8)
    tmp |= verticalWatchBoard & (tmp << 8)
    tmp |= verticalWatchBoard & (tmp << 8)
    tmp |= verticalWatchBoard & (tmp << 8)
    tmp |= verticalWatchBoard & (tmp << 8)
    legalBoard |= blankBoard & (tmp << 8)

    //下
    tmp = verticalWatchBoard & (playerBoard >> 8)
    tmp |= verticalWatchBoard & (tmp >> 8)
    tmp |= verticalWatchBoard & (tmp >> 8)
    tmp |= verticalWatchBoard & (tmp >> 8)
    tmp |= verticalWatchBoard & (tmp >> 8)
    tmp |= verticalWatchBoard & (tmp >> 8)
    legalBoard |= blankBoard & (tmp >> 8)

    //右斜め上
    tmp = allSideWatchBoard & (playerBoard << 7)
    tmp |= allSideWatchBoard & (tmp << 7)
    tmp |= allSideWatchBoard & (tmp << 7)
    tmp |= allSideWatchBoard & (tmp << 7)
    tmp |= allSideWatchBoard & (tmp << 7)
    tmp |= allSideWatchBoard & (tmp << 7)
    legalBoard |= blankBoard & (tmp << 7)

    //左斜め上
    tmp = allSideWatchBoard & (playerBoard << 9)
    tmp |= allSideWatchBoard & (tmp << 9)
    tmp |= allSideWatchBoard & (tmp << 9)
    tmp |= allSideWatchBoard & (tmp << 9)
    tmp |= allSideWatchBoard & (tmp << 9)
    tmp |= allSideWatchBoard & (tmp << 9)
    legalBoard |= blankBoard & (tmp << 9)

    //右斜め下
    tmp = allSideWatchBoard & (playerBoard >> 9)
    tmp |= allSideWatchBoard & (tmp >> 9)
    tmp |= allSideWatchBoard & (tmp >> 9)
    tmp |= allSideWatchBoard & (tmp >> 9)
    tmp |= allSideWatchBoard & (tmp >> 9)
    tmp |= allSideWatchBoard & (tmp >> 9)
    legalBoard |= blankBoard & (tmp >> 9)

    //左斜め下
    tmp = allSideWatchBoard & (playerBoard >> 7)
    tmp |= allSideWatchBoard & (tmp >> 7)
    tmp |= allSideWatchBoard & (tmp >> 7)
    tmp |= allSideWatchBoard & (tmp >> 7)
    tmp |= allSideWatchBoard & (tmp >> 7)
    tmp |= allSideWatchBoard & (tmp >> 7)
    legalBoard |= blankBoard & (tmp >> 7)

    legalBoard

  }

  // board の pos に player が石を置いた結果、得られる状態を返す
  def applyMove(board: Board, player: Player, pos: Position): Game = {

    // // 行数の分だけ makeRow を行い、その結果を使って盤面を作る
    // def makeBoard(board: Board, flipList: List[Position], y: Int): Board = {
    //   board match {
    //     case Nil     => Nil
    //     case r :: rs =>
    //       // y 行目の座標のうち、色が反転するもののリスト
    //       val flipListY =
    //         flipList.filter(p => p match { case (px, py) => py == y })
    //       makeRow(r, flipListY, 1, y) :: makeBoard(rs, flipList, y + 1)
    //   }
    // }

    // // 反転後の行を作る
    // def makeRow(
    //     row: List[Square],
    //     flipListY: List[Position],
    //     x: Int,
    //     y: Int
    // ): List[Square] = {
    //   row match {
    //     case Nil => Nil
    //     case square :: squares => {
    //       val newSquare =
    //         // 反転リストに入っている座標は player
    //         if (contain(x, flipListY)) player
    //         // player が石を置く場所は player
    //         else if (pos == (x, y)) player
    //         // それ以外はそのまま
    //         else square
    //       newSquare :: makeRow(squares, flipListY, x + 1, y)
    //     }
    //   }
    // }

    // // (x, y) が flipListY に含まれるかを判定
    // def contain(x: Int, flipListY: List[Position]): Boolean = {
    //   flipListY match {
    //     case Nil            => false
    //     case (px, py) :: ps => if (px == x) true else contain(x, ps)
    //   }
    // }

    if (!(outflanks(board, player, pos))) {
      throw new Exception("not a valid move")
    } else {
      // 反転する座標のリスト
      val flipList = flippedPositions(board, player, pos)
      val (black, white) = board
      var (playerBoard, opponentBoard) =
        if (player == Black) (black, white) else (white, black)
      playerBoard = playerBoard ^ (posToLong(pos) | flipList)
      opponentBoard = opponentBoard ^ flipList
      // 次のプレイヤー
      val nextplayer = opponent(player)
      if (player == Black) ((playerBoard, opponentBoard), nextplayer)
      else ((opponentBoard, playerBoard), nextplayer)
    }
  }

  // 戦略
  type Strategy = Game => Position

  // posList を前から順番に見ていき、可能な手を見つける
  def firstMove(game: Game): Position = {

    def firstMoveAux(list: List[Position]): Position = {
      val (board, player) = game
      list match {
        case Nil     => throw new Exception("no valid move")
        case p :: ps => if (outflanks(board, player, p)) p else firstMoveAux(ps)
      }
    }

    firstMoveAux(posList)
  }

  // 人間のキー入力を受け取る
  def human: Strategy = { game =>
    val (board, player) = game
    val strMove = io.StdIn.readLine().split(' ')
    val move = (strMove(0).toInt, strMove(1).toInt)
    if (!(outflanks(board, player, move))) {
      println("Not a valid move! Please try again.");
      human(game)
    } else move
  }

  // ヒューリスティック
  type Heuristic = Game => Int

  // 黒 - 白 の値を返す
  def countDiff: Heuristic = { game =>
    countPieces(game._1, Black) - countPieces(game._1, White)
  }

  // 戦略の適用
  def applyStrategy(game: Game, strategy: Strategy): Game = {
    val (board, player) = game
    val nextPlayer = opponent(player)
    if (
      !(posList.foldRight(false)((p, b) => b || outflanks(board, player, p)))
    ) {
      printf("skip!\n");
      (board, nextPlayer)
    } else {
      val pos = strategy(game)
      if (!(outflanks(board, player, pos))) {
        throw new Exception("invalid move")
      } else {
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
      printf(
        "Black: %d, White: %d, Winner: %s\n",
        blackscore,
        whitescore,
        winner
      );
      sys.exit()
    } else {
      drawBoard(board);
      val newgame = applyStrategy(game, strat1)
      playLoop(newgame, strat2, strat1)
    }
  }

  /////////
  // 課題 //
  /////////

  // 1. minimaxEval
  // 目的：minimax法に基づいてゲームの状態を評価する
  def minimaxEval(heuristic: Heuristic, depth: Int, game: Game): Int = {
    val (board, player) = game;
    // val oppo = opponent(player);

    if (depth == 0 || gameOver(game)) { //深さ0 or GameOverなら評価
      return heuristic(game)
    } else {
      val validList = validMoves(board, player)
      if (longToPosList(validList) == Nil) { //置ける場所がない（＝スキップ）なら相手側へ
        return minimaxEval(heuristic, depth, (board, opponent(player)))
      } else {
        player match {
          case Black =>
            longToPosList(validList).foldLeft(Int.MinValue)((tmp, pos) =>
              max(
                tmp,
                minimaxEval(
                  heuristic,
                  depth - 1,
                  applyMove(board, player, pos)
                )
              )
            )
          case White =>
            longToPosList(validList).foldLeft(Int.MaxValue)((tmp, pos) =>
              min(
                tmp,
                minimaxEval(
                  heuristic,
                  depth - 1,
                  applyMove(board, player, pos)
                )
              )
            )
        }
      }
    }
  }

  // 2. minimax
  // 目的：minimax法に基づいて最適な手を示す
  def minimax(heuristic: Heuristic, depth: Int): Strategy = { game =>
    val (board, player) = game
    val validList = validMoves(board, player)
    player match {
      case Black => {
        longToPosList(validList)
          .foldLeft((Int.MinValue, (-1, -1)))((tmp, pos) => {
            val (tmp_max, tmp_pos) = tmp
            val score =
              minimaxEval(heuristic, depth - 1, applyMove(board, player, pos))

            if (tmp_max < score) (score, pos)
            else tmp
          })
          ._2

      }
      case White => {
        longToPosList(validList)
          .foldLeft((Int.MaxValue, (-1, -1)))((tmp, pos) => {
            val (tmp_min, tmp_pos) = tmp
            val score =
              minimaxEval(heuristic, depth - 1, applyMove(board, player, pos))

            if (tmp_min > score) (score, pos)
            else tmp
          })
          ._2
      }
    }
  }

  // 3. alphabetaEval
  // 目的：alphabeta法に基づいてゲームの状態を評価する
  def alphabetaEval(
      heuristic: Heuristic,
      depth: Int,
      a: Int,
      b: Int,
      game: Game
  ): Int = {
    val (board, player) = game
    if (depth == 0 || gameOver(game)) return heuristic(game)
    else {
      val validList = validMoves(board, player)
      if (longToPosList(validList) == Nil)
        return alphabetaEval(
          heuristic,
          depth,
          a,
          b,
          (board, opponent(player))
        )
      else {
        player match {
          case Black => {
            var alpha = a
            longToPosList(validList).foreach(pos => {
              alpha = max(
                alpha,
                alphabetaEval(
                  heuristic,
                  depth - 1,
                  alpha,
                  b,
                  applyMove(board, player, pos)
                )
              )
              if (alpha > b) return alpha
            })
            return alpha
          }
          case White => {
            var beta = b
            longToPosList(validList).foreach(pos => {
              beta = min(
                beta,
                alphabetaEval(
                  heuristic,
                  depth - 1,
                  a,
                  beta,
                  applyMove(board, player, pos)
                )
              )
              if (a > beta) return beta
            })
            return beta
          }
        }
      }
    }
  }

  // 4. alphabeta
  // 目的：alphabeta法に基づいて最適な手を求める
  def alphabeta(heuristic: Heuristic, depth: Int): Strategy = { game =>
    val (board, player) = game
    val validList = validMoves(board, player)

    player match {
      case Black => {
        longToPosList(validList)
          .foldLeft((Int.MinValue, (-1, -1)))((tmp, pos) => {
            val (tmp_max, tmp_pos) = tmp
            val score =
              alphabetaEval(
                heuristic,
                depth - 1,
                tmp_max,
                Int.MaxValue,
                applyMove(board, player, pos)
              )

            if (tmp_max < score) (score, pos)
            else tmp
          })
          ._2
      }
      case White => {
        longToPosList(validList)
          .foldLeft((Int.MaxValue, (-1, -1)))((tmp, pos) => {
            val (tmp_min, tmp_pos) = tmp
            val score =
              alphabetaEval(
                heuristic,
                depth - 1,
                Int.MinValue,
                tmp_min,
                applyMove(board, player, pos)
              )

            if (tmp_min > score) (score, pos)
            else tmp
          })
          ._2
      }
    }
  }
}

object OthelloMain extends App {
  import OthelloLib._

  // どれか1つのコメントを外す

  // 黒, 白ともに firstMove
  // playLoop(newGame, firstMove, firstMove)

  // 黒：人間, 白：firstMove
  // playLoop(newGame, human, firstMove)

  // 黒, 白ともに深さ4の minimax 法
  // playLoop(newGame, minimax(countDiff, 4), minimax(countDiff, 4))

  // 黒, 白ともに深さ4の alpha-beta 法
  playLoop(newGame, alphabeta(countDiff, 8), alphabeta(countDiff, 6))
}

// 5. 実験結果
/*
実験１
黒の戦略：minimax(countDiff,4)
白の戦略：minimax(countDiff,4)
黒 vs. 白の数：Black: 36, White: 28, Winner: Black
実行時間 (Total time)：7s

実験２
黒の戦略：
白の戦略：
黒 vs. 白の数：
実行時間 (Total time)：

実験３
黒の戦略：
白の戦略：
黒 vs. 白の数：
実行時間 (Total time)：

実験４
黒の戦略：
白の戦略：
黒 vs. 白の数：
実行時間 (Total time)：

考察：


 */
