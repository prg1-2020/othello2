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
//ゲームと手を受け取りひっくり返る＋置いた手のposリストを返す
  def whichFlip(board: Board, player: Player, pos: Position):List[Position]={

    pos::flippedPositions(board, player, pos)
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
       //printf("skip!\n");
       (board, nextPlayer)
    }
    else if(validMoves(board,player)== Nil)(board, nextPlayer)
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
  def alphabetaEval(heuristic: Heuristic, depth: Int, a: Int, b: Int, game: Game): Int = {
    if(gameOver(game)) countPieces(game._1, Black) - countPieces(game._1, White)
    else if(depth == 0) heuristic(game)
    else if(validMoves(game._1,game._2)==Nil) alphabetaEval(heuristic,depth,a,b, (game._1,opponent(game._2))) //ここのa,bはテキトー
    else {
      val nextPos = validMoves(game._1,game._2)
      game._2 match {
        case Black =>{
          var v = Int.MinValue 
          var alpha = a 
          for(c <- nextPos) {
            v = max(v,alphabetaEval(heuristic,depth-1,alpha,b,applyMove(game._1,game._2,c)))
            alpha = max(alpha ,v)
            if(alpha >= b)  return v
          }
          return v
        }
        case White => {
          var v = Int.MaxValue 
          var beta = b 
          for(c <- nextPos) {
            v = min(v,alphabetaEval(heuristic,depth-1,a,beta,applyMove(game._1,game._2,c)))
            beta = min(beta ,v)
            if(a <= beta)  return v
          }
          return v}}}}
          
  def alphabeta(heuristic: Heuristic, depth: Int): Strategy = {
    
    game =>
    
      val nextMove = validMoves(game._1,game._2)
      val nextMoveTuple = nextMove.map(x => (x,alphabetaEval(heuristic,depth-1,Int.MinValue,Int.MaxValue,applyMove(game._1,game._2,x))))
      val nextMoveTupleSorted = nextMoveTuple.sortWith((xi,xj) => xi._2 >= xj._2 ) 
      if(game._2 == Black) nextMoveTupleSorted(0)._1 else nextMoveTupleSorted.last._1
  }



  def alphabetaRandom(heuristic: Heuristic, depth: Int,ipsilon:Double): Strategy = {
    import scala.util.Random
    val random = Random.nextDouble()
    game =>
      if(random > ipsilon){
        val nextMove = validMoves(game._1,game._2)
        val nextMoveTuple = nextMove.map(x => (x,alphabetaEval(heuristic,depth-1,Int.MinValue,Int.MaxValue,applyMove(game._1,game._2,x))))
        val nextMoveTupleSorted = nextMoveTuple.sortWith((xi,xj) => xi._2 >= xj._2 ) 
    
        if(game._2 == Black )  nextMoveTupleSorted.head._1 else nextMoveTupleSorted.last._1
    }
      else { scala.util.Random.shuffle(validMoves(game._1,game._2)).head
    }
  }
}
  


object OthelloMain extends App {
  import OthelloLib._
  import java.io._
  import scala.io.Source
  import scala.util.Random
  val n = 10
  var fliped :List[List[Position]] = List(Nil)
 
    var v = Array.ofDim[Double](10,64)
    var tmp = Array.ofDim[Double](10,64)
    var ab = Array.ofDim[Double](60,2)
    var abtmp = Array.ofDim[Double](60,2) //正規分布を使って

  def gaussianAdd(array:Array[Array[Double]],mean:Double,sigma:Double):Array[Array[Double]]={
    for(i <- Range(0,60)){
      array(i)(0) = array(i)(0) + Random.nextGaussian()*sigma + mean
      array(i)(1) = array(i)(1) + Random.nextGaussian()*sigma + mean
    }
    array
  }
  
    def boardToList(board:Board):List[Square]={
    val list:List[Square] =Nil
    board.foldRight(list)((x,a)=>  x.foldRight(list)((y,b)=> y::b) ++ a )
}
  def randomMove2:Strategy={
    game =>{
      scala.util.Random.shuffle(validMoves(game._1,game._2)).head

    }


  }

  def weightedCountPiecedef(game: Game,list:Array[Double]): Double = {
    val (board, player) = game
    var x = 0.0
    val listBoard = boardToList(board)
    for(i <- Range(0,64)){
       if( listBoard(i) == player)x = x + list(i)
    }
    x
  }

  //ここから
  def myHeuristic:Heuristic={
    game =>{
      var a = 0.0
      var b = 0.0
      val turn = countPieces(game._1, Black) + countPieces(game._1, White)-4
      for(i <- Range(0,10)){
        if(i<= turn && turn < i*6){
          a = weightedCountPiecedef(game,v(i))-weightedCountPiecedef((game._1,opponent(game._2)),v(i))
        }
      }
      b = validMoves(game._1,game._2).length - validMoves(game._1,opponent(game._2)).length

      1000*(abtmp(turn)(1)*a + abtmp(turn)(1)*b).toInt
    }
}

def posToInt(pos:Position):Int={
  val(x,y)= pos
  x-1+8*(y-1)
}

def arraySum(array1:Array[Array[Double]],array2:Array[Array[Double]],a:Double,b:Double,n:Int,m:Int):Array[Array[Double]]={
  for(i <- Range(0,n)){
    for(j <- Range(0,m)){
      array1(i)(j)= a*array1(i)(j) +b*array2(i)(j)
    }
    
  }
  array1

}

  def playLoop2(game: Game, strat1: Strategy, strat2: Strategy ,learner:Player): Game = {
    val (board, player) = game
    fliped= fliped.reverse
    if (gameOver(game)) {
      for(i <-Range(0,10)){
        for(j <- Range(0,3)){
          if(i+10*j<fliped.length){
          for( k <- fliped(i+10*j).map(x => posToInt(x))){
            tmp(i)(k) = tmp(i)(k) +1
          }
        }
      }}
      var diff = 0
      if(learner == player) { diff = (countPieces(board,Black)-countPieces(board,White))}
      else { diff = (countPieces(board,Black)-countPieces(board,White))*(-1)}
      v = arraySum(v,tmp,1-diff/1000,diff/1000,10,64)
      //a bの調整も
      ab = arraySum(ab,abtmp,1-diff/1000,diff/1000,60,2)

     game
    }
    else {
      if (learner == player) fliped =  whichFlip(board,player,strat1(game))::fliped //要素は３０こ
      val newgame = applyStrategy(game, strat1)
      playLoop2(newgame, strat2, strat1,learner)
    }
    
  }
 
   //書き込み
  val nil:List[Double] = Nil
 
def study(times:Int,opponent:Strategy):Unit={
  for(aaa <- Range(0,times)){
    val src =Source.fromFile("para.txt","UTF-8")
    val lines = src.getLines()
    for (i <- Range(0,n)) {
      v(i) = lines.next() match {
        case line => line.split(",").map(x => x.toDouble).toArray
      }
    }
    src.close()

    val lines2 = Source.fromFile("tmp.txt","UTF-8").getLines()
  for (i <- Range(0,n)) {
      tmp(i) = lines2.next() match {
        case line => line.split(",").map(x => x.toDouble).toArray
      }
    }
   //lines2.close()
    val lines3 = Source.fromFile("ab.txt","UTF-8").getLines()
  for (i <- Range(0,60)) {
      ab(i) = lines3.next() match {
        case line => line.split(",").map(x => x.toDouble).toArray
      }
    }
  //lines3.close()
    val lines4 = Source.fromFile("abtmp.txt","UTF-8").getLines()
  for (i <- Range(0,60)) {
      abtmp(i) = lines4.next() match {
        case line => line.split(",").map(x => x.toDouble).toArray
      }
    }
  //lines4.close()
    gaussianAdd(abtmp,0,0.01)
    //playLoop2(newGame,alphabetaRandom(myHeuristic,4,0.1),opponent,Black)
    playLoop2(newGame,alphabeta(countDiff,4),opponent,Black)

  val list = v.map(x => x.toList).foldRight(nil)((x,z)=> x ++z )
  val list1 = ab.map(x => x.toList).foldRight(nil)((x,z)=> x ++z )

    val pw = new PrintWriter("para.txt")
    for(i<- 0 until 64*n){
      if(i !=0 && i%64==0) pw.write("\n")
      pw.write(list(i).toString + ",")
    }
  pw.close

  val pw2 = new PrintWriter("para.txt")
  for(i<- 0 until 64*n){
      if(i !=0 && i%64==0) pw2.write("\n")
      pw2.write(list(i).toString + ",")
    }
  pw2.close

  val pw3 = new PrintWriter("ab.txt")
    for(i<- 0 until 60*2){
      if(i !=0 && i%2==0) pw3.write("\n")
      pw3.write(list1(i).toString + ",")
    }
  pw3.close
  val pw4 = new PrintWriter("abtmp.txt")
    for(i<- 0 until 60*2){
      if(i !=0 && i%2==0) pw4.write("\n")
      pw4.write(list1(i).toString + ",")
    }
  pw4.close





  }






  println("finished")
}


  study(100,randomMove2)
  // 1つ目の randomMove を自分の戦略に変更
  //playLoop2(newGame, randomMove, randomMove)
}











 
 /*
  var v :Array[IndexedSeq[Double]] = Array.fill(n)(IndexedSeq())
  var tmp :Array[IndexedSeq[Double]] = Array.fill(n)(IndexedSeq())
  
  //読み込み
  val lines = Source.fromFile("para.txt","UTF-8").getLines()
  for (i <- Range(0,n)) {
      v(i) = lines.next() match {
        case "" => IndexedSeq()
        case line => line.split(",").map(x => x.toDouble).toIndexedSeq
      }
    }

    val lines2 = Source.fromFile("tmp.txt","UTF-8").getLines()
  for (i <- Range(0,n)) {
      tmp(i) = lines2.next() match {
        case "" => IndexedSeq()
        case line => line.split(",").map(x => x.toDouble).toIndexedSeq
      }
    }
*/
  
  /*
  val pw = new PrintWriter("para.txt")
    for(i<- 0 until 64*n){
      if(i !=0 && i%64==0) pw.write("\n")
      pw.write(list(i).toString + ",")
    }
  pw.close

  val pw2 = new PrintWriter("para.txt")
  for(i<- 0 until 64*n){
      if(i !=0 && i%64==0) pw2.write("\n")
      pw2.write(list(i).toString + ",")
    }
  pw2.close

  val list1 = List.fill(60*2)(1)
  val pw3 = new PrintWriter("ab.txt")
    for(i<- 0 until 60*2){
      if(i !=0 && i%2==0) pw3.write("\n")
      pw3.write(list1(i).toString + ",")
    }
  pw3.close
  val pw4 = new PrintWriter("abtmp.txt")
    for(i<- 0 until 60*2){
      if(i !=0 && i%2==0) pw4.write("\n")
      pw4.write(list1(i).toString + ",")
    }
  pw4.close
*/

/*
    val lines = Source.fromFile("para.txt","UTF-8").getLines()
    for (i <- Range(0,n)) {
      v(i) = lines.next() match {
        case line => line.split(",").map(x => x.toDouble).toArray
      }
    }

    val lines2 = Source.fromFile("tmp.txt","UTF-8").getLines()
  for (i <- Range(0,n)) {
      tmp(i) = lines2.next() match {
        case line => line.split(",").map(x => x.toDouble).toArray
      }
    }
   
    val lines3 = Source.fromFile("ab.txt","UTF-8").getLines()
  for (i <- Range(0,60)) {
      ab(i) = lines3.next() match {
        case line => line.split(",").map(x => x.toDouble).toArray
      }
    }

    val lines4 = Source.fromFile("abtmp.txt","UTF-8").getLines()
  for (i <- Range(0,60)) {
      abtmp(i) = lines4.next() match {
        case line => line.split(",").map(x => x.toDouble).toArray
      }
    }
    */








/*
import scala.util.Random
object  OthelloMain extends App {
  import java.io._
  import scala.io.Source
  val n = 10
  var v :Array[IndexedSeq[Double]] = Array.fill(n)(IndexedSeq())
  var tmp :Array[IndexedSeq[Double]] = Array.fill(n)(IndexedSeq())
  var listList = List(List("q"))
  //読み込み
  val lines = Source.fromFile("para.txt","UTF-8").getLines()
  for (i <- Range(0,n)) {
      v(i) = lines.next() match {
        case "" => IndexedSeq()
        case line => line.split(",").map(x => x.toDouble).toIndexedSeq
      }
    }
  //ここから


    
  
  
  
   
 
   //書き込み
  val list = List.fill(64*n)(0)
  val pw = new PrintWriter("para.txt")
  //for(j <-0 until 60){
    for(i<- 0 until 64*n){
      if(i !=0 && i%64==0) pw.write("\n")
      pw.write(list(i).toString + ",")
    }
  
  pw.close
  






  //policy iteration 
  /*
  def poisson(lambda :Double):Int={
    var n = 0
    val random = Random.nextDouble()
    var exp = Math.exp(-lambda)
    while(random > exp ){
      n = n + 1
      exp = lambda*exp/n + exp
    }
    n
  }
  val parkCapacity = 20
  var v =  Array.ofDim[Double](parkCapacity,parkCapacity+1)
  var v2 =  Array.ofDim[Double](parkCapacity,parkCapacity+1)
  val lambdaReq1 =3
  val lambdaReq2 =4
  val lambdaRetu1 =3
  val lambdaRetu2 =2
  val theta = 0.0001
  var delta = 1.0
  val list0 = List.range(0,parkCapacity+1)
  val gamma = 0.9
  def pie(i:Int,j:Int,k:Int,l:Int):Double = {
    1
  }
  def reward(i:Int,j:Int,k:Int,l:Int):Double={
    1
  }

  while(theta>delta){
    delta = 0
    for(i <- list0){
      for(j <- list0){
        v2(i)(j) = v(i)(j)
        for(k <- list0){
          for(l <- list0){
            v(i)(j)= v(i)(j) + pie(i,j,k,l)*(reward(i,j,k,l) + gamma*v(k)(l))

          }

        }
        delta = (v2(i)(j) - v(i)(j)).abs
      }
    }
  }
*/
 
  /*epsilon greedy algorithm
  val steps = 40
  val arms = 10
  //estimate
  var estimate = Array.fill(arms)(5.0)
  //10 armed
  var meanInit = Array.fill(arms)(Random.nextGaussian()*7 - 5)

  var totalReward = 0.0
  var idealReward =0.0
  val listOfReward = Nil

  //select action   e-greedy method
  def selectAction(e:Double):Int={
    if(Random.nextDouble() < e) Random.nextInt(arms)
    else {
      val max = estimate.max
      estimate.indexOf(max)
    }
  }

  //step size parameter 必要なら作る
  var stepSizePara = 0.1
  //estimate
  def weightedAve(a:Int,r:Double):Array[Double]={
    estimate(a) += stepSizePara*(r-estimate(a))
    return estimate
}

  
  //action -> reward
  def reward(array:Array[Double],a:Int):Double={
    array(a)+ Random.nextGaussian()
  }
  
  // nonstationary
  def sumArray(array1:Array[Double],array2:Array[Double]):Array[Double]={
    for(i <- 0 until array1.length){
      array1(i) += array2(i)
    }
    array1
  }
  
  def change(array:Array[Double]):Array[Double]={
    val arrayChange = Array.fill(arms)(Random.nextGaussian()*0.02)
    sumArray(array,arrayChange)

  }
  import java.io._
  val pw = new PrintWriter("greedy.txt")
  for(i <-0 until steps){
    //行動を決定
    val action = selectAction(0.01)
    //報酬を得る
    totalReward += reward(meanInit,action)
    //nonstationary
    meanInit = change(meanInit)
    //評価を更新
    estimate(action) += 0.1*(1 - Math.pow(0.9,i))*(reward(meanInit,action)-estimate(action))
    pw.write( (totalReward/(i+1)).toString() + " ")
  }
  println(totalReward/steps)
  
  pw.close
*/
  
}
*/
