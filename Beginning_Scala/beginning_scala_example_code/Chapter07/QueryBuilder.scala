import java.util.Date
import java.sql._
import scala.collection.mutable.ListBuffer

trait BasicColumn[TableType <: Table[TableType], T] {
  def default: T
  def name: String
  def getField(rs: ResultSet): T
  def set(st: PreparedStatement, offset: Int, value: T)
}

trait Table[MyType <: Table[MyType]] extends SuperTuple {
  this: MyType =>

  def table: String

  type ColumnTypes <: Product


  def columns: ColumnTypes with FieldProduct[MyType]

  trait MyColumn[T] extends BasicColumn[MyType, T] {
    def ~[OT](p: MyColumn[OT]): MyTuple2[MyType, T, OT] = {
      val col = this
      new MyTuple2[MyType, T, OT](col.default, p.default) {
        def fields = List(col, p)
        def fieldProduct: (MyColumn[T], MyColumn[OT]) = (col, p)
      }
    }
  }

  case class IntColumn(name: String) extends MyColumn[Int] {
    def default = 0
    def set(st: PreparedStatement, offset: Int, value: Int) {
      st.setInt(offset, value)
    }
    def getField(rs: ResultSet): Int = rs.getInt(name)
  }

  case class LongColumn(name: String) extends MyColumn[Long] {
    def default = 0
    def set(st: PreparedStatement, offset: Int, value: Long) {
      st.setLong(offset, value)
    }
    def getField(rs: ResultSet): Long = rs.getLong(name)
  }

  case class StringColumn(name: String) extends MyColumn[String] {
    def default = ""
    def set(st: PreparedStatement, offset: Int, value: String) {
      st.setString(offset, value)
    }
    def getField(rs: ResultSet): String = rs.getString(name)
  }
  case class DateColumn(name: String) extends MyColumn[Date] {
    def default = new Date(0)
    def set(st: PreparedStatement, offset: Int, value: Date) {
      st.setDate(offset, new java.sql.Date(value.getTime))
    }
    def getField(rs: ResultSet): Date = rs.getDate(name)
  }

  def find[FT <: Product with FieldProduct[MyType]]
  (cols: FT, query: QueryParam[MyType]*): List[FT#ReturnType] = {
    val select = "SELECT "+cols.fields.map(f => table+"."+f.name).mkString(", ")

    val by = query.flatMap{
      case b @ By(_, _) => Some(b)
      case _ => None
    }.toList

    val where = by match {
      case Nil => ""
      case xs => " WHERE "+xs.map(f => table+"."+
                                  f.column.name+" = ?").mkString(" AND ")
    }

    val orderBy = query.flatMap{
      case b @ OrderBy(_, _) => Some(b)
      case _ => None
    }.toList match {
      case Nil => ""
      case xs => " ORDER BY "+xs.map(f => table+"."+
                                     f.column.name+
                                     f.order.sql).mkString(", ")
    }

    using(getJDBCConnection) { conn =>
      prepareStatement(conn, select + where + orderBy) { st =>
        by.zipWithIndex.foreach{case (b, idx) => b.bind(st, idx + 1)}
        executeQuery(st) {
          cols.buildResult _
        }
      }
    }
  }

  protected def getJDBCConnection: Connection

  def using[T <: {def close(): Unit}, R](t: T)(f: T => R): R =
  try {f(t)} finally {t.close()}

  protected def prepareStatement[T](conn: Connection, sql: String)
  (f: PreparedStatement => T): T =
  using(conn.prepareStatement(sql))(f)

  protected def executeQuery[T](st: PreparedStatement)
  (f: ResultSet => T): List[T] =
  using(st.executeQuery){ rs =>
    val ret = new ListBuffer[T]
    while (rs.next) ret += f(rs)
    ret.toList
  }

}

trait QueryParam[TableType <: Table[TableType]]

case class OrderBy[TableType <: Table[TableType]]
(column: BasicColumn[TableType, _], order: SortOrder) extends
QueryParam[TableType]

sealed trait SortOrder {def sql: String}
case object Ascending extends SortOrder {def sql: String = " "}
case object Descending extends SortOrder {def sql = " DESC "}


case class By[TableType <: Table[TableType], T, PT]
(column: BasicColumn[TableType, T], param: PT)(implicit f: PT => T)
extends QueryParam[TableType] {
  def bind(st: PreparedStatement, offset: Int): Unit = {
    column.set(st, offset, param)
  }
}

trait SuperTuple {
  sealed trait FieldProduct[TableType <: Table[TableType]] {
    def fields: List[BasicColumn[TableType, _]]
    def fieldProduct: Product
    def buildResult(rs: ResultSet): ReturnType
    type ReturnType <: Product
  }

  abstract class MyTuple2[TableType <: Table[TableType],
                          A1, A2](a1: A1, a2: A2)
  extends Tuple2[A1, A2](a1, a2) with FieldProduct[TableType] {
    def fieldProduct: (BasicColumn[TableType, A1],
                       BasicColumn[TableType, A2])

    type ReturnType = (A1, A2)

    def buildResult(rs: ResultSet): ReturnType =
    (fieldProduct._1.getField(rs),
     fieldProduct._2.getField(rs))

    def ~[OT](p: BasicColumn[TableType, OT]):
    MyTuple3[TableType, A1, A2, OT] ={
      val f = fields
      val fp = fieldProduct

      new MyTuple3[TableType, A1, A2, OT](this._1, this._2, p.default) {
        val fields = f ::: List(p)
        val fieldProduct = (fp._1, fp._2, p)
      }
    }
  }

  abstract class MyTuple3[TableType <: Table[TableType],
                          A1, A2, A3]
  (a1: A1, a2: A2, a3: A3) extends
  Tuple3[A1, A2, A3](a1, a2, a3) with FieldProduct[TableType] {
    def buildResult(rs: ResultSet): ReturnType =
    (fieldProduct._1.getField(rs),
     fieldProduct._2.getField(rs),
     fieldProduct._3.getField(rs))

    type ReturnType = (A1, A2, A3)

    def fieldProduct: (BasicColumn[TableType, A1],
                       BasicColumn[TableType, A2],
                       BasicColumn[TableType, A3])
    def ~[OT](p: BasicColumn[TableType, OT]):
    MyTuple4[TableType, A1, A2, A3, OT] = {
      val f = fields
      val fp = fieldProduct
      new MyTuple4[TableType, A1, A2, A3,
                   OT](
        this._1, this._2, this._3, p.default) {
        val fields = f ::: List(p)
        val fieldProduct = (fp._1, fp._2, fp._3, p)
      }
    }
  }

  abstract class MyTuple4[TableType <: Table[TableType],
                          A1, A2, A3, A4]
  (a1: A1, a2: A2, a3: A3, a4: A4) extends
  Tuple4[A1, A2, A3, A4](a1, a2, a3, a4) with FieldProduct[TableType] {
    def fieldProduct: (BasicColumn[TableType, A1],
                       BasicColumn[TableType, A2],
                       BasicColumn[TableType, A3],
                       BasicColumn[TableType, A4])

    type ReturnType = (A1, A2, A3, A4)

    def buildResult(rs: ResultSet): ReturnType =
    (fieldProduct._1.getField(rs),
     fieldProduct._2.getField(rs),
     fieldProduct._3.getField(rs),
     fieldProduct._4.getField(rs))
  }
}

trait ConnectionSupplier {
  protected def getJDBCConnection: Connection = null // do something better
}

class MyTable extends Table[MyTable] with ConnectionSupplier {
  val table = "mytable"
  val id = IntColumn("id")
  val name = StringColumn("name")
  val birthday = DateColumn("birthday")

  type ColumnTypes = (Int, String, Date)
  def columns = id ~ name ~ birthday
}

object MyTable extends MyTable
