import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}
import scala.collection.mutable
import java.time.LocalDate
import java.io.{File, FileWriter, PrintWriter}
import java.time.LocalDateTime


sealed trait Json
case class JStr(value: String) extends Json
case class JNum(value: Double) extends Json
case class JBool(value: Boolean) extends Json
case object JNull extends Json
case class JArr(items: List[Json]) extends Json
case class JObj(fields: Map[String, Json]) extends Json

def escapeJson(s: String): String =
  val sb = new StringBuilder
  var i = 0
  while i < s.length do
    val c = s.charAt(i)
    c match
      case '"'  => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case c if c < ' ' => sb.append(f"\\u${c.toInt}%04x")
      case c => sb.append(c)
    i += 1
  sb.toString

def jsonToString(j: Json, indent: Int = 0): String = j match
  case JStr(s)  => "\"" + escapeJson(s) + "\""
  case JNum(n)  => if n == n.toLong.toDouble then n.toLong.toString else n.toString
  case JBool(b) => b.toString
  case JNull    => "null"
  case JArr(items) =>
    if items.isEmpty then "[]"
    else
      val pad = "  " * (indent + 1)
      val closePad = "  " * indent
      items.map(i => pad + jsonToString(i, indent + 1)).mkString("[\n", ",\n", "\n" + closePad + "]")
  case JObj(fields) =>
    if fields.isEmpty then "{}"
    else
      val pad = "  " * (indent + 1)
      val closePad = "  " * indent
      fields.toSeq.map { case (k, v) =>
        pad + "\"" + escapeJson(k) + "\": " + jsonToString(v, indent + 1)
      }.mkString("{\n", ",\n", "\n" + closePad + "}")

class JsonParser(input: String):
  private var pos = 0
  private val s = input

  def parse(): Json =
    skipWs()
    val v = parseValue()
    skipWs()
    if pos < s.length then throw new RuntimeException(s"Лишние символы в JSON, позиция $pos")
    v

  private def skipWs(): Unit =
    while pos < s.length && s.charAt(pos).isWhitespace do pos += 1

  private def parseValue(): Json =
    if pos >= s.length then throw new RuntimeException("Неожиданный конец JSON")
    s.charAt(pos) match
      case '{' => parseObject()
      case '[' => parseArray()
      case '"' => JStr(parseString())
      case 't' =>
        if s.startsWith("true", pos) then { pos += 4; JBool(true) }
        else throw new RuntimeException(s"Ожидалось true в позиции $pos")
      case 'f' =>
        if s.startsWith("false", pos) then { pos += 5; JBool(false) }
        else throw new RuntimeException(s"Ожидалось false в позиции $pos")
      case 'n' =>
        if s.startsWith("null", pos) then { pos += 4; JNull }
        else throw new RuntimeException(s"Ожидалось null в позиции $pos")
      case _ => parseNumber()

  private def parseObject(): Json =
    expect('{')
    skipWs()
    val fields = mutable.LinkedHashMap[String, Json]()
    if pos < s.length && s.charAt(pos) == '}' then
      pos += 1
      JObj(fields.toMap)
    else
      var continue = true
      while continue do
        skipWs()
        val key = parseString()
        skipWs()
        expect(':')
        skipWs()
        val value = parseValue()
        fields(key) = value
        skipWs()
        if pos < s.length && s.charAt(pos) == ',' then pos += 1
        else continue = false
      skipWs()
      expect('}')
      JObj(fields.toMap)

  private def parseArray(): Json =
    expect('[')
    skipWs()
    val items = mutable.ArrayBuffer[Json]()
    if pos < s.length && s.charAt(pos) == ']' then
      pos += 1
      JArr(items.toList)
    else
      var continue = true
      while continue do
        skipWs()
        items += parseValue()
        skipWs()
        if pos < s.length && s.charAt(pos) == ',' then pos += 1
        else continue = false
      skipWs()
      expect(']')
      JArr(items.toList)

  private def parseString(): String =
    expect('"')
    val sb = new StringBuilder
    var done = false
    while !done do
      if pos >= s.length then throw new RuntimeException("Незакрытая строка в JSON")
      val c = s.charAt(pos)
      pos += 1
      c match
        case '"' => done = true
        case '\\' =>
          if pos >= s.length then throw new RuntimeException("Незавершённый escape в JSON")
          val e = s.charAt(pos); pos += 1
          e match
            case '"'  => sb.append('"')
            case '\\' => sb.append('\\')
            case '/'  => sb.append('/')
            case 'n'  => sb.append('\n')
            case 'r'  => sb.append('\r')
            case 't'  => sb.append('\t')
            case 'b'  => sb.append('\b')
            case 'f'  => sb.append('\f')
            case 'u' =>
              if pos + 4 > s.length then throw new RuntimeException("Некорректный \\u escape")
              val hex = s.substring(pos, pos + 4)
              pos += 4
              sb.append(Integer.parseInt(hex, 16).toChar)
            case _ => throw new RuntimeException(s"Неизвестный escape \\$e")
        case c => sb.append(c)
    sb.toString

  private def parseNumber(): Json =
    val start = pos
    if pos < s.length && (s.charAt(pos) == '-' || s.charAt(pos) == '+') then pos += 1
    while pos < s.length && (s.charAt(pos).isDigit || s.charAt(pos) == '.' ||
      s.charAt(pos) == 'e' || s.charAt(pos) == 'E' || s.charAt(pos) == '+' ||
      s.charAt(pos) == '-') do pos += 1
    val token = s.substring(start, pos)
    if token.isEmpty then throw new RuntimeException(s"Ожидалось число в позиции $start")
    JNum(token.toDouble)

  private def expect(c: Char): Unit =
    skipWs()
    if pos >= s.length || s.charAt(pos) != c then
      throw new RuntimeException(s"Ожидался символ '$c' в позиции $pos")
    pos += 1

def jStr(j: Json): String = j match
  case JStr(s) => s
  case _ => throw new RuntimeException("Ожидалась строка")

def jInt(j: Json): Int = j match
  case JNum(n) => n.toInt
  case JStr(s) => s.toInt
  case _ => throw new RuntimeException("Ожидалось число")

def jBool(j: Json): Boolean = j match
  case JBool(b) => b
  case JStr(s)  => s == "true"
  case _ => throw new RuntimeException("Ожидалось логическое значение")

def jArr(j: Json): List[Json] = j match
  case JArr(items) => items
  case _ => throw new RuntimeException("Ожидался массив")

def jField(j: Json, name: String): Json = j match
  case JObj(fields) => fields.getOrElse(name, throw new RuntimeException(s"Нет поля '$name'"))
  case _ => throw new RuntimeException("Ожидался объект")

def jOptField(j: Json, name: String): Option[Json] = j match
  case JObj(fields) => fields.get(name)
  case _ => None


case class PlanningConfig(
  dayStart: String,
  dayEnd: String,
  maximumTaskHoursPerDay: Int,
  minimumTaskBlockMinutes: Int,
  maximumTaskBlockMinutes: Int,
  breakAfterMinutes: Int,
  breakDurationMinutes: Int,
  dailyReserveMinutes: Int,
  deadlineReserveDays: Int)

case class PrioritiesConfig(low: Int, normal: Int, high: Int, critical: Int):
  def value(name: String): Option[Int] = name match
    case "низкий" | "low" => Some(low)
    case "средний" | "normal" => Some(normal)
    case "высокий" | "high" => Some(high)
    case "критический" | "critical" => Some(critical)
    case _ => None

case class RoutineConfig(
  name: String,
  days: Seq[String],
  start: String,
  end: String,
  durationMinutes: Int,
  allowedFrom: String,
  allowedTo: String,
  minimumBlockMinutes: Int)

case class AppConfig(
  logFile: String,
  stateFile: String,
  reportDirectory: String,
  defaultPlanningMode: String,
  planning: PlanningConfig,
  priorities: PrioritiesConfig,
  defaultRoutines: Seq[RoutineConfig])

object AppConfig:
  def default: AppConfig = AppConfig(
    logFile = "planner.log",
    stateFile = "planner-state.json",
    reportDirectory = "reports",
    defaultPlanningMode = "balanced",
    planning = PlanningConfig(
      dayStart = "07:00",
      dayEnd = "23:30",
      maximumTaskHoursPerDay = 6,
      minimumTaskBlockMinutes = 30,
      maximumTaskBlockMinutes = 180,
      breakAfterMinutes = 120,
      breakDurationMinutes = 20,
      dailyReserveMinutes = 60,
      deadlineReserveDays = 1),
    priorities = PrioritiesConfig(low = 10, normal = 20, high = 30, critical = 40),
    defaultRoutines = Seq(
      RoutineConfig(
        name = "Сон",
        days = Seq("monday","tuesday","wednesday","thursday","friday","saturday","sunday"),
        start = "23:30", end = "07:30",
        durationMinutes = 0, allowedFrom = "", allowedTo = "", minimumBlockMinutes = 0),
      RoutineConfig(
        name = "Питание",
        days = Seq("monday","tuesday","wednesday","thursday","friday","saturday","sunday"),
        start = "", end = "",
        durationMinutes = 180, allowedFrom = "07:00", allowedTo = "22:00",
        minimumBlockMinutes = 30)))

object ConfigLoader:
  def load(path: String): Try[AppConfig] = Try {
    val file = new File(path)
    if !file.exists() then
      val deflt = AppConfig.default
      save(deflt, path)
      deflt
    else
      val src = scala.io.Source.fromFile(file, "UTF-8")
      val text = try src.mkString finally src.close()
      val json = new JsonParser(text).parse()
      parseConfig(json)
  }

  def save(cfg: AppConfig, path: String): Try[Unit] = Try {
    val w = new PrintWriter(path, "UTF-8")
    try w.println(jsonToString(configToJson(cfg)))
    finally w.close()
  }

  def parseConfig(j: Json): AppConfig =
    val planning = jField(j, "planning")
    val priorities = jField(j, "priorities")
    val routines = jArr(jField(j, "defaultRoutines")).map(parseRoutine)
    AppConfig(
      logFile = jStr(jField(j, "logFile")),
      stateFile = jStr(jField(j, "stateFile")),
      reportDirectory = jStr(jField(j, "reportDirectory")),
      defaultPlanningMode = jStr(jField(j, "defaultPlanningMode")),
      planning = PlanningConfig(
        dayStart = jStr(jField(planning, "dayStart")),
        dayEnd = jStr(jField(planning, "dayEnd")),
        maximumTaskHoursPerDay = jInt(jField(planning, "maximumTaskHoursPerDay")),
        minimumTaskBlockMinutes = jInt(jField(planning, "minimumTaskBlockMinutes")),
        maximumTaskBlockMinutes = jInt(jField(planning, "maximumTaskBlockMinutes")),
        breakAfterMinutes = jInt(jField(planning, "breakAfterMinutes")),
        breakDurationMinutes = jInt(jField(planning, "breakDurationMinutes")),
        dailyReserveMinutes = jInt(jField(planning, "dailyReserveMinutes")),
        deadlineReserveDays = jInt(jField(planning, "deadlineReserveDays"))),
      priorities = PrioritiesConfig(
        low = jInt(jField(priorities, "low")),
        normal = jInt(jField(priorities, "normal")),
        high = jInt(jField(priorities, "high")),
        critical = jInt(jField(priorities, "critical"))),
      defaultRoutines = routines)

  private def parseRoutine(j: Json): RoutineConfig =
    RoutineConfig(
      name = jStr(jField(j, "name")),
      days = jArr(jField(j, "days")).map(jStr),
      start = jOptField(j, "start").map(jStr).getOrElse(""),
      end = jOptField(j, "end").map(jStr).getOrElse(""),
      durationMinutes = jOptField(j, "durationMinutes").map(jInt).getOrElse(0),
      allowedFrom = jOptField(j, "allowedFrom").map(jStr).getOrElse(""),
      allowedTo = jOptField(j, "allowedTo").map(jStr).getOrElse(""),
      minimumBlockMinutes = jOptField(j, "minimumBlockMinutes").map(jInt).getOrElse(0))

  def configToJson(c: AppConfig): Json =
    JObj(Map(
      "logFile" -> JStr(c.logFile),
      "stateFile" -> JStr(c.stateFile),
      "reportDirectory" -> JStr(c.reportDirectory),
      "defaultPlanningMode" -> JStr(c.defaultPlanningMode),
      "planning" -> JObj(Map(
        "dayStart" -> JStr(c.planning.dayStart),
        "dayEnd" -> JStr(c.planning.dayEnd),
        "maximumTaskHoursPerDay" -> JNum(c.planning.maximumTaskHoursPerDay),
        "minimumTaskBlockMinutes" -> JNum(c.planning.minimumTaskBlockMinutes),
        "maximumTaskBlockMinutes" -> JNum(c.planning.maximumTaskBlockMinutes),
        "breakAfterMinutes" -> JNum(c.planning.breakAfterMinutes),
        "breakDurationMinutes" -> JNum(c.planning.breakDurationMinutes),
        "dailyReserveMinutes" -> JNum(c.planning.dailyReserveMinutes),
        "deadlineReserveDays" -> JNum(c.planning.deadlineReserveDays))),
      "priorities" -> JObj(Map(
        "low" -> JNum(c.priorities.low),
        "normal" -> JNum(c.priorities.normal),
        "high" -> JNum(c.priorities.high),
        "critical" -> JNum(c.priorities.critical))),
      "defaultRoutines" -> JArr(c.defaultRoutines.map(r => JObj(Map(
        "name" -> JStr(r.name),
        "days" -> JArr(r.days.map(JStr(_)).toList),
        "start" -> JStr(r.start),
        "end" -> JStr(r.end),
        "durationMinutes" -> JNum(r.durationMinutes),
        "allowedFrom" -> JStr(r.allowedFrom),
        "allowedTo" -> JStr(r.allowedTo),
        "minimumBlockMinutes" -> JNum(r.minimumBlockMinutes))): Json).toList)
    ))

object Log:
  private var path: String = "planner.log"
  private var silent: Boolean = false

  def configure(p: String): Unit =
    path = if p.isEmpty then "planner.log" else p
    silent = false

  def disable(): Unit = silent = true

  def info(msg: String): Unit =
    if !silent then
      Try {
        val w = new PrintWriter(new FileWriter(path, true))
        try w.println(s"${LocalDateTime.now()} $msg")
        finally w.close()
      }
      ()

  def error(msg: String): Unit = info(s"[ERROR] $msg")

trait MenuNode:
  def show: String

case class MenuLeaf(name: String, action: () => Unit) extends MenuNode:
  override def show: String = name

case class MenuContainer2(name: String, content: Seq[MenuNode]) extends MenuNode:
  override def show: String = name

case object MenuBack extends MenuNode:
  override def show: String = "Назад"

object TaskState:
  val Planned = "запланировано"
  val InProgress = "выполняется"
  val Completed = "завершено"
  val Overdue = "просрочено"
  val Cancelled = "отменено"
  val Unplaced = "не удалось разместить"

object PlanMode:
  val Early = "раннее"
  val Even = "равномерное"
  val Balanced = "сбалансированное"

case class Task(
  id: String,
  name: String,
  description: String,
  hours: Int,
  var done: Int,
  startDate: String,
  deadline: String,
  priority: String,
  divisible: Boolean,
  minBlock: Int,
  var state: String,
  createdAt: Int = 0 ):
  def remaining: Int = hours - done

case class Routine(
  name: String,
  daysOfWeek: Seq[String],
  startTime: String,
  endTime: String,
  hours: Int,
  validFrom: String,
  validTo: String,
  mandatory: Boolean,
  var enabled: Boolean,
  windowFrom: String,
  windowTo: String,
  minBlock: Int)

case class BusyInterval(
  name: String,
  date: String,
  startTime: String,
  endTime: String,
  description: String)

case class TaskBlock(
  taskId: String,
  taskName: String,
  startMin: Int,
  endMin: Int):
  def lengthMin: Int = endMin - startMin
  def hours: Int = lengthMin / 60

case class UnplacedInfo(
   taskId: String,
   taskName: String,
   remainingHours: Int,
   deadline: String,
   availableHours: Int,
   deficitHours: Int,
   reason: String)

class AppState:
  var config: AppConfig = AppConfig.default
  val tasks = mutable.ArrayBuffer[Task]()
  val routines = mutable.ArrayBuffer[Routine]()
  val busy = mutable.ArrayBuffer[BusyInterval]()
  val unavailableDays = mutable.Set[String]()
  var periodStart: String = ""
  var periodEnd: String = ""
  val plan = mutable.LinkedHashMap[String, mutable.ArrayBuffer[String]]()
  val scheduled = mutable.LinkedHashMap[String, mutable.ArrayBuffer[TaskBlock]]()
  val unplaced = mutable.ArrayBuffer[String]()
  val unplacedInfo = mutable.ArrayBuffer[UnplacedInfo]()
  var dailyReserveHours: Int = 1
  var reserveBeforeDeadlineDays: Int = 0
  var planMode: String = PlanMode.Early
  var nextTaskNumber: Int = 1

case class Interval(startMin: Int, endMin: Int):
  def lengthMin: Int = endMin - startMin

case class DailyBudget(
  day: String,
  routineItems: Seq[(String, Int)],
  busyItems: Seq[(String, Int)],
  totalCommittedHours: Int,
  reserveHours: Int,
  freeForTasksHours: Int,
  freeIntervals: Seq[Interval],
  error: String)


def prompt(msg: String): String =
  println(msg)
  val line = StdIn.readLine()
  if line == null then "" else line.trim

def promptInt(msg: String): Int =
  @tailrec
  def loop(): Int =
    prompt(msg).toIntOption match
      case Some(n) => n
      case None =>
        println("Введите целое число")
        loop()
  loop()

def promptYesNo(msg: String): Boolean =
  val s = prompt(msg).toLowerCase
  s == "да" || s == "д" || s == "yes" || s == "y"

def timeToMinutes(t: String): Int =
  val parts = t.split(":")
  if parts.length != 2 then 0
  else parts(0).trim.toIntOption.getOrElse(0) * 60 + parts(1).trim.toIntOption.getOrElse(0)

def minutesToTime(m: Int): String =
  val mm = ((m % (24 * 60)) + 24 * 60) % (24 * 60)
  val h = mm / 60
  val mn = mm % 60
  f"$h%02d:$mn%02d"

def hoursBetween(start: String, end: String): Int =
  if start.isEmpty || end.isEmpty then 0
  else
    var diff = timeToMinutes(end) - timeToMinutes(start)
    if diff < 0 then diff = diff + 24 * 60
    diff / 60

def datesInRange(start: String, end: String): Seq[String] =
  Try {
    var cur = LocalDate.parse(start)
    val last = LocalDate.parse(end)
    val buf = mutable.ArrayBuffer[String]()
    while !cur.isAfter(last) do
      buf += cur.toString
      cur = cur.plusDays(1)
    buf.toSeq
  }.getOrElse(Seq.empty)

def priorityRank(p: String): Int = p match
  case "высокий" | "high" | "критический" | "critical" => 0
  case "средний" | "normal" => 1
  case "низкий" | "low"  => 2
  case _ => 3

def knownPriority(p: String): Boolean =
  Seq("высокий", "средний", "низкий", "критический", "low", "normal", "high", "critical").contains(p)

def routineApplies(r: Routine, day: String): Boolean =
  if !r.enabled then false
  else
    val dow = LocalDate.parse(day).getDayOfWeek.toString
    val dayMatches = r.daysOfWeek.contains(dow)
    val inValid = (r.validFrom.isEmpty || r.validFrom <= day) &&
      (r.validTo.isEmpty   || day <= r.validTo)
    dayMatches && inValid

def routineHoursFor(r: Routine): Int =
  if r.startTime.nonEmpty && r.endTime.nonEmpty then hoursBetween(r.startTime, r.endTime)
  else r.hours

def busyHoursFor(b: BusyInterval): Int =
  hoursBetween(b.startTime, b.endTime)

def refreshStates(s: AppState): Unit =
  val today = LocalDate.now().toString
  s.tasks.foreach { t =>
    if t.state == TaskState.Cancelled then ()
    else if t.done >= t.hours then t.state = TaskState.Completed
    else if t.deadline.nonEmpty && t.deadline < today then t.state = TaskState.Overdue
    else if t.done > 0 then t.state = TaskState.InProgress
    else t.state = TaskState.Planned
  }

def mergeIntervals(intervals: Seq[Interval]): Seq[Interval] =
  if intervals.isEmpty then Seq.empty
  else
    val sorted = intervals.sortBy(i => (i.startMin, i.endMin))
    val result = mutable.ArrayBuffer[Interval]()
    var cur = sorted.head
    for i <- 1 until sorted.length do
      val next = sorted(i)
      if next.startMin <= cur.endMin then
        cur = Interval(cur.startMin, math.max(cur.endMin, next.endMin))
      else
        result += cur
        cur = next
    result += cur
    result.toSeq

def complementIntervals(merged: Seq[Interval], dayStart: Int, dayEnd: Int): Seq[Interval] =
  val result = mutable.ArrayBuffer[Interval]()
  var cur = dayStart
  for iv <- merged do
    val s = math.max(iv.startMin, dayStart)
    val e = math.min(iv.endMin, dayEnd)
    if s > cur then result += Interval(cur, s)
    if e > cur then cur = e
  if cur < dayEnd then result += Interval(cur, dayEnd)
  result.toSeq

def routineIntervalsFor(r: Routine): Seq[Interval] =
  if r.startTime.nonEmpty && r.endTime.nonEmpty then
    val start = timeToMinutes(r.startTime)
    val end = timeToMinutes(r.endTime)
    if start == end then Seq.empty
    else if end > start then Seq(Interval(start, end))
    else Seq(Interval(start, 24 * 60), Interval(0, end))
  else
    val from = if r.windowFrom.nonEmpty then timeToMinutes(r.windowFrom) else 0
    val h = routineHoursFor(r)
    if h <= 0 then Seq.empty
    else Seq(Interval(from, math.min(from + h * 60, 24 * 60)))

def configDayWindow(s: AppState): (Int, Int) =
  val dS = timeToMinutes(s.config.planning.dayStart)
  val dE = timeToMinutes(s.config.planning.dayEnd)
  if dE > dS then (dS, dE) else (0, 24 * 60)

def computeDailyBudget(s: AppState, day: String): DailyBudget =
  val routineItems = mutable.ArrayBuffer[(String, Int)]()
  val busyItems = mutable.ArrayBuffer[(String, Int)]()
  val allIntervals = mutable.ArrayBuffer[Interval]()

  val applicable = s.routines.filter(r => routineApplies(r, day))
  for r <- applicable do
    val hrs = routineHoursFor(r)
    if hrs > 0 then
      routineItems += ((r.name, hrs))
      allIntervals ++= routineIntervalsFor(r)

  for b <- s.busy if b.date == day do
    val hrs = busyHoursFor(b)
    busyItems += ((b.name, hrs))
    val start = timeToMinutes(b.startTime)
    val end = timeToMinutes(b.endTime)
    if end > start then
      allIntervals += Interval(start, end)
    else if end < start then
      allIntervals += Interval(start, 24 * 60)
      allIntervals += Interval(0, end)

  val (dayStartMin, dayEndMin) = configDayWindow(s)
  val merged = mergeIntervals(allIntervals.toSeq)
  val totalCommitted = merged.map(_.lengthMin).sum / 60

  var error = ""
  if totalCommitted > 24 then
    error = s"обязательные занятия занимают $totalCommitted ч (больше 24 ч)"

  val reserve = s.dailyReserveHours
  val windowHours = (dayEndMin - dayStartMin) / 60
  var freeForTasks = windowHours - totalCommitted - reserve
  if freeForTasks < 0 then freeForTasks = 0

  val freeIntervals =
    if error.nonEmpty then Seq.empty
    else complementIntervals(merged, dayStartMin, dayEndMin)

  DailyBudget(day, routineItems.toSeq, busyItems.toSeq, totalCommitted,
    reserve, freeForTasks, freeIntervals, error)

object StateStore:
  def save(s: AppState, path: String): Try[Unit] = Try {
    val json = JObj(Map(
      "tasks" -> JArr(s.tasks.map(taskToJson).toList),
      "routines" -> JArr(s.routines.map(routineToJson).toList),
      "busy" -> JArr(s.busy.map(busyToJson).toList),
      "unavailableDays" -> JArr(s.unavailableDays.toList.map(JStr(_))),
      "periodStart" -> JStr(s.periodStart),
      "periodEnd" -> JStr(s.periodEnd),
      "plan" -> JObj(s.plan.map { case (k, v) =>
        k -> (JArr(v.toList.map(JStr(_))): Json) }.toMap),
      "scheduled" -> JObj(s.scheduled.map { case (k, v) =>
        k -> (JArr(v.toList.map(blockToJson)): Json) }.toMap),
      "unplaced" -> JArr(s.unplaced.toList.map(JStr(_))),
      "unplacedInfo" -> JArr(s.unplacedInfo.toList.map(unplacedToJson)),
      "dailyReserveHours" -> JNum(s.dailyReserveHours),
      "reserveBeforeDeadlineDays" -> JNum(s.reserveBeforeDeadlineDays),
      "planMode" -> JStr(s.planMode),
      "nextTaskNumber" -> JNum(s.nextTaskNumber)
    ))
    val w = new PrintWriter(path, "UTF-8")
    try w.println(jsonToString(json))
    finally w.close()
  }

  def load(s: AppState, path: String): Try[Unit] = Try {
    val file = new File(path)
    if file.exists() then
      val src = scala.io.Source.fromFile(file, "UTF-8")
      val text = try src.mkString finally src.close()
      val json = new JsonParser(text).parse()
      applyState(s, json)
  }

  private def taskToJson(t: Task): Json = JObj(Map(
    "id" -> JStr(t.id),
    "name" -> JStr(t.name),
    "description" -> JStr(t.description),
    "hours" -> JNum(t.hours),
    "done" -> JNum(t.done),
    "startDate" -> JStr(t.startDate),
    "deadline" -> JStr(t.deadline),
    "priority" -> JStr(t.priority),
    "divisible" -> JBool(t.divisible),
    "minBlock" -> JNum(t.minBlock),
    "state" -> JStr(t.state),
    "createdAt" -> JNum(t.createdAt)))

  private def routineToJson(r: Routine): Json = JObj(Map(
    "name" -> JStr(r.name),
    "daysOfWeek" -> JArr(r.daysOfWeek.map(JStr(_)).toList),
    "startTime" -> JStr(r.startTime),
    "endTime" -> JStr(r.endTime),
    "hours" -> JNum(r.hours),
    "validFrom" -> JStr(r.validFrom),
    "validTo" -> JStr(r.validTo),
    "mandatory" -> JBool(r.mandatory),
    "enabled" -> JBool(r.enabled),
    "windowFrom" -> JStr(r.windowFrom),
    "windowTo" -> JStr(r.windowTo),
    "minBlock" -> JNum(r.minBlock)))

  private def busyToJson(b: BusyInterval): Json = JObj(Map(
    "name" -> JStr(b.name),
    "date" -> JStr(b.date),
    "startTime" -> JStr(b.startTime),
    "endTime" -> JStr(b.endTime),
    "description" -> JStr(b.description)))

  private def blockToJson(b: TaskBlock): Json = JObj(Map(
    "taskId" -> JStr(b.taskId),
    "taskName" -> JStr(b.taskName),
    "startMin" -> JNum(b.startMin),
    "endMin" -> JNum(b.endMin)))

  private def unplacedToJson(u: UnplacedInfo): Json = JObj(Map(
    "taskId" -> JStr(u.taskId),
    "taskName" -> JStr(u.taskName),
    "remainingHours" -> JNum(u.remainingHours),
    "deadline" -> JStr(u.deadline),
    "availableHours" -> JNum(u.availableHours),
    "deficitHours" -> JNum(u.deficitHours),
    "reason" -> JStr(u.reason)))

  private def applyState(s: AppState, j: Json): Unit =
    s.tasks.clear()
    for t <- jArr(jField(j, "tasks")) do
      s.tasks += Task(
        id = jStr(jField(t, "id")),
        name = jStr(jField(t, "name")),
        description = jStr(jField(t, "description")),
        hours = jInt(jField(t, "hours")),
        done = jInt(jField(t, "done")),
        startDate = jStr(jField(t, "startDate")),
        deadline = jStr(jField(t, "deadline")),
        priority = jStr(jField(t, "priority")),
        divisible = jBool(jField(t, "divisible")),
        minBlock = jInt(jField(t, "minBlock")),
        state = jStr(jField(t, "state")),
        createdAt = jOptField(t, "createdAt").map(jInt).getOrElse(0))

    s.routines.clear()
    for r <- jArr(jField(j, "routines")) do
      s.routines += Routine(
        name = jStr(jField(r, "name")),
        daysOfWeek = jArr(jField(r, "daysOfWeek")).map(jStr),
        startTime = jStr(jField(r, "startTime")),
        endTime = jStr(jField(r, "endTime")),
        hours = jInt(jField(r, "hours")),
        validFrom = jStr(jField(r, "validFrom")),
        validTo = jStr(jField(r, "validTo")),
        mandatory = jBool(jField(r, "mandatory")),
        enabled = jBool(jField(r, "enabled")),
        windowFrom = jStr(jField(r, "windowFrom")),
        windowTo = jStr(jField(r, "windowTo")),
        minBlock = jInt(jField(r, "minBlock")))

    s.busy.clear()
    for b <- jArr(jField(j, "busy")) do
      s.busy += BusyInterval(
        name = jStr(jField(b, "name")),
        date = jStr(jField(b, "date")),
        startTime = jStr(jField(b, "startTime")),
        endTime = jStr(jField(b, "endTime")),
        description = jStr(jField(b, "description")))

    s.unavailableDays.clear()
    for d <- jArr(jField(j, "unavailableDays")) do s.unavailableDays += jStr(d)

    s.periodStart = jStr(jField(j, "periodStart"))
    s.periodEnd = jStr(jField(j, "periodEnd"))

    s.plan.clear()
    jField(j, "plan") match
      case JObj(fields) =>
        for (k, v) <- fields do
          s.plan(k) = mutable.ArrayBuffer.from(jArr(v).map(jStr))
      case _ => ()

    s.scheduled.clear()
    jField(j, "scheduled") match
      case JObj(fields) =>
        for (k, v) <- fields do
          val buf = mutable.ArrayBuffer[TaskBlock]()
          for blk <- jArr(v) do
            buf += TaskBlock(
              taskId = jStr(jField(blk, "taskId")),
              taskName = jStr(jField(blk, "taskName")),
              startMin = jInt(jField(blk, "startMin")),
              endMin = jInt(jField(blk, "endMin")))
          s.scheduled(k) = buf
      case _ => ()

    s.unplaced.clear()
    for u <- jArr(jField(j, "unplaced")) do s.unplaced += jStr(u)

    s.unplacedInfo.clear()
    for u <- jArr(jField(j, "unplacedInfo")) do
      s.unplacedInfo += UnplacedInfo(
        taskId = jStr(jField(u, "taskId")),
        taskName = jStr(jField(u, "taskName")),
        remainingHours = jInt(jField(u, "remainingHours")),
        deadline = jStr(jField(u, "deadline")),
        availableHours = jInt(jField(u, "availableHours")),
        deficitHours = jInt(jField(u, "deficitHours")),
        reason = jStr(jField(u, "reason")))

    s.dailyReserveHours = jInt(jField(j, "dailyReserveHours"))
    s.reserveBeforeDeadlineDays = jInt(jField(j, "reserveBeforeDeadlineDays"))
    s.planMode = jStr(jField(j, "planMode"))
    s.nextTaskNumber = jOptField(j, "nextTaskNumber").map(jInt).getOrElse(1)


object Validation:
  def validateConfig(c: AppConfig): Seq[String] =
    val errs = mutable.ArrayBuffer[String]()
    if c.planning.dailyReserveMinutes < 0 then
      errs += "резерв имеет отрицательное значение"
    if c.planning.minimumTaskBlockMinutes > c.planning.maximumTaskBlockMinutes then
      errs += "минимальный блок больше максимального"
    if c.planning.maximumTaskHoursPerDay * 60 < c.planning.minimumTaskBlockMinutes then
      errs += "максимальная дневная нагрузка меньше минимального блока"
    if !Seq("balanced", "early", "even", "раннее", "равномерное", "сбалансированное")
      .contains(c.defaultPlanningMode) then
      errs += s"неизвестный режим планирования: ${c.defaultPlanningMode}"

    for
      i <- c.defaultRoutines.indices
      j <- (i + 1) until c.defaultRoutines.length
    do
      val a = c.defaultRoutines(i)
      val b = c.defaultRoutines(j)
      val commonDays = a.days.map(_.toUpperCase).toSet.intersect(b.days.map(_.toUpperCase).toSet)
      if commonDays.nonEmpty &&
        a.start.nonEmpty && a.end.nonEmpty &&
        b.start.nonEmpty && b.end.nonEmpty then
        val aS = timeToMinutes(a.start); val aE = timeToMinutes(a.end)
        val bS = timeToMinutes(b.start); val bE = timeToMinutes(b.end)
        val (aS2, aE2) = if aE > aS then (aS, aE) else (aS, aE + 24 * 60)
        val (bS2, bE2) = if bE > bS then (bS, bE) else (bS, bE + 24 * 60)
        if aS2 < bE2 && bS2 < aE2 then
          errs += s"пересекающиеся обязательные занятия: ${a.name} и ${b.name}"
    errs.toSeq

  def validateState(s: AppState): Seq[String] =
    val errs = mutable.ArrayBuffer[String]()

    for t <- s.tasks do
      if t.hours <= 0 then errs += s"дело ${t.id}: нулевая или отрицательная трудоёмкость"
      if t.done < 0 then errs += s"дело ${t.id}: отрицательное выполненное время"
      if t.done > t.hours then errs += s"дело ${t.id}: выполнено больше, чем трудоёмкость"
      if t.startDate.nonEmpty && t.deadline.nonEmpty && t.deadline < t.startDate then
        errs += s"дело ${t.id}: срок раньше даты начала"
      if !knownPriority(t.priority) then
        errs += s"дело ${t.id}: неизвестный приоритет '${t.priority}'"
      if !t.divisible && t.remaining > 0 && t.minBlock > 0 && t.minBlock > t.remaining then
        errs += s"дело ${t.id}: минимальный блок больше оставшейся трудоёмкости"

    for r <- s.routines if r.enabled do
      if r.startTime.nonEmpty && r.endTime.nonEmpty then
        val st = timeToMinutes(r.startTime)
        val en = timeToMinutes(r.endTime)
        val len = if en > st then en - st else (24 * 60 - st) + en
        if len == 0 then errs += s"занятие ${r.name}: нулевая продолжительность"
        if len > 24 * 60 then errs += s"занятие ${r.name}: длится больше суток"

    val days = Seq("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY")
    for d <- days do
      val active = s.routines.filter(r => r.enabled && r.mandatory && r.daysOfWeek.contains(d)
        && r.startTime.nonEmpty && r.endTime.nonEmpty)
      for i <- active.indices; j <- (i + 1) until active.length do
        val a = active(i); val b = active(j)
        val aS = timeToMinutes(a.startTime); val aE0 = timeToMinutes(a.endTime)
        val bS = timeToMinutes(b.startTime); val bE0 = timeToMinutes(b.endTime)
        val (aS2, aE2) = if aE0 > aS then (aS, aE0) else (aS, aE0 + 24 * 60)
        val (bS2, bE2) = if bE0 > bS then (bS, bE0) else (bS, bE0 + 24 * 60)
        if aS2 < bE2 && bS2 < aE2 then
          errs += s"пересекающиеся обязательные занятия '$d': ${a.name} и ${b.name}"

    for d <- days do
      val total = s.routines.filter(r => r.enabled && r.mandatory && r.daysOfWeek.contains(d))
        .map(routineHoursFor).sum
      if total > 24 then errs += s"суммарная обязательная нагрузка '$d' больше суток ($total ч)"

    errs.toSeq


def weekdayRu(d: LocalDate): String =
  d.getDayOfWeek.toString match
    case "MONDAY"    => "Понедельник"
    case "TUESDAY"   => "Вторник"
    case "WEDNESDAY" => "Среда"
    case "THURSDAY"  => "Четверг"
    case "FRIDAY"    => "Пятница"
    case "SATURDAY"  => "Суббота"
    case "SUNDAY"    => "Воскресенье"
    case _ => d.getDayOfWeek.toString

def formatInterval(start: Int, end: Int): String =
  val s = minutesToTime(start)
  val e = minutesToTime(end)
  s"$s–$e"

def plannedHoursOnDay(s: AppState, day: String): Int =
  s.scheduled.get(day).map(_.map(_.hours).sum).getOrElse(0)

def buildDayDisplay(s: AppState, day: String): Seq[(Int, Int, String)] =
  val display = mutable.ArrayBuffer[(Int, Int, String)]()

  val applicable = s.routines.filter(r => routineApplies(r, day))
  for r <- applicable do
    if r.startTime.nonEmpty && r.endTime.nonEmpty then
      val start = timeToMinutes(r.startTime)
      var end = timeToMinutes(r.endTime)
      if end < start then end += 24 * 60
      display += ((start, end, r.name))

  for b <- s.busy if b.date == day do
    val start = timeToMinutes(b.startTime)
    var end = timeToMinutes(b.endTime)
    if end < start then end += 24 * 60
    display += ((start, end, b.name))

  for blk <- s.scheduled.getOrElse(day, mutable.ArrayBuffer[TaskBlock]()) do
    display += ((blk.startMin, blk.endMin, blk.taskName))

  val (dayStartMin, dayEndMin) = configDayWindow(s)
  val occupied = mutable.ArrayBuffer[Interval]()
  for r <- applicable do occupied ++= routineIntervalsFor(r)
  for b <- s.busy if b.date == day do
    val start = timeToMinutes(b.startTime)
    val end = timeToMinutes(b.endTime)
    if end > start then occupied += Interval(start, end)
    else if end < start then
      occupied += Interval(start, 24 * 60)
      occupied += Interval(0, end)
  for blk <- s.scheduled.getOrElse(day, mutable.ArrayBuffer[TaskBlock]()) do
    occupied += Interval(blk.startMin, blk.endMin)

  val merged = mergeIntervals(occupied.toSeq)
  val free = complementIntervals(merged, dayStartMin, dayEndMin)

  var reserveMin = s.dailyReserveHours * 60
  for iv <- free do
    if reserveMin > 0 then
      val take = math.min(iv.lengthMin, reserveMin)
      if take > 0 then
        display += ((iv.startMin, iv.startMin + take, "Отдых и резерв"))
        reserveMin -= take
      if iv.endMin > iv.startMin + take then
        display += ((iv.startMin + take, iv.endMin, "Свободное время"))
    else
      display += ((iv.startMin, iv.endMin, "Свободное время"))

  display.sortBy(_._1).toSeq


def showTasks(s: AppState): Unit =
  refreshStates(s)
  if s.tasks.isEmpty then println("Список дел пуст")
  else
    s.tasks.foreach { t =>
      println(
        s"${t.id} | ${t.name} | ${t.hours}ч | выполнено ${t.done}ч | " +
          s"с ${t.startDate} по ${t.deadline} | приоритет: ${t.priority} | " +
          s"делится: ${if t.divisible then "да" else "нет"} | мин.блок: ${t.minBlock}ч | " +
          s"состояние: ${t.state}")
      if t.description.nonEmpty then println(s" описание: ${t.description}")
    }

def addTask(s: AppState): Unit =
  val id = prompt("Идентификатор дела:")
  val name = prompt("Название:")
  val description = prompt("Описание:")
  val hours = promptInt("Трудоёмкость (часов):")
  val startDate = prompt("Дата, начиная с которой можно выполнять :")
  val deadline = prompt("Срок завершения:")
  val priority = prompt("Приоритет:")
  val divisible = promptYesNo("Можно делить на части? (да/нет):")
  val minBlock = if divisible then promptInt("Минимальная продолжительность одного блока (часов):") else hours

  if hours <= 0 then
    println("Трудоёмкость должна быть положительной")
    Log.error(s"addTask: некорректная трудоёмкость $hours")
  else if !knownPriority(priority) then
    println(s"Неизвестный приоритет '$priority'")
    Log.error(s"addTask: неизвестный приоритет '$priority'")
  else if deadline.nonEmpty && startDate.nonEmpty && deadline < startDate then
    println("Срок не может быть раньше даты начала")
    Log.error(s"addTask: срок $deadline раньше старта $startDate")
  else if s.tasks.exists(_.id == id) then
    println(s"Дело '$id' уже существует")
  else
    s.tasks += Task(id, name, description, hours, 0, startDate, deadline,
      priority, divisible, minBlock, TaskState.Planned, s.nextTaskNumber)
    s.nextTaskNumber += 1
    Log.info(s"Добавлено дело id=$id hours=$hours priority=$priority")
    println(s"Дело '$id' добавлено")

def editTask(s: AppState): Unit =
  val id = prompt("Идентификатор дела для изменения:")
  s.tasks.find(_.id == id) match
    case None => println(s"'$id' не найдено")
    case Some(t) =>
      val idx = s.tasks.indexOf(t)
      val name = prompt(s"Новое название (${t.name}):")
      val description = prompt(s"Новое описание (${t.description}):")
      val hours = promptInt(s"Трудоёмкость (${t.hours}):")
      val startDate = prompt(s"Дата начала (${t.startDate}):")
      val deadline = prompt(s"Срок (${t.deadline}):")
      val priority = prompt(s"Приоритет (${t.priority}):")
      val divisible = promptYesNo(s"Делить на части? (${if t.divisible then "да" else "нет"}):")
      val minBlock = if divisible then promptInt(s"Мин.блок (${t.minBlock}):") else hours

      if hours <= 0 then
        println("Трудоёмкость должна быть положительной")
        return
      if priority.nonEmpty && !knownPriority(priority) then
        println(s"Неизвестный приоритет '$priority'")
        return

      val newDeadline = if deadline.isEmpty then t.deadline else deadline
      val newStart = if startDate.isEmpty then t.startDate else startDate
      if newDeadline.nonEmpty && newStart.nonEmpty && newDeadline < newStart then
        println("Срок не может быть раньше даты начала")
        return

      s.tasks(idx) = t.copy(
        name = if name.isEmpty then t.name else name,
        description = if description.isEmpty then t.description else description,
        hours = hours,
        startDate = newStart,
        deadline = newDeadline,
        priority = if priority.isEmpty then t.priority else priority,
        divisible = divisible,
        minBlock = minBlock)
      Log.info(s"Изменено дело id=$id")
      println("Дело изменено")

def deleteTask(s: AppState): Unit =
  val id = prompt("Идентификатор дела для удаления:")
  val before = s.tasks.size
  s.tasks.filterInPlace(_.id != id)
  if s.tasks.size == before then
    println(s"Дело '$id' не найдено")
  else
    for (d, blocks) <- s.scheduled do
      blocks.filterInPlace(_.taskId != id)
    for (d, items) <- s.plan do
      s.plan(d) = items.filterNot(_.startsWith(s"${s.tasks.find(_.id == id).map(_.name).getOrElse(id)}:"))
    Log.info(s"Удалено дело id=$id")
    println(s"Дело '$id' удалено")

def cancelTask(s: AppState): Unit =
  val id = prompt("Идентификатор дела для отмены:")
  s.tasks.find(_.id == id) match
    case None => println(s"Дело '$id' не найдено")
    case Some(t) =>
      t.state = TaskState.Cancelled
      Log.info(s"Отменено дело id=$id")
      println(s"Дело '${t.name}' отменено")

def showRoutines(s: AppState): Unit =
  if s.routines.isEmpty then println("Регулярных занятий нет")
  else
    s.routines.foreach { r =>
      val time =
        if r.startTime.nonEmpty && r.endTime.nonEmpty then s"${r.startTime}-${r.endTime}"
        else s"гибкое, окно ${r.windowFrom}-${r.windowTo}, мин.блок ${r.minBlock}ч"
      val period =
        if r.validFrom.isEmpty && r.validTo.isEmpty then "без ограничения"
        else s"${r.validFrom}..${r.validTo}"
      println(
        s"${r.name} | дни: ${r.daysOfWeek.mkString(",")} | ${time} | " +
          s"${routineHoursFor(r)}ч | период: $period | " +
          s"обязательное: ${if r.mandatory then "да" else "нет"} | " +
          s"включено: ${if r.enabled then "да" else "нет"}"
      )
    }

def addRoutine(s: AppState): Unit =
  val name = prompt("Название занятия:")
  val daysStr = prompt("Дни недели через запятую:")
  val days = daysStr.split(",").map(_.trim.toUpperCase).filter(_.nonEmpty).toSeq

  val strict = promptYesNo("Задать строгое время начала/окончания? (да/нет):")
  var startTime = ""
  var endTime = ""
  var hours = 0
  var windowFrom = ""
  var windowTo = ""
  var minBlock = 0

  if strict then
    startTime = prompt("Время начала:")
    endTime = prompt("Время окончания:")
    hours = hoursBetween(startTime, endTime)
  else
    hours  = promptInt("Общая продолжительность (часов):")
    windowFrom = prompt("Допустимое время с:")
    windowTo = prompt("Допустимое время до:")
    minBlock = promptInt("Минимальный отдельный блок (часов):")

  val validFrom = prompt("Период действия с (пусто = без ограничения):")
  val validTo = prompt("Период действия до (пусто = без ограничения):")
  val mandatory = promptYesNo("Обязательное занятие? (да/нет):")

  s.routines += Routine(name, days, startTime, endTime, hours,
    validFrom, validTo, mandatory, true,
    windowFrom, windowTo, minBlock)
  Log.info(s"Добавлено регулярное занятие '$name'")
  println("Регулярное занятие добавлено")

def editRoutine(s: AppState): Unit =
  val name = prompt("Название занятия для изменения:")
  s.routines.find(_.name == name) match
    case None => println(s"Занятие '$name' не найдено")
    case Some(r) =>
      val idx = s.routines.indexOf(r)
      val daysStr = prompt(s"Дни недели (${r.daysOfWeek.mkString(",")}):")
      val days = if daysStr.isEmpty then r.daysOfWeek
      else daysStr.split(",").map(_.trim.toUpperCase).filter(_.nonEmpty).toSeq
      val startTime = prompt(s"Время начала (${r.startTime}):")
      val endTime = prompt(s"Время окончания (${r.endTime}):")
      val hours = if startTime.nonEmpty && endTime.nonEmpty then hoursBetween(startTime, endTime)
      else promptInt(s"Продолжительность (${r.hours}):")
      val validFrom = prompt(s"Период с (${r.validFrom}):")
      val validTo = prompt(s"Период по (${r.validTo}):")
      val mandatory = promptYesNo(s"Обязательное? (${if r.mandatory then "да" else "нет"}):")
      val enabled = promptYesNo(s"Включено? (${if r.enabled then "да" else "нет"}):")

      s.routines(idx) = r.copy(
        daysOfWeek = days,
        startTime = if startTime.isEmpty then r.startTime else startTime,
        endTime = if endTime.isEmpty then r.endTime else endTime,
        hours = hours,
        validFrom = if validFrom.isEmpty then r.validFrom else validFrom,
        validTo = if validTo.isEmpty then r.validTo else validTo,
        mandatory = mandatory,
        enabled = enabled)
      Log.info(s"Изменено регулярное занятие '$name'")
      println("Занятие изменено")

def toggleRoutine(s: AppState): Unit =
  val name = prompt("Название занятия:")
  s.routines.find(_.name == name) match
    case None    => println(s"Занятие '$name' не найдено")
    case Some(r) =>
      r.enabled = !r.enabled
      Log.info(s"Регулярное занятие '$name' -> enabled=${r.enabled}")
      println(s"Занятие '${r.name}' теперь ${if r.enabled then "включено" else "отключено"}")

def initDefaultRoutines(s: AppState): Unit =
  val allDays = Seq("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY")
  if s.config.defaultRoutines.nonEmpty then
    for rc <- s.config.defaultRoutines do
      val days = rc.days.map(_.toUpperCase)
      val st = rc.start
      val en = rc.end
      val hrs = if rc.durationMinutes > 0 then rc.durationMinutes / 60
      else hoursBetween(st, en)
      s.routines += Routine(
        name = rc.name,
        daysOfWeek = if days.nonEmpty then days else allDays,
        startTime = st,
        endTime = en,
        hours = hrs,
        validFrom = "",
        validTo = "",
        mandatory = true,
        enabled = true,
        windowFrom = rc.allowedFrom,
        windowTo = rc.allowedTo,
        minBlock = if rc.minimumBlockMinutes > 0 then rc.minimumBlockMinutes / 60 else 0)
  else
    s.routines += Routine("Сон", allDays, "23:30", "07:30", 8,
      "", "", true, true, "", "", 0)
    s.routines += Routine("Питание", allDays, "", "", 3,
      "", "", true, true, "07:00", "22:00", 1)

def showBusy(s: AppState): Unit =
  if s.busy.isEmpty then println("Разовых занятых интервалов нет")
  else
    s.busy.foreach { b =>
      println(s"${b.date} ${b.startTime}-${b.endTime} | ${b.name} | ${busyHoursFor(b)}ч")
      if b.description.nonEmpty then println(s"    описание: ${b.description}")
    }

def addBusy(s: AppState): Unit =
  val name = prompt("Название события:")
  val date = prompt("Дата:")
  val startTime = prompt("Время начала:")
  val endTime = prompt("Время окончания:")
  val description = prompt("Описание:")

  if startTime.nonEmpty && endTime.nonEmpty && startTime == endTime then
    println("Время начала и окончания совпадают")
    return

  s.busy += BusyInterval(name, date, startTime, endTime, description)
  Log.info(s"Добавлен занятый интервал $date $startTime-$endTime name=$name")
  println("Разовый занятый интервал добавлен")

  if s.plan.contains(date) && s.plan(date).nonEmpty then
    println(s"Внимание: на $date уже есть автоматически размещённые дела.")
    if promptYesNo("Перестроить план с учётом нового интервала? (да/нет):") then
      if s.periodStart.nonEmpty && s.periodEnd.nonEmpty then
        val from = if date < s.periodStart then s.periodStart else date
        if from <= s.periodEnd then buildPlanFrom(s, from)
        else buildPlan(s)
      else
        buildPlan(s)

def setPeriod(s: AppState): Unit =
  s.periodStart = prompt("Начало периода:")
  s.periodEnd   = prompt("Конец периода:")
  Log.info(s"Период: ${s.periodStart} .. ${s.periodEnd}")
  println(s"Период: с ${s.periodStart} по ${s.periodEnd}")

def showReserves(s: AppState): Unit =
  println(s"Ежедневный резерв: ${s.dailyReserveHours} ч")
  println(s"Резерв перед сроком: ${s.reserveBeforeDeadlineDays} дн.")
  println(s"Режим планирования: ${s.planMode}")
  println(s"Планируемая часть дня: ${s.config.planning.dayStart} - ${s.config.planning.dayEnd}")
  println(s"Максимум часов задач в день: ${s.config.planning.maximumTaskHoursPerDay}")
  println(s"Блок: от ${s.config.planning.minimumTaskBlockMinutes} до ${s.config.planning.maximumTaskBlockMinutes} мин")

def setDailyReserve(s: AppState): Unit =
  val n = promptInt(s"Ежедневный резерв (${s.dailyReserveHours}):")
  if n < 0 then println("Резерв не может быть отрицательным")
  else
    s.dailyReserveHours = n
    println(s"Ежедневный резерв: $n ч")

def setReserveBeforeDeadline(s: AppState): Unit =
  val n = promptInt(s"Резерв перед сроком в днях (${s.reserveBeforeDeadlineDays}):")
  if n < 0 then println("Резерв не может быть отрицательным")
  else
    s.reserveBeforeDeadlineDays = n
    println(s"Резерв перед сроком: $n дн.")

def chooseMode(s: AppState): Unit =
  println(s"Текущий режим: ${s.planMode}")
  println(s"1. ${PlanMode.Early}")
  println(s"2. ${PlanMode.Even}")
  val c = prompt("Выбор:")
  if c == "1" then
    s.planMode = PlanMode.Early
    println(s"Режим: ${s.planMode}")
  else if c == "2" then
    s.planMode = PlanMode.Even
    println(s"Режим: ${s.planMode}")
  else
    println("Не понял выбор")

def showDailyBudget(s: AppState): Unit =
  val day = prompt("Дата:")
  val b = computeDailyBudget(s, day)
  println(s"=== $day ===")
  val (dS, dE) = configDayWindow(s)
  println(f"${"Планируемая часть дня"}%-40s ${minutesToTime(dS)} - ${minutesToTime(dE)}")
  for (name, hrs) <- b.routineItems do
    println(f"$name%-40s $hrs ч")
  for (name, hrs) <- b.busyItems do
    println(f"$name%-40s $hrs ч")
  println(f"${"Итого обязательного"}%-40s ${b.totalCommittedHours} ч")
  println(f"${"Резерв"}%-40s ${b.reserveHours} ч")
  if b.error.nonEmpty then
    println(s"ОШИБКА: ${b.error}")
    println("План для этого дня строиться не будет.")
  else
    println(f"${"Свободно для дел"}%-40s ${b.freeForTasksHours} ч")
    println("Свободные интервалы:")
    if b.freeIntervals.isEmpty then
      println("  (нет)")
    else
      for iv <- b.freeIntervals do
        val total = iv.lengthMin
        val h = total / 60
        val m = total % 60
        val lenStr = if m == 0 then s"${h}ч" else s"${h}ч ${m}м"
        println(s"  ${minutesToTime(iv.startMin)} - ${minutesToTime(iv.endMin)} ($lenStr)")

def effectiveStart(t: Task): String =
  if t.startDate.isEmpty then "0000-01-01" else t.startDate

def effectiveDeadline(s: AppState, t: Task): String =
  if t.deadline.isEmpty then "9999-12-31"
  else if s.reserveBeforeDeadlineDays > 0 then
    Try(LocalDate.parse(t.deadline).minusDays(s.reserveBeforeDeadlineDays).toString)
      .getOrElse(t.deadline)
  else t.deadline

def buildPlan(s: AppState): Unit =
  if s.periodStart.isEmpty || s.periodEnd.isEmpty then
    println("Сначала выберите период планирования")
    return

  val errs = Validation.validateState(s)
  if errs.nonEmpty then
    println("Обнаружены ошибки в данных:")
    errs.foreach(e => println(s"  - $e"))
    Log.error(s"Ошибки валидации при построении плана: ${errs.mkString("; ")}")
    return

  s.plan.clear()
  s.scheduled.clear()
  s.unplaced.clear()
  s.unplacedInfo.clear()
  buildPlanFrom(s, s.periodStart)

def buildPlanFrom(s: AppState, fromDay: String): Unit =
  if s.periodStart.isEmpty || s.periodEnd.isEmpty then
    println("Сначала выберите период планирования")
    return

  Log.info(s"Начало построения плана с $fromDay по ${s.periodEnd}, режим ${s.planMode}")

  val days = datesInRange(fromDay, s.periodEnd)
  if days.isEmpty then
    println("Нет дней для планирования")
    return

  for d <- days do
    s.plan.remove(d)
    s.scheduled.remove(d)

  s.unplaced.clear()
  s.unplacedInfo.clear()

  val maxTaskHours = s.config.planning.maximumTaskHoursPerDay

  val freeHoursMap = mutable.Map[String, Int]()
  for d <- days do
    s.plan(d) = mutable.ArrayBuffer[String]()
    s.scheduled(d) = mutable.ArrayBuffer[TaskBlock]()
    if s.unavailableDays.contains(d) then
      freeHoursMap(d) = 0
    else
      val b = computeDailyBudget(s, d)
      if b.error.nonEmpty then
        Log.error(s"День $d пропущен: ${b.error}")
        println(s"День $d пропущен: ${b.error}")
        freeHoursMap(d) = 0
      else
        freeHoursMap(d) = math.min(b.freeForTasksHours, maxTaskHours)

  val totalAvailable = freeHoursMap.values.sum
  Log.info(s"Доступно часов для планирования: $totalAvailable")

  val remaining = mutable.Map[String, Int]()
  for t <- s.tasks do
    if t.state == TaskState.Cancelled then remaining(t.id) = 0
    else remaining(t.id) = math.max(0, t.hours - t.done)

  val activeTasks = s.tasks.filter(t => t.state != TaskState.Cancelled && remaining(t.id) > 0)

  val useFullDeadline = mutable.Set[String]()
  if s.reserveBeforeDeadlineDays > 0 then
    for t <- activeTasks do
      val effD = effectiveDeadline(s, t)
      val availWithRes = days.filter(d => d >= effectiveStart(t) && d <= effD)
        .map(d => freeHoursMap.getOrElse(d, 0)).sum
      if availWithRes < remaining(t.id) then
        val availNoRes = days.filter(d => d >= effectiveStart(t) && d <= t.deadline)
          .map(d => freeHoursMap.getOrElse(d, 0)).sum
        if availNoRes >= remaining(t.id) then
          println(s"Дело «${t.name}» невозможно завершить с резервом в ${s.reserveBeforeDeadlineDays} дн.")
          println(s"Без учёта резерва дело можно завершить к ${t.deadline}.")
          if promptYesNo(s"Разрешить использовать резерв для дела «${t.name}»? (да/нет):") then
            useFullDeadline += t.id

  def finalDeadline(t: Task): String =
    if useFullDeadline.contains(t.id) then
      if t.deadline.isEmpty then "9999-12-31" else t.deadline
    else effectiveDeadline(s, t)

  def slackOf(t: Task): Int =
    val effD = finalDeadline(t)
    var avail = 0
    for d <- days do
      if d >= effectiveStart(t) && d <= effD then
        avail += freeHoursMap.getOrElse(d, 0)
    avail - remaining(t.id)

  val sorted = activeTasks.sortBy { t =>
    (finalDeadline(t), priorityRank(t.priority), slackOf(t), t.createdAt)
  }

  val dayFree = mutable.Map[String, Int]()
  for d <- days do dayFree(d) = freeHoursMap.getOrElse(d, 0)

  val freeIntervalsMap = mutable.Map[String, mutable.ArrayBuffer[Interval]]()
  for d <- days do
    if s.unavailableDays.contains(d) then
      freeIntervalsMap(d) = mutable.ArrayBuffer[Interval]()
    else
      val b = computeDailyBudget(s, d)
      if b.error.nonEmpty then
        freeIntervalsMap(d) = mutable.ArrayBuffer[Interval]()
      else
        freeIntervalsMap(d) = mutable.ArrayBuffer.from(b.freeIntervals)

  val minBlockConfig = s.config.planning.minimumTaskBlockMinutes
  val maxBlockConfig = s.config.planning.maximumTaskBlockMinutes

  def allocateInDay(day: String, t: Task, wantHours: Int): Int =
    var remainingMin = wantHours * 60
    var allocatedMin = 0
    val intervals = freeIntervalsMap(day)
    var i = 0
    while i < intervals.length && remainingMin > 0 do
      val iv = intervals(i)
      var canMin = math.min(iv.lengthMin, remainingMin)
      canMin = math.min(canMin, maxBlockConfig)
      canMin = (canMin / 60) * 60
      val minBlockMin = math.max(t.minBlock * 60, minBlockConfig)

      if canMin > 0 then
        if !t.divisible && canMin < remainingMin then
          canMin = 0
        else if t.divisible && canMin < minBlockMin && canMin < remainingMin then
          canMin = 0

      if canMin > 0 then
        val start = iv.startMin
        val end = start + canMin
        s.scheduled(day) += TaskBlock(t.id, t.name, start, end)
        s.plan(day) += s"${t.name}: ${canMin / 60}ч (${minutesToTime(start)}–${minutesToTime(end)})"
        allocatedMin += canMin
        remainingMin -= canMin

        if canMin >= iv.lengthMin then
          intervals.remove(i)
        else
          intervals(i) = Interval(iv.startMin + canMin, iv.endMin)
          i += 1
      else
        i += 1
    allocatedMin / 60

  val isEven = s.planMode == PlanMode.Even

  for t <- sorted do
    val effD = finalDeadline(t)
    val effS = effectiveStart(t)
    for d <- days do
      if remaining(t.id) > 0 && d >= effS && d <= effD && dayFree(d) > 0 then
        if t.divisible then
          var want = remaining(t.id)
          if isEven then
            var remDays = 0
            for dd <- days do
              if dd >= d && dd <= effD then remDays += 1
            if remDays > 0 then
              val per = math.ceil(remaining(t.id).toDouble / remDays).toInt
              if per < want then want = per
          var canTake = math.min(dayFree(d), want)
          if canTake < t.minBlock && canTake < remaining(t.id) then
            canTake = 0
          if canTake > 0 then
            val actually = allocateInDay(d, t, canTake)
            if actually > 0 then
              remaining(t.id) -= actually
              dayFree(d) -= actually
        else
          if dayFree(d) >= remaining(t.id) then
            val actually = allocateInDay(d, t, remaining(t.id))
            if actually == remaining(t.id) then
              dayFree(d) -= actually
              remaining(t.id) = 0

  for t <- s.tasks do
    if t.state != TaskState.Cancelled && remaining(t.id) > 0 then
      s.unplaced += s"${t.name} (${remaining(t.id)}ч)"
      val effD = finalDeadline(t)
      val avail = days.filter(d => d >= effectiveStart(t) && d <= effD)
        .map(d => dayFree.getOrElse(d, 0)).sum
      val deficit = math.max(0, remaining(t.id) - avail)
      val reason =
        if s.unavailableDays.nonEmpty then "есть недоступные дни"
        else if s.busy.exists(b => b.date >= fromDay && b.date <= effD) then "занятые интервалы"
        else "недостаточно свободного времени"
      s.unplacedInfo += UnplacedInfo(t.id, t.name, remaining(t.id), effD, avail, deficit, reason)
      t.state = TaskState.Unplaced
      Log.info(s"Дело id=${t.id} не размещено: остаток ${remaining(t.id)}ч, дефицит $deficit ч")

  val distributed = s.scheduled.values.map(_.map(_.hours).sum).sum
  Log.info(s"Распределено часов: $distributed из $totalAvailable")
  if s.unplacedInfo.nonEmpty then
    Log.info(s"Обнаружена перегрузка: ${s.unplacedInfo.size} дел(а) не размещено")

  refreshStates(s)
  println("План построен")

def rebuildPlan(s: AppState): Unit =
  Log.info("Перестроение плана")
  buildPlan(s)

def viewDay(s: AppState): Unit =
  val day = prompt("Дата:")
  val b = computeDailyBudget(s, day)
  println(s"План на $day")
  if b.error.nonEmpty then
    println(s"ОШИБКА: ${b.error}")
  else
    val items = buildDayDisplay(s, day)
    if items.isEmpty then
      println("(пусто)")
    else
      items.foreach { case (st, en, label) =>
        println(f"${formatInterval(st, en)}%-13s $label")
      }

    val planned = plannedHoursOnDay(s, day)
    val freeUnplanned = math.max(0, b.freeForTasksHours - planned)
    println()
    println(s"Плановая загрузка: $planned ч")
    println(s"Обязательные занятия: ${b.totalCommittedHours} ч")
    println(s"Резерв: ${b.reserveHours} ч")
    println(s"Свободное незапланированное время: $freeUnplanned ч")

def viewWeek(s: AppState): Unit =
  Try(LocalDate.parse(prompt("Начало недели:"))).toOption match
    case None => println("Некорректная дата")
    case Some(start) =>
      for i <- 0 until 7 do
        val date = start.plusDays(i)
        val d = date.toString
        val b = computeDailyBudget(s, d)
        val planned = plannedHoursOnDay(s, d)
        val avail = b.freeForTasksHours
        val filled =
          if avail <= 0 then 0
          else math.min(10, math.round(planned.toDouble / avail * 10).toInt)
        val bar = "█" * filled + "░" * (10 - filled)
        println(f"${weekdayRu(date)}%-12s $bar  $planned из $avail доступных часов")
        println(f"    обязательные: ${b.totalCommittedHours}ч, резерв: ${b.reserveHours}ч, свободно: ${math.max(0, avail - planned)}ч")

def viewAll(s: AppState): Unit =
  if s.plan.isEmpty then println("Плана нет")
  else
    s.plan.toSeq.sortBy(_._1).foreach { case (d, _) =>
      println(s"--- $d ---")
      val items = buildDayDisplay(s, d)
      if items.isEmpty then println("(пусто)")
      else
        items.foreach { case (st, en, label) =>
          println(f"${formatInterval(st, en)}%-13s $label")
        }
    }

def viewTaskSchedule(s: AppState): Unit =
  val id = prompt("Идентификатор дела:")
  s.tasks.find(_.id == id) match
    case None => println(s"Дело '$id' не найдено")
    case Some(t) =>
      println(s"Дело: ${t.name}")
      println(s"Трудоёмкость: ${t.hours}ч, выполнено: ${t.done}ч, осталось: ${t.remaining}ч")
      println(s"Срок: ${t.deadline}, приоритет: ${t.priority}, состояние: ${t.state}")
      var any = false
      for (d, blocks) <- s.scheduled.toSeq.sortBy(_._1) do
        for b <- blocks if b.taskId == id do
          any = true
          println(s"$d ${minutesToTime(b.startMin)}–${minutesToTime(b.endMin)} (${b.hours}ч)")
      if !any then println("Дело не размещено в плане")

def markProgress(s: AppState): Unit =
  val id = prompt("Идентификатор дела:")
  val delta = promptInt("Сколько часов выполнено (прибавить, можно отрицательное):")
  s.tasks.find(_.id == id) match
    case None => println(s"Дело '$id' не найдено")
    case Some(t) =>
      val oldDone = t.done
      t.done = math.max(0, math.min(t.hours, t.done + delta))
      if t.done >= t.hours then t.state = TaskState.Completed
      else if t.done > 0 then t.state = TaskState.InProgress
      else t.state = TaskState.Planned

      Log.info(s"Отмечен прогресс дела id=$id: было $oldDone, стало ${t.done}")

      println(s"Дело: ${t.name}")
      println(s"Было выполнено: $oldDone ч")
      println(s"Добавлено: $delta ч")
      println(s"Всего выполнено: ${t.done} ч")
      println(s"Осталось: ${t.remaining} ч")

      val today = LocalDate.now().toString
      val from = if s.periodStart.nonEmpty && today < s.periodStart then s.periodStart else today
      if s.periodEnd.nonEmpty && from <= s.periodEnd then
        println("Пересчёт текущей и будущей части плана...")
        buildPlanFrom(s, from)
      else
        println("Период планирования не задан или уже прошёл, план не пересчитан")

def markUnavailable(s: AppState): Unit =
  val day = prompt("Недоступная дата:")
  s.unavailableDays += day

  val blocks = s.scheduled.getOrElse(day, mutable.ArrayBuffer[TaskBlock]())
  val count = blocks.size
  val hours = blocks.map(_.hours).sum

  println(s"$day отмечено как недоступное.")
  println(s"Отменено плановых блоков: $count")
  println(s"Возвращено в очередь: $hours часов")
  Log.info(s"День $day отмечен как недоступный, снято блоков: $count, часов: $hours")

  s.plan.remove(day)
  s.scheduled.remove(day)

  if promptYesNo("Перестроить план начиная с этой даты? (да/нет):") then
    if s.periodStart.nonEmpty && s.periodEnd.nonEmpty then
      val from = if day < s.periodStart then s.periodStart else day
      if from <= s.periodEnd then buildPlanFrom(s, from)
      else println("Дата вне периода планирования")
    else
      println("Период планирования не задан")
  else
    println("Для продолжения необходимо перестроить план.")

def showOverload(s: AppState): Unit =
  println("Невмещённые дела")
  if s.unplacedInfo.isEmpty then
    println("(нет)")
  else
    for u <- s.unplacedInfo do
      println(
        s"${u.taskName} — осталось ${u.remainingHours}ч, срок ${u.deadline}, " +
          s"доступно ${u.availableHours}ч, дефицит ${u.deficitHours}ч (${u.reason})"
      )

  val totalRemaining = s.unplacedInfo.map(_.remainingHours).sum
  val totalDeficit = s.unplacedInfo.map(_.deficitHours).sum
  if totalRemaining > 0 then
    println(s"Итого не размещено: $totalRemaining ч, дефицит: $totalDeficit ч")

  println("Дни с высокой нагрузкой")
  var any = false
  for (d, _) <- s.plan.toSeq.sortBy(_._1) do
    val b = computeDailyBudget(s, d)
    val planned = plannedHoursOnDay(s, d)
    if b.error.nonEmpty then
      println(s"$d: ${b.error}")
      any = true
    else if b.freeForTasksHours > 0 && planned >= b.freeForTasksHours then
      println(s"$d: загружено $planned из ${b.freeForTasksHours} доступных ч")
      any = true
  if !any then println("(нет)")

  println("Возможные способы устранения перегрузки:")
  println("  - перенести срок")
  println("  - уменьшить трудоёмкость")
  println("  - разрешить использовать резерв")
  println("  - увеличить допустимую дневную нагрузку")
  println("  - отменить или сократить другое дело")
  println("  - освободить занятый интервал")

def monthRu(m: Int): String = m match
  case 1 => "января"; case 2 => "февраля"; case 3 => "марта"
  case 4 => "апреля"; case 5 => "мая"; case 6 => "июня"
  case 7 => "июля"; case 8 => "августа"; case 9 => "сентября"
  case 10 => "октября"; case 11 => "ноября"; case 12 => "декабря"
  case _ => ""

def formatDateRu(d: String): String =
  Try {
    val ld = LocalDate.parse(d)
    s"${ld.getDayOfMonth} ${monthRu(ld.getMonthValue)} ${ld.getYear}"
  }.getOrElse(d)

def buildReport(s: AppState): String =
  val sb = new StringBuilder
  sb.append("=== СВОДКА ===\n")
  if s.periodStart.nonEmpty && s.periodEnd.nonEmpty then
    sb.append(s"Период: ${formatDateRu(s.periodStart)} – ${formatDateRu(s.periodEnd)}\n")
  else
    sb.append("Период не задан\n")
  sb.append("\n")

  val totalHours = s.tasks.filter(_.state != TaskState.Cancelled).map(_.hours).sum
  val doneHours = s.tasks.filter(_.state != TaskState.Cancelled).map(_.done).sum
  val remHours = math.max(0, totalHours - doneHours)

  sb.append(s"Дел: ${s.tasks.count(_.state != TaskState.Cancelled)}\n")
  sb.append(s"Общая трудоёмкость: $totalHours ч\n")
  sb.append(s"Уже выполнено: $doneHours ч\n")
  sb.append(s"Осталось: $remHours ч\n")

  var availableHours = 0
  if s.periodStart.nonEmpty && s.periodEnd.nonEmpty then
    for d <- datesInRange(s.periodStart, s.periodEnd) do
      val b = computeDailyBudget(s, d)
      if b.error.isEmpty && !s.unavailableDays.contains(d) then
        availableHours += math.min(b.freeForTasksHours, s.config.planning.maximumTaskHoursPerDay)
  sb.append(s"Доступно для планирования: $availableHours ч\n")
  sb.append(s"Резерв: ${s.dailyReserveHours * datesInRange(s.periodStart, s.periodEnd).size} ч\n")
  sb.append("\n")

  val plannedTaskIds = s.scheduled.values.flatten.map(_.taskId).toSet
  val activeTasks = s.tasks.filter(_.state != TaskState.Cancelled)
  val fully = activeTasks.count(t => t.remaining <= 0 || s.scheduled.values.flatten.filter(_.taskId == t.id).map(_.hours).sum >= t.remaining)
  val partially = activeTasks.count { t =>
    val placedH = s.scheduled.values.flatten.filter(_.taskId == t.id).map(_.hours).sum
    placedH > 0 && placedH < t.remaining
  }
  val overdue = activeTasks.count(_.state == TaskState.Overdue)

  sb.append(s"Полностью размещено дел: $fully\n")
  sb.append(s"Частично размещено: $partially\n")
  sb.append(s"Просрочено: $overdue\n")
  sb.append("\n")

  sb.append("=== СПИСОК ДЕЛ ===\n")
  for t <- s.tasks do
    sb.append(s"${t.id} | ${t.name} | ${t.hours}ч | выполнено ${t.done}ч | " +
      s"осталось ${t.remaining}ч | срок ${t.deadline} | приоритет ${t.priority} | состояние ${t.state}\n")
  sb.append("\n")

  sb.append("=== РАСПРЕДЕЛЕНИЕ ПО ДНЯМ ===\n")
  for (d, blocks) <- s.scheduled.toSeq.sortBy(_._1) do
    val hrs = blocks.map(_.hours).sum
    sb.append(s"$d: $hrs ч (блоков ${blocks.size})\n")
  sb.append("\n")

  sb.append("=== ДНИ С МАКСИМАЛЬНОЙ НАГРУЗКОЙ ===\n")
  val loadList = s.scheduled.toSeq.map { case (d, blks) => (d, blks.map(_.hours).sum) }
    .sortBy(-_._2).take(5)
  if loadList.isEmpty then sb.append("(нет)\n")
  else
    for (d, h) <- loadList do sb.append(s"$d: $h ч\n")
  sb.append("\n")

  sb.append("=== НЕВМЕЩЁННЫЕ ДЕЛА ===\n")
  if s.unplacedInfo.isEmpty then sb.append("(нет)\n")
  else
    for u <- s.unplacedInfo do
      sb.append(s"${u.taskName}: осталось ${u.remainingHours}ч, " +
        s"срок ${u.deadline}, доступно ${u.availableHours}ч, " +
        s"дефицит ${u.deficitHours}ч (${u.reason})\n")
  sb.append("\n")

  sb.append("=== РЕЖИМ И ОГРАНИЧЕНИЯ ===\n")
  sb.append(s"Режим планирования: ${s.planMode}\n")
  sb.append(s"Начало дня: ${s.config.planning.dayStart}\n")
  sb.append(s"Окончание дня: ${s.config.planning.dayEnd}\n")
  sb.append(s"Максимум задач в день: ${s.config.planning.maximumTaskHoursPerDay} ч\n")
  sb.append(s"Минимальный блок: ${s.config.planning.minimumTaskBlockMinutes} мин\n")
  sb.append(s"Максимальный блок: ${s.config.planning.maximumTaskBlockMinutes} мин\n")
  sb.append(s"Перерыв после: ${s.config.planning.breakAfterMinutes} мин\n")
  sb.append(s"Длительность перерыва: ${s.config.planning.breakDurationMinutes} мин\n")
  sb.append(s"Ежедневный резерв: ${s.dailyReserveHours} ч\n")
  sb.append(s"Резерв перед сроком: ${s.reserveBeforeDeadlineDays} дн.\n")
  sb.append("\n")

  sb.append("=== РЕГУЛЯРНЫЕ ЗАНЯТИЯ ===\n")
  for r <- s.routines do
    sb.append(s"${r.name} | ${r.daysOfWeek.mkString(",")} | " +
      s"${r.startTime}-${r.endTime} | ${routineHoursFor(r)}ч | " +
      s"включено: ${r.enabled}\n")
  sb.append("\n")

  sb.append("=== РАЗОВЫЕ ЗАНЯТЫЕ ИНТЕРВАЛЫ ===\n")
  if s.busy.isEmpty then sb.append("(нет)\n")
  else
    for b <- s.busy do
      sb.append(s"${b.date} ${b.startTime}-${b.endTime} | ${b.name} | ${b.description}\n")

  sb.toString

def saveReport(s: AppState): Unit =
  val name = prompt("Имя файла отчёта:")
  val dir = new File(s.config.reportDirectory)
  if !dir.exists() then dir.mkdirs()
  val file = new File(dir, name)

  Try {
    val w = new PrintWriter(file, "UTF-8")
    try w.println(buildReport(s))
    finally w.close()
    Log.info(s"Отчёт сохранён: ${file.getPath}")
    println(s"Отчёт сохранён в ${file.getPath}")
  }.recover { case e =>
    Log.error(s"Не удалось сохранить отчёт: ${e.getMessage}")
    println(s"Ошибка: ${e.getMessage}")
  }


def applyConfig(s: AppState, cfg: AppConfig): Unit =
  s.config = cfg
  s.dailyReserveHours = cfg.planning.dailyReserveMinutes / 60
  s.reserveBeforeDeadlineDays = cfg.planning.deadlineReserveDays
  s.planMode = cfg.defaultPlanningMode match
    case "balanced" | "сбалансированное" => PlanMode.Even
    case "early" | "раннее"               => PlanMode.Early
    case "even" | "равномерное"           => PlanMode.Even
    case _ => PlanMode.Early

def saveStateToFile(s: AppState): Unit =
  StateStore.save(s, s.config.stateFile) match
    case Success(_) =>
      Log.info(s"Состояние сохранено в ${s.config.stateFile}")
      println(s"Состояние сохранено в ${s.config.stateFile}")
    case Failure(e) =>
      Log.error(s"Не удалось сохранить состояние: ${e.getMessage}")
      println(s"Не удалось сохранить состояние: ${e.getMessage}")

def loadConfigFromFile(s: AppState): Unit =
  val path = "config.json"
  ConfigLoader.load(path) match
    case Success(cfg) =>
      val errs = Validation.validateConfig(cfg)
      if errs.nonEmpty then
        println("Новая конфигурация некорректна, используется предыдущая:")
        errs.foreach(e => println(s"  - $e"))
        Log.error(s"Некорректная конфигурация: ${errs.mkString("; ")}")
      else
        applyConfig(s, cfg)
        Log.configure(cfg.logFile)
        Log.info(s"Конфигурация загружена из $path")
        println(s"Конфигурация загружена из $path")
    case Failure(e) =>
      println(s"Не удалось загрузить конфигурацию: ${e.getMessage}")
      println("Используется последняя корректная версия.")
      Log.error(s"Ошибка загрузки конфигурации: ${e.getMessage}")

def resetState(s: AppState): Unit =
  s.tasks.clear()
  s.routines.clear()
  s.busy.clear()
  s.unavailableDays.clear()
  s.plan.clear()
  s.scheduled.clear()
  s.unplaced.clear()
  s.unplacedInfo.clear()
  s.periodStart = ""
  s.periodEnd = ""
  s.dailyReserveHours = s.config.planning.dailyReserveMinutes / 60
  s.reserveBeforeDeadlineDays = s.config.planning.deadlineReserveDays
  s.planMode = PlanMode.Early
  s.nextTaskNumber = 1
  initDefaultRoutines(s)
  println("Состояние сброшено, применён шаблон из конфигурации")

@tailrec
def menuLoop(menuContainer: MenuContainer2, prevs: Seq[MenuContainer2]): Unit =
  val baseContent = menuContainer.content
  val content =
    if prevs.isEmpty then baseContent
    else baseContent :+ MenuBack
  content.zipWithIndex.map { case (node, idx) => s"$idx. ${node.show}" }.foreach(println)

  @tailrec
  def readChoice(max: Int): Int =
    Option(StdIn.readLine()).flatMap(_.trim.toIntOption) match
      case Some(value) if value >= 0 && value < max => value
      case _ =>
        println(s"Введите число от 0 до ${max - 1}")
        readChoice(max)

  val choice = readChoice(content.length)
  content(choice) match
    case MenuBack =>
      menuLoop(prevs.last, prevs.dropRight(1))
    case mc: MenuContainer2 =>
      menuLoop(mc, prevs.appended(menuContainer))
    case MenuLeaf(_, action) =>
      action()
      menuLoop(menuContainer, prevs)







@main
def MenuDemo2(): Unit =
  val state = new AppState

  ConfigLoader.load("config.json") match
    case Success(cfg) =>
      val cfgErrs = Validation.validateConfig(cfg)
      if cfgErrs.nonEmpty then
        println("Конфигурация некорректна, применяется базовый шаблон:")
        cfgErrs.foreach(e => println(s"  - $e"))
        applyConfig(state, AppConfig.default)
        Log.configure(AppConfig.default.logFile)
        Log.error(s"Некорректная конфигурация: ${cfgErrs.mkString("; ")}")
      else
        applyConfig(state, cfg)
        Log.configure(cfg.logFile)
        Log.info(s"Конфигурация загружена: ${cfg.logFile}")
    case Failure(e) =>
      println(s"Не удалось загрузить конфигурацию: ${e.getMessage}")
      println("Используется базовый шаблон.")
      applyConfig(state, AppConfig.default)
      Log.configure(AppConfig.default.logFile)
      Log.error(s"Ошибка загрузки конфигурации: ${e.getMessage}")

  StateStore.load(state, state.config.stateFile) match
    case Success(_) =>
      Log.info(s"Состояние загружено из ${state.config.stateFile}")
      if state.routines.isEmpty then initDefaultRoutines(state)
    case Failure(e) =>
      println(s"Файл состояния повреждён или недоступен: ${e.getMessage}")
      println("Запуск с чистым состоянием.")
      Log.error(s"Ошибка загрузки состояния: ${e.getMessage}")
      resetState(state)
      if state.routines.isEmpty then initDefaultRoutines(state)


  val tasksMenu = MenuContainer2("Дела", Seq(
    MenuLeaf("Показать дела", () => showTasks(state)),
    MenuLeaf("Добавить дело", () => addTask(state)),
    MenuLeaf("Изменить дело", () => editTask(state)),
    MenuLeaf("Удалить дело", () => deleteTask(state)),
    MenuLeaf("Отменить дело", () => cancelTask(state))
  ))

  val routinesMenu = MenuContainer2("Регулярные занятия", Seq(
    MenuLeaf("Показать", () => showRoutines(state)),
    MenuLeaf("Добавить", () => addRoutine(state)),
    MenuLeaf("Изменить", () => editRoutine(state)),
    MenuLeaf("Включить/отключить", () => toggleRoutine(state))
  ))

  val busyMenu = MenuContainer2("Разовые занятые интервалы", Seq(
    MenuLeaf("Показать", () => showBusy(state)),
    MenuLeaf("Добавить", () => addBusy(state))
  ))

  val planMenu = MenuContainer2("Планирование", Seq(
    MenuLeaf("Выбрать период", () => setPeriod(state)),
    MenuLeaf("Построить план", () => buildPlan(state)),
    MenuLeaf("Перестроить план", () => rebuildPlan(state)),
    MenuLeaf("Показать расчёт дня", () => showDailyBudget(state))
  ))

  val reserveMenu = MenuContainer2("Резервы и режим", Seq(
    MenuLeaf("Показать", () => showReserves(state)),
    MenuLeaf("Ежедневный резерв", () => setDailyReserve(state)),
    MenuLeaf("Резерв перед сроком (дней)", () => setReserveBeforeDeadline(state)),
    MenuLeaf("Режим планирования", () => chooseMode(state))
  ))

  val viewMenu = MenuContainer2("Просмотр плана", Seq(
    MenuLeaf("На день", () => viewDay(state)),
    MenuLeaf("На неделю", () => viewWeek(state)),
    MenuLeaf("На весь период", () => viewAll(state)),
    MenuLeaf("Расписание выбранного дела", () => viewTaskSchedule(state))
  ))

  val mainMenu = MenuContainer2("Главное меню", Seq(
    tasksMenu,
    routinesMenu,
    busyMenu,
    planMenu,
    reserveMenu,
    viewMenu,
    MenuLeaf("Отметить прогресс", () => markProgress(state)),
    MenuLeaf("Отметить недоступный день", () => markUnavailable(state)),
    MenuLeaf("Перегруженные дни / невмещённые дела", () => showOverload(state)),
    MenuLeaf("Сохранить отчёт", () => saveReport(state)),
    MenuLeaf("Сохранить состояние", () => saveStateToFile(state)),
    MenuLeaf("Перезагрузить конфигурацию", () => loadConfigFromFile(state)),
    MenuLeaf("Сбросить состояние", () => resetState(state)),
    MenuLeaf("Выход", () => throw new Exception("exit"))
  ))

  val result = Try(menuLoop(mainMenu, Seq.empty))

  StateStore.save(state, state.config.stateFile) match
    case Success(_) => Log.info("Состояние сохранено при выходе")
    case Failure(e) => Log.error(s"Не удалось сохранить состояние при выходе: ${e.getMessage}")

  Log.info("Завершение программы")

  result match
    case Failure(_) => println("Работа завершена")
    case Success(_) => println("Работа завершена")