import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}
import scala.collection.mutable
import java.time.LocalDate
import java.time.LocalDateTime
import java.io.{File, FileWriter, PrintWriter}


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
  case JStr(s) => "\"" + escapeJson(s) + "\""
  case JNum(n) => if n == n.toLong.toDouble then n.toLong.toString else n.toString
  case JBool(b) => b.toString
  case JNull => "null"
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
            case '"' => sb.append('"')
            case '\\' => sb.append('\\')
            case '/' => sb.append('/')
            case 'n' => sb.append('\n')
            case 'r' => sb.append('\r')
            case 't' => sb.append('\t')
            case 'b' => sb.append('\b')
            case 'f' => sb.append('\f')
            case 'u' =>
              if pos + 4 > s.length then throw new RuntimeException("Некорректный \\u escape")
              val hex = s.substring(pos, pos + 4)
              pos += 4
              sb.append(Integer.parseInt(hex, 16).toChar)
            case _ => throw new RuntimeException(s"Неизвестный escape $e")
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
  case JNull => ""
  case _ => throw new RuntimeException("Ожидалась строка")

def jStrOpt(j: Json): Option[String] = j match
  case JStr(s) if s.nonEmpty => Some(s)
  case _ => None

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
  case JObj(fields) => fields.getOrElse(name, JNull)
  case _ => throw new RuntimeException("Ожидался объект")

def jOptField(j: Json, name: String): Option[Json] = j match
  case JObj(fields) => fields.get(name)
  case _ => None

def jDate(j: Json): Option[LocalDate] = jStrOpt(j).flatMap(parseDate)


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
    case "средний" | "normal"  => Some(normal)
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
      dayStart = "07:00", dayEnd = "23:30",
      maximumTaskHoursPerDay = 6,
      minimumTaskBlockMinutes = 30, maximumTaskBlockMinutes = 180,
      breakAfterMinutes = 120, breakDurationMinutes = 20,
      dailyReserveMinutes = 60, deadlineReserveDays = 1),
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
        try w.println(s"${LocalDateTime.now()} $msg") finally w.close()
      }
      ()
  def error(msg: String): Unit = info(s"[ERROR] $msg")

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
                 description: Option[String],
                 hours: Int,
                 var done: Int,
                 startDate: Option[LocalDate],
                 deadline: Option[LocalDate],
                 priority: String,
                 divisible: Boolean,
                 minBlock: Int,
                 var state: String,
                 createdAt: Int = 0):
  def remaining: Int = hours - done

case class Routine(
                    name: String,
                    daysOfWeek: Seq[String],
                    startTime: String,
                    endTime: String,
                    hours: Int,
                    validFrom: Option[LocalDate],
                    validTo: Option[LocalDate],
                    mandatory: Boolean,
                    var enabled: Boolean,
                    windowFrom: String,
                    windowTo: String,
                    minBlock: Int)

case class BusyInterval(
                         name: String,
                         date: LocalDate,
                         startTime: String,
                         endTime: String,
                         description: Option[String])

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
                         deadline: Option[LocalDate],
                         availableHours: Int,
                         deficitHours: Int,
                         reason: String)

case class Interval(startMin: Int, endMin: Int):
  def lengthMin: Int = endMin - startMin

case class DailyBudget(
                        day: LocalDate,
                        routineItems: Seq[(String, Int)],
                        busyItems: Seq[(String, Int)],
                        totalCommittedHours: Int,
                        reserveHours: Int,
                        freeForTasksHours: Int,
                        freeIntervals: Seq[Interval],
                        error: Option[String])

class AppState:
  var config: AppConfig = AppConfig.default
  val tasks = mutable.ArrayBuffer[Task]()
  val routines = mutable.ArrayBuffer[Routine]()
  val busy = mutable.ArrayBuffer[BusyInterval]()
  val unavailableDays = mutable.Set[LocalDate]()
  var periodStart: Option[LocalDate] = None
  var periodEnd: Option[LocalDate] = None
  val plan = mutable.LinkedHashMap[LocalDate, mutable.ArrayBuffer[String]]()
  val scheduled = mutable.LinkedHashMap[LocalDate, mutable.ArrayBuffer[TaskBlock]]()
  val unplaced = mutable.ArrayBuffer[String]()
  val unplacedInfo = mutable.ArrayBuffer[UnplacedInfo]()
  var dailyReserveHours: Int = 1
  var reserveBeforeDeadlineDays: Int = 0
  var planMode: String = PlanMode.Early
  var nextTaskNumber: Int = 1


sealed trait AppError
object AppError:
  case class TaskNotFound(id: String) extends AppError
  case class TaskAlreadyExists(id: String) extends AppError
  case class RoutineNotFound(name: String) extends AppError
  case class InvalidHours(value: Int) extends AppError
  case class InvalidDateRange(start: LocalDate, deadline: LocalDate) extends AppError
  case class UnknownPriority(raw: String) extends AppError
  case class MinBlockTooLarge(minBlock: Int, remaining: Int) extends AppError
  case class RoutineOverlap(a: String, b: String, day: String) extends AppError
  case class ConfigValidationErrors(errors: List[String]) extends AppError
  case class StateValidationErrors(errors: List[String]) extends AppError
  case class IoError(message: String, cause: Option[Throwable] = None) extends AppError
  case class InvalidCommand(message: String) extends AppError
  case object PeriodNotSet extends AppError
  case object NoDaysToPlan extends AppError
  case class NegativeReserve(value: Int) extends AppError
  case class NegativeDeadlineReserve(value: Int) extends AppError
  case object ExitRequested extends AppError


sealed trait Command
object Command:
  case object ShowTasks extends Command
  case class AddTask(id: String, name: String, description: Option[String],
                     hours: Int, startDate: Option[LocalDate], deadline: Option[LocalDate],
                     priority: String, divisible: Boolean, minBlock: Int) extends Command
  case class EditTask(id: String, updates: TaskUpdates) extends Command
  case class DeleteTask(id: String) extends Command
  case class CancelTask(id: String) extends Command
  case class MarkProgress(id: String, delta: Int) extends Command

  case object ShowRoutines extends Command
  case class AddRoutine(name: String, days: Seq[String], startTime: String, endTime: String,
                        hours: Int, windowFrom: String, windowTo: String, minBlock: Int,
                        validFrom: Option[LocalDate], validTo: Option[LocalDate],
                        mandatory: Boolean) extends Command
  case class ToggleRoutine(name: String) extends Command

  case object ShowBusy extends Command
  case class AddBusy(name: String, date: LocalDate, startTime: String, endTime: String,
                     description: Option[String]) extends Command

  case class SetPeriod(start: LocalDate, end: LocalDate) extends Command
  case object BuildPlan extends Command
  case object RebuildPlan extends Command
  case class ShowDailyBudget(day: LocalDate) extends Command
  case class ViewDay(day: LocalDate) extends Command
  case class ViewWeek(start: LocalDate) extends Command
  case object ViewAll extends Command
  case class ViewTaskSchedule(id: String) extends Command

  case object ShowReserves extends Command
  case class SetDailyReserve(hours: Int) extends Command
  case class SetReserveBeforeDeadline(days: Int) extends Command
  case class ChooseMode(mode: String) extends Command

  case class MarkUnavailable(day: LocalDate) extends Command
  case object ShowOverload extends Command
  case class SaveReport(name: String) extends Command
  case object SaveState extends Command
  case object ReloadConfig extends Command
  case object ResetState extends Command
  case object Exit extends Command

case class TaskUpdates(
                        name: Option[String] = None,
                        description: Option[String] = None,
                        hours: Option[Int] = None,
                        startDate: Option[LocalDate] = None,
                        deadline: Option[LocalDate] = None,
                        priority: Option[String] = None,
                        divisible: Option[Boolean] = None,
                        minBlock: Option[Int] = None)


def parseDate(s: String): Option[LocalDate] =
  if s == null || s.trim.isEmpty then None
  else Try(LocalDate.parse(s.trim)).toOption

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

def datesInRange(start: LocalDate, end: LocalDate): Seq[LocalDate] =
  val buf = mutable.ArrayBuffer[LocalDate]()
  var cur = start
  while !cur.isAfter(end) do
    buf += cur
    cur = cur.plusDays(1)
  buf.toSeq

def priorityRank(p: String): Int = p match
  case "высокий" | "high" | "критический" | "critical" => 0
  case "средний" | "normal" => 1
  case "низкий" | "low" => 2
  case _ => 3

def knownPriority(p: String): Boolean =
  Seq("высокий","средний","низкий","критический","low","normal","high","critical").contains(p)

def weekdayRu(d: LocalDate): String = d.getDayOfWeek.toString match
  case "MONDAY" => "Понедельник"
  case "TUESDAY" => "Вторник"
  case "WEDNESDAY" => "Среда"
  case "THURSDAY" => "Четверг"
  case "FRIDAY" => "Пятница"
  case "SATURDAY" => "Суббота"
  case "SUNDAY" => "Воскресенье"
  case other => other

def monthRu(m: Int): String = m match
  case 1 => "января"; case 2 => "февраля"; case 3 => "марта"
  case 4 => "апреля"; case 5 => "мая";    case 6 => "июня"
  case 7 => "июля";   case 8 => "августа"; case 9 => "сентября"
  case 10 => "октября"; case 11 => "ноября"; case 12 => "декабря"
  case _ => ""

def formatDateRu(d: LocalDate): String =
  s"${d.getDayOfMonth} ${monthRu(d.getMonthValue)} ${d.getYear}"

def formatInterval(start: Int, end: Int): String =
  s"${minutesToTime(start)}–${minutesToTime(end)}"


object Validation:
  def validateConfig(c: AppConfig): Seq[String] =
    val errs = mutable.ArrayBuffer[String]()
    if c.planning.dailyReserveMinutes < 0 then
      errs += "резерв имеет отрицательное значение"
    if c.planning.minimumTaskBlockMinutes > c.planning.maximumTaskBlockMinutes then
      errs += "минимальный блок больше максимального"
    if c.planning.maximumTaskHoursPerDay * 60 < c.planning.minimumTaskBlockMinutes then
      errs += "максимальная дневная нагрузка меньше минимального блока"
    if !Seq("balanced","early","even","раннее","равномерное","сбалансированное")
      .contains(c.defaultPlanningMode) then
      errs += s"неизвестный режим планирования: ${c.defaultPlanningMode}"
    errs.toSeq

  def validateState(s: AppState): Seq[String] =
    val errs = mutable.ArrayBuffer[String]()
    for t <- s.tasks do
      if t.hours <= 0 then errs += s"дело ${t.id}: нулевая или отрицательная трудоёмкость"
      if t.done < 0 then errs += s"дело ${t.id}: отрицательное выполненное время"
      if t.done > t.hours then errs += s"дело ${t.id}: выполнено больше, чем трудоёмкость"
      for { st <- t.startDate; dl <- t.deadline if dl.isBefore(st) }
        errs += s"дело ${t.id}: срок раньше даты начала"
      if !knownPriority(t.priority) then
        errs += s"дело ${t.id}: неизвестный приоритет '${t.priority}'"
    errs.toSeq


object Domain:

  def refreshStates(s: AppState): Unit =
    val today = LocalDate.now()
    s.tasks.foreach { t =>
      if t.state == TaskState.Cancelled then ()
      else if t.done >= t.hours then t.state = TaskState.Completed
      else if t.deadline.exists(_.isBefore(today)) then t.state = TaskState.Overdue
      else if t.done > 0 then t.state = TaskState.InProgress
      else t.state = TaskState.Planned
    }

  def routineApplies(r: Routine, day: LocalDate): Boolean =
    if !r.enabled then false
    else
      val dow = day.getDayOfWeek.toString
      val dayMatches = r.daysOfWeek.contains(dow)
      val inValid = r.validFrom.forall(!day.isBefore(_)) &&
        r.validTo.forall(!day.isAfter(_))
      dayMatches && inValid

  def routineHoursFor(r: Routine): Int =
    if r.startTime.nonEmpty && r.endTime.nonEmpty then hoursBetween(r.startTime, r.endTime)
    else r.hours

  def busyHoursFor(b: BusyInterval): Int = hoursBetween(b.startTime, b.endTime)

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

  def computeDailyBudget(s: AppState, day: LocalDate): DailyBudget =
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
      if end > start then allIntervals += Interval(start, end)
      else if end < start then
        allIntervals += Interval(start, 24 * 60)
        allIntervals += Interval(0, end)
    val (dayStartMin, dayEndMin) = configDayWindow(s)
    val merged = mergeIntervals(allIntervals.toSeq)
    val totalCommitted = merged.map(_.lengthMin).sum / 60
    val error =
      if totalCommitted > 24 then Some(s"обязательные занятия занимают $totalCommitted ч (больше 24 ч)")
      else None
    val reserve = s.dailyReserveHours
    val windowHours = (dayEndMin - dayStartMin) / 60
    var freeForTasks = windowHours - totalCommitted - reserve
    if freeForTasks < 0 then freeForTasks = 0
    val freeIntervals =
      if error.isDefined then Seq.empty
      else complementIntervals(merged, dayStartMin, dayEndMin)
    DailyBudget(day, routineItems.toSeq, busyItems.toSeq, totalCommitted,
      reserve, freeForTasks, freeIntervals, error)

  def plannedHoursOnDay(s: AppState, day: LocalDate): Int =
    s.scheduled.get(day).map(_.map(_.hours).sum).getOrElse(0)

  def buildDayDisplay(s: AppState, day: LocalDate): Seq[(Int, Int, String)] =
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
      else
        display += ((iv.startMin, iv.endMin, "Свободное время"))
    display.sortBy(_._1).toSeq

  // ---- Дела ----
  def addTask(s: AppState, cmd: Command.AddTask): Either[AppError, Unit] =
    if cmd.hours <= 0 then Left(AppError.InvalidHours(cmd.hours))
    else if !knownPriority(cmd.priority) then Left(AppError.UnknownPriority(cmd.priority))
    else if s.tasks.exists(_.id == cmd.id) then Left(AppError.TaskAlreadyExists(cmd.id))
    else (cmd.startDate, cmd.deadline) match
      case (Some(st), Some(dl)) if dl.isBefore(st) =>
        Left(AppError.InvalidDateRange(st, dl))
      case _ =>
        s.tasks += Task(cmd.id, cmd.name, cmd.description, cmd.hours, 0,
          cmd.startDate, cmd.deadline, cmd.priority, cmd.divisible, cmd.minBlock,
          TaskState.Planned, s.nextTaskNumber)
        s.nextTaskNumber += 1
        Right(())

  def editTask(s: AppState, id: String, u: TaskUpdates): Either[AppError, Unit] =
    s.tasks.find(_.id == id) match
      case None => Left(AppError.TaskNotFound(id))
      case Some(t) =>
        val newHours = u.hours.getOrElse(t.hours)
        if newHours <= 0 then Left(AppError.InvalidHours(newHours))
        else
          val newPriority = u.priority.getOrElse(t.priority)
          if newPriority.nonEmpty && !knownPriority(newPriority) then
            Left(AppError.UnknownPriority(newPriority))
          else
            val newStart = u.startDate.orElse(t.startDate)
            val newDl = u.deadline.orElse(t.deadline)
            (newStart, newDl) match
              case (Some(st), Some(dl)) if dl.isBefore(st) =>
                Left(AppError.InvalidDateRange(st, dl))
              case _ =>
                val idx = s.tasks.indexOf(t)
                s.tasks(idx) = t.copy(
                  name = u.name.getOrElse(t.name),
                  description = u.description.orElse(t.description),
                  hours = newHours,
                  startDate = newStart,
                  deadline = newDl,
                  priority = if newPriority.isEmpty then t.priority else newPriority,
                  divisible = u.divisible.getOrElse(t.divisible),
                  minBlock = u.minBlock.getOrElse(t.minBlock))
                Right(())

  def deleteTask(s: AppState, id: String): Either[AppError, Unit] =
    val before = s.tasks.size
    s.tasks.filterInPlace(_.id != id)
    if s.tasks.size == before then Left(AppError.TaskNotFound(id))
    else
      for (d, blocks) <- s.scheduled do blocks.filterInPlace(_.taskId != id)
      for (d, items) <- s.plan do
        val taskName = id
        s.plan(d) = items.filterNot(_.startsWith(s"$taskName:"))
      Right(())

  def cancelTask(s: AppState, id: String): Either[AppError, Unit] =
    s.tasks.find(_.id == id) match
      case None => Left(AppError.TaskNotFound(id))
      case Some(t) =>
        t.state = TaskState.Cancelled
        Right(())

  def markProgress(s: AppState, id: String, delta: Int): Either[AppError, Unit] =
    s.tasks.find(_.id == id) match
      case None => Left(AppError.TaskNotFound(id))
      case Some(t) =>
        t.done = math.max(0, math.min(t.hours, t.done + delta))
        if t.done >= t.hours then t.state = TaskState.Completed
        else if t.done > 0 then t.state = TaskState.InProgress
        else t.state = TaskState.Planned
        Right(())

  // ---- Регулярные занятия ----
  def addRoutine(s: AppState, cmd: Command.AddRoutine): Either[AppError, Unit] =
    s.routines += Routine(cmd.name, cmd.days, cmd.startTime, cmd.endTime, cmd.hours,
      cmd.validFrom, cmd.validTo, cmd.mandatory, true, cmd.windowFrom, cmd.windowTo, cmd.minBlock)
    Right(())

  def toggleRoutine(s: AppState, name: String): Either[AppError, Unit] =
    s.routines.find(_.name == name) match
      case None => Left(AppError.RoutineNotFound(name))
      case Some(r) =>
        r.enabled = !r.enabled
        Right(())

  // ---- Занятые интервалы ----
  def addBusy(s: AppState, cmd: Command.AddBusy): Either[AppError, Unit] =
    if cmd.startTime.nonEmpty && cmd.endTime.nonEmpty && cmd.startTime == cmd.endTime then
      Left(AppError.InvalidCommand("Время начала и окончания совпадают"))
    else
      s.busy += BusyInterval(cmd.name, cmd.date, cmd.startTime, cmd.endTime, cmd.description)
      Right(())

  // ---- Период / резервы / режим ----
  def setPeriod(s: AppState, start: LocalDate, end: LocalDate): Unit =
    s.periodStart = Some(start)
    s.periodEnd = Some(end)

  def setDailyReserve(s: AppState, h: Int): Either[AppError, Unit] =
    if h < 0 then Left(AppError.NegativeReserve(h))
    else { s.dailyReserveHours = h; Right(()) }

  def setReserveBeforeDeadline(s: AppState, d: Int): Either[AppError, Unit] =
    if d < 0 then Left(AppError.NegativeDeadlineReserve(d))
    else { s.reserveBeforeDeadlineDays = d; Right(()) }

  def chooseMode(s: AppState, m: String): Unit =
    s.planMode = m match
      case "balanced" | "сбалансированное" => PlanMode.Even
      case "early" | "раннее" => PlanMode.Early
      case "even" | "равномерное" => PlanMode.Even
      case _ => PlanMode.Early

  def markUnavailable(s: AppState, day: LocalDate): Unit =
    s.unavailableDays += day
    s.plan.remove(day)
    s.scheduled.remove(day)

  // ---- Планирование ----
  def effectiveStart(t: Task): LocalDate =
    t.startDate.getOrElse(LocalDate.of(0, 1, 1))

  def effectiveDeadline(s: AppState, t: Task): LocalDate =
    t.deadline match
      case None => LocalDate.of(9999, 12, 31)
      case Some(dl) if s.reserveBeforeDeadlineDays > 0 =>
        dl.minusDays(s.reserveBeforeDeadlineDays)
      case Some(dl) => dl

  def buildPlan(s: AppState): Either[AppError, Unit] =
    (s.periodStart, s.periodEnd) match
      case (Some(start), Some(end)) => buildPlanFrom(s, start)
      case _ => Left(AppError.PeriodNotSet)

  def buildPlanFrom(s: AppState, fromDay: LocalDate): Either[AppError, Unit] =
    (s.periodStart, s.periodEnd) match
      case (Some(pStart), Some(pEnd)) =>
        val errs = Validation.validateState(s)
        if errs.nonEmpty then Left(AppError.StateValidationErrors(errs.toList))
        else
          val days = datesInRange(fromDay, pEnd)
          if days.isEmpty then Left(AppError.NoDaysToPlan)
          else
            for d <- days do
              s.plan.remove(d)
              s.scheduled.remove(d)
            s.unplaced.clear()
            s.unplacedInfo.clear()

            val maxTaskHours = s.config.planning.maximumTaskHoursPerDay
            val freeHoursMap = mutable.Map[LocalDate, Int]()
            for d <- days do
              s.plan(d) = mutable.ArrayBuffer[String]()
              s.scheduled(d) = mutable.ArrayBuffer[TaskBlock]()
              if s.unavailableDays.contains(d) then freeHoursMap(d) = 0
              else
                val b = computeDailyBudget(s, d)
                if b.error.isDefined then freeHoursMap(d) = 0
                else freeHoursMap(d) = math.min(b.freeForTasksHours, maxTaskHours)

            val remaining = mutable.Map[String, Int]()
            for t <- s.tasks do
              if t.state == TaskState.Cancelled then remaining(t.id) = 0
              else remaining(t.id) = math.max(0, t.hours - t.done)

            val activeTasks = s.tasks.filter(t => t.state != TaskState.Cancelled && remaining(t.id) > 0)

            val sorted = activeTasks.sortBy { t =>
              (effectiveDeadline(s, t), priorityRank(t.priority), t.createdAt)
            }

            val dayFree = mutable.Map[LocalDate, Int]()
            for d <- days do dayFree(d) = freeHoursMap.getOrElse(d, 0)

            val freeIntervalsMap = mutable.Map[LocalDate, mutable.ArrayBuffer[Interval]]()
            for d <- days do
              if s.unavailableDays.contains(d) then freeIntervalsMap(d) = mutable.ArrayBuffer[Interval]()
              else
                val b = computeDailyBudget(s, d)
                if b.error.isDefined then freeIntervalsMap(d) = mutable.ArrayBuffer[Interval]()
                else freeIntervalsMap(d) = mutable.ArrayBuffer.from(b.freeIntervals)

            val minBlockConfig = s.config.planning.minimumTaskBlockMinutes
            val maxBlockConfig = s.config.planning.maximumTaskBlockMinutes

            def allocateInDay(day: LocalDate, t: Task, wantHours: Int): Int =
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
                  if !t.divisible && canMin < remainingMin then canMin = 0
                  else if t.divisible && canMin < minBlockMin && canMin < remainingMin then canMin = 0
                if canMin > 0 then
                  val start = iv.startMin
                  val end = start + canMin
                  s.scheduled(day) += TaskBlock(t.id, t.name, start, end)
                  s.plan(day) += s"${t.name}: ${canMin / 60}ч (${minutesToTime(start)}–${minutesToTime(end)})"
                  allocatedMin += canMin
                  remainingMin -= canMin
                  if canMin >= iv.lengthMin then intervals.remove(i)
                  else { intervals(i) = Interval(iv.startMin + canMin, iv.endMin); i += 1 }
                else i += 1
              allocatedMin / 60

            val isEven = s.planMode == PlanMode.Even
            for t <- sorted do
              val effD = effectiveDeadline(s, t)
              val effS = effectiveStart(t)
              for d <- days do
                if remaining(t.id) > 0 && !d.isBefore(effS) && !d.isAfter(effD) && dayFree(d) > 0 then
                  if t.divisible then
                    var want = remaining(t.id)
                    if isEven then
                      var remDays = 0
                      for dd <- days do
                        if !dd.isBefore(d) && !dd.isAfter(effD) then remDays += 1
                      if remDays > 0 then
                        val per = math.ceil(remaining(t.id).toDouble / remDays).toInt
                        if per < want then want = per
                    var canTake = math.min(dayFree(d), want)
                    if canTake < t.minBlock && canTake < remaining(t.id) then canTake = 0
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
                val effD = effectiveDeadline(s, t)
                val avail = days.filter(d => !d.isBefore(effectiveStart(t)) && !d.isAfter(effD))
                  .map(d => dayFree.getOrElse(d, 0)).sum
                val deficit = math.max(0, remaining(t.id) - avail)
                val reason =
                  if s.unavailableDays.nonEmpty then "есть недоступные дни"
                  else if s.busy.exists(b => !b.date.isBefore(fromDay) && !b.date.isAfter(effD)) then "занятые интервалы"
                  else "недостаточно свободного времени"
                s.unplacedInfo += UnplacedInfo(t.id, t.name, remaining(t.id), t.deadline, avail, deficit, reason)
                t.state = TaskState.Unplaced

            refreshStates(s)
            Right(())
      case _ => Left(AppError.PeriodNotSet)


object ConfigLoader:
  def load(path: String): Either[AppError, AppConfig] =
    val attempt: Try[AppConfig] = Try {
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
    attempt match
      case Success(v) => Right(v)
      case Failure(e) => Left(AppError.IoError(e.getMessage, Some(e)))

  private def save(cfg: AppConfig, path: String): Either[AppError, Unit] =
    Try {
      val w = new PrintWriter(path, "UTF-8")
      try w.println(jsonToString(configToJson(cfg)))
      finally w.close()
    } match
      case Success(_) => Right(())
      case Failure(e) => Left(AppError.IoError(e.getMessage, Some(e)))

  private def parseConfig(j: Json): AppConfig =
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
      start = jStrOpt(jField(j, "start")).getOrElse(""),
      end = jStrOpt(jField(j, "end")).getOrElse(""),
      durationMinutes = jOptField(j, "durationMinutes").map(jInt).getOrElse(0),
      allowedFrom = jStrOpt(jField(j, "allowedFrom")).getOrElse(""),
      allowedTo = jStrOpt(jField(j, "allowedTo")).getOrElse(""),
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

object StateStore:
  def save(s: AppState, path: String): Either[AppError, Unit] =
    Try {
      val json = JObj(Map(
        "tasks" -> JArr(s.tasks.map(taskToJson).toList),
        "routines" -> JArr(s.routines.map(routineToJson).toList),
        "busy" -> JArr(s.busy.map(busyToJson).toList),
        "unavailableDays" -> JArr(s.unavailableDays.toList.map(d => JStr(d.toString))),
        "periodStart" -> s.periodStart.map(d => JStr(d.toString)).getOrElse(JNull),
        "periodEnd" -> s.periodEnd.map(d => JStr(d.toString)).getOrElse(JNull),
        "plan" -> JObj(s.plan.map { case (k, v) =>
          k.toString -> (JArr(v.toList.map(JStr(_))): Json)
        }.toMap),
        "scheduled" -> JObj(s.scheduled.map { case (k, v) =>
          k.toString -> (JArr(v.toList.map(blockToJson)): Json)
        }.toMap),
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
    } match
      case Success(_) => Right(())
      case Failure(e) => Left(AppError.IoError(e.getMessage, Some(e)))

  def load(s: AppState, path: String): Either[AppError, Unit] =
    val file = new File(path)
    if !file.exists() then Right(())
    else
      val attempt: Try[Unit] = Try {
        val src = scala.io.Source.fromFile(file, "UTF-8")
        val text = try src.mkString finally src.close()
        val json = new JsonParser(text).parse()
        applyState(s, json)
      }
      attempt match
        case Success(_) => Right(())
        case Failure(e) => Left(AppError.IoError(e.getMessage, Some(e)))

  private def taskToJson(t: Task): Json = JObj(Map(
    "id" -> JStr(t.id),
    "name" -> JStr(t.name),
    "description" -> t.description.map(JStr(_)).getOrElse(JNull),
    "hours" -> JNum(t.hours),
    "done" -> JNum(t.done),
    "startDate" -> t.startDate.map(d => JStr(d.toString)).getOrElse(JNull),
    "deadline" -> t.deadline.map(d => JStr(d.toString)).getOrElse(JNull),
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
    "validFrom" -> r.validFrom.map(d => JStr(d.toString)).getOrElse(JNull),
    "validTo" -> r.validTo.map(d => JStr(d.toString)).getOrElse(JNull),
    "mandatory" -> JBool(r.mandatory),
    "enabled" -> JBool(r.enabled),
    "windowFrom" -> JStr(r.windowFrom),
    "windowTo" -> JStr(r.windowTo),
    "minBlock" -> JNum(r.minBlock)))

  private def busyToJson(b: BusyInterval): Json = JObj(Map(
    "name" -> JStr(b.name),
    "date" -> JStr(b.date.toString),
    "startTime" -> JStr(b.startTime),
    "endTime" -> JStr(b.endTime),
    "description" -> b.description.map(JStr(_)).getOrElse(JNull)))

  private def blockToJson(b: TaskBlock): Json = JObj(Map(
    "taskId" -> JStr(b.taskId),
    "taskName" -> JStr(b.taskName),
    "startMin" -> JNum(b.startMin),
    "endMin" -> JNum(b.endMin)))

  private def unplacedToJson(u: UnplacedInfo): Json = JObj(Map(
    "taskId" -> JStr(u.taskId),
    "taskName" -> JStr(u.taskName),
    "remainingHours" -> JNum(u.remainingHours),
    "deadline" -> u.deadline.map(d => JStr(d.toString)).getOrElse(JNull),
    "availableHours" -> JNum(u.availableHours),
    "deficitHours" -> JNum(u.deficitHours),
    "reason" -> JStr(u.reason)))

  private def applyState(s: AppState, j: Json): Unit =
    s.tasks.clear()
    for t <- jArr(jField(j, "tasks")) do
      s.tasks += Task(
        id = jStr(jField(t, "id")),
        name = jStr(jField(t, "name")),
        description = jStrOpt(jField(t, "description")),
        hours = jInt(jField(t, "hours")),
        done = jInt(jField(t, "done")),
        startDate = jDate(jField(t, "startDate")),
        deadline = jDate(jField(t, "deadline")),
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
        validFrom = jDate(jField(r, "validFrom")),
        validTo = jDate(jField(r, "validTo")),
        mandatory = jBool(jField(r, "mandatory")),
        enabled = jBool(jField(r, "enabled")),
        windowFrom = jStr(jField(r, "windowFrom")),
        windowTo = jStr(jField(r, "windowTo")),
        minBlock = jInt(jField(r, "minBlock")))
    s.busy.clear()
    for b <- jArr(jField(j, "busy")) do
      s.busy += BusyInterval(
        name = jStr(jField(b, "name")),
        date = parseDate(jStr(jField(b, "date"))).getOrElse(LocalDate.now()),
        startTime = jStr(jField(b, "startTime")),
        endTime = jStr(jField(b, "endTime")),
        description = jStrOpt(jField(b, "description")))
    s.unavailableDays.clear()
    for d <- jArr(jField(j, "unavailableDays")) do
      parseDate(jStr(d)).foreach(s.unavailableDays += _)
    s.periodStart = jDate(jField(j, "periodStart"))
    s.periodEnd = jDate(jField(j, "periodEnd"))
    s.plan.clear()
    jField(j, "plan") match
      case JObj(fields) =>
        for (k, v) <- fields do
          parseDate(k).foreach { d =>
            s.plan(d) = mutable.ArrayBuffer.from(jArr(v).map(jStr))
          }
      case _ => ()
    s.scheduled.clear()
    jField(j, "scheduled") match
      case JObj(fields) =>
        for (k, v) <- fields do
          parseDate(k).foreach { d =>
            val buf = mutable.ArrayBuffer[TaskBlock]()
            for blk <- jArr(v) do
              buf += TaskBlock(
                taskId = jStr(jField(blk, "taskId")),
                taskName = jStr(jField(blk, "taskName")),
                startMin = jInt(jField(blk, "startMin")),
                endMin = jInt(jField(blk, "endMin")))
            s.scheduled(d) = buf
          }
      case _ => ()
    s.unplaced.clear()
    for u <- jArr(jField(j, "unplaced")) do s.unplaced += jStr(u)
    s.unplacedInfo.clear()
    for u <- jArr(jField(j, "unplacedInfo")) do
      s.unplacedInfo += UnplacedInfo(
        taskId = jStr(jField(u, "taskId")),
        taskName = jStr(jField(u, "taskName")),
        remainingHours = jInt(jField(u, "remainingHours")),
        deadline = jDate(jField(u, "deadline")),
        availableHours = jInt(jField(u, "availableHours")),
        deficitHours = jInt(jField(u, "deficitHours")),
        reason = jStr(jField(u, "reason")))
    s.dailyReserveHours = jInt(jField(j, "dailyReserveHours"))
    s.reserveBeforeDeadlineDays = jInt(jField(j, "reserveBeforeDeadlineDays"))
    s.planMode = jStr(jField(j, "planMode"))
    s.nextTaskNumber = jOptField(j, "nextTaskNumber").map(jInt).getOrElse(1)

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
        startTime = st, endTime = en,
        hours = hrs,
        validFrom = None, validTo = None,
        mandatory = true, enabled = true,
        windowFrom = rc.allowedFrom, windowTo = rc.allowedTo,
        minBlock = if rc.minimumBlockMinutes > 0 then rc.minimumBlockMinutes / 60 else 0)
  else
    s.routines += Routine("Сон", allDays, "23:30", "07:30", 8,
      None, None, true, true, "", "", 0)
    s.routines += Routine("Питание", allDays, "", "", 3,
      None, None, true, true, "07:00", "22:00", 1)

def applyConfig(s: AppState, cfg: AppConfig): Unit =
  s.config = cfg
  s.dailyReserveHours = cfg.planning.dailyReserveMinutes / 60
  s.reserveBeforeDeadlineDays = cfg.planning.deadlineReserveDays
  s.planMode = cfg.defaultPlanningMode match
    case "balanced" | "сбалансированное" => PlanMode.Even
    case "early" | "раннее"              => PlanMode.Early
    case "even" | "равномерное"          => PlanMode.Even
    case _                               => PlanMode.Early

def resetState(s: AppState): Unit =
  s.tasks.clear()
  s.routines.clear()
  s.busy.clear()
  s.unavailableDays.clear()
  s.plan.clear()
  s.scheduled.clear()
  s.unplaced.clear()
  s.unplacedInfo.clear()
  s.periodStart = None
  s.periodEnd = None
  s.dailyReserveHours = s.config.planning.dailyReserveMinutes / 60
  s.reserveBeforeDeadlineDays = s.config.planning.deadlineReserveDays
  s.planMode = PlanMode.Early
  s.nextTaskNumber = 1
  initDefaultRoutines(s)

def buildReport(s: AppState): String =
  val sb = new StringBuilder
  sb.append("=== СВОДКА ===\n")
  (s.periodStart, s.periodEnd) match
    case (Some(a), Some(b)) => sb.append(s"Период: ${formatDateRu(a)} – ${formatDateRu(b)}\n")
    case _ => sb.append("Период не задан\n")
  val active = s.tasks.filter(_.state != TaskState.Cancelled)
  val totalHours = active.map(_.hours).sum
  val doneHours = active.map(_.done).sum
  sb.append(s"Дел: ${active.size}\n")
  sb.append(s"Общая трудоёмкость: $totalHours ч\n")
  sb.append(s"Уже выполнено: $doneHours ч\n")
  sb.append(s"Осталось: ${math.max(0, totalHours - doneHours)} ч\n\n")
  sb.append("=== СПИСОК ДЕЛ ===\n")
  for t <- s.tasks do
    sb.append(s"${t.id} | ${t.name} | ${t.hours}ч | выполнено ${t.done}ч | " +
      s"осталось ${t.remaining}ч | срок ${t.deadline.map(_.toString).getOrElse("-")} | " +
      s"приоритет ${t.priority} | состояние ${t.state}\n")
  sb.append("\n=== РАСПРЕДЕЛЕНИЕ ПО ДНЯМ ===\n")
  for (d, blocks) <- s.scheduled.toSeq.sortBy(_._1) do
    val hrs = blocks.map(_.hours).sum
    sb.append(s"$d: $hrs ч (блоков ${blocks.size})\n")
  sb.append("\n=== НЕВМЕЩЁННЫЕ ДЕЛА ===\n")
  if s.unplacedInfo.isEmpty then sb.append("(нет)\n")
  else
    for u <- s.unplacedInfo do
      sb.append(s"${u.taskName}: осталось ${u.remainingHours}ч, " +
        s"срок ${u.deadline.map(_.toString).getOrElse("-")}, " +
        s"доступно ${u.availableHours}ч, дефицит ${u.deficitHours}ч (${u.reason})\n")
  sb.toString

def saveReport(s: AppState, name: String): Either[AppError, String] =
  Try {
    val dir = new File(s.config.reportDirectory)
    if !dir.exists() then dir.mkdirs()
    val file = new File(dir, name)
    val w = new PrintWriter(file, "UTF-8")
    try w.println(buildReport(s))
    finally w.close()
    file.getPath
  } match
    case Success(p) => Right(p)
    case Failure(e) => Left(AppError.IoError(e.getMessage, Some(e)))


object UI:
  def prompt(msg: String): String =
    print(msg)
    val line = StdIn.readLine()
    if line == null then "" else line.trim

  def promptOpt(msg: String): Option[String] =
    val s = prompt(msg)
    if s.isEmpty then None else Some(s)

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

  def promptDate(msg: String): Option[LocalDate] =
    val s = prompt(msg)
    if s.isEmpty then None
    else parseDate(s) match
      case some @ Some(_) => some
      case None =>
        println("Некорректная дата, используйте ГГГГ-ММ-ДД")
        promptDate(msg)

  def promptDateRequired(msg: String): LocalDate =
    promptDate(msg).getOrElse {
      println("Дата обязательна")
      promptDateRequired(msg)
    }

  def readCommand: Command =
    println()
    println("1. Дела")
    println("2. Регулярные занятия")
    println("3. Разовые занятые интервалы")
    println("4. Планирование")
    println("5. Резервы и режим")
    println("6. Просмотр плана")
    println("7. Отметить прогресс")
    println("8. Отметить недоступный день")
    println("9. Перегрузка / невмещённые")
    println("10. Сохранить отчёт")
    println("11. Сохранить состояние")
    println("12. Перезагрузить конфигурацию")
    println("13. Сбросить состояние")
    println("0. Выход")
    prompt("Выбор: ") match
      case "1" => readTasksMenu()
      case "2" => readRoutinesMenu()
      case "3" => readBusyMenu()
      case "4" => readPlanMenu()
      case "5" => readReservesMenu()
      case "6" => readViewMenu()
      case "7" => Command.MarkProgress(prompt("Идентификатор дела: "), promptInt("Сколько часов выполнено: "))
      case "8" => Command.MarkUnavailable(promptDateRequired("Недоступная дата: "))
      case "9" => Command.ShowOverload
      case "10" => Command.SaveReport(prompt("Имя файла отчёта: "))
      case "11" => Command.SaveState
      case "12" => Command.ReloadConfig
      case "13" => Command.ResetState
      case "0" => Command.Exit
      case _ => println("Не понял выбор"); readCommand

  def readTasksMenu(): Command =
    println("1.1. Показать дела")
    println("1.2. Добавить дело")
    println("1.3. Изменить дело")
    println("1.4. Удалить дело")
    println("1.5. Отменить дело")
    println("1.0. Назад")
    prompt("Выбор: ") match
      case "1.1" => Command.ShowTasks
      case "1.2" => readAddTask()
      case "1.3" => readEditTask()
      case "1.4" => Command.DeleteTask(prompt("Идентификатор дела: "))
      case "1.5" => Command.CancelTask(prompt("Идентификатор дела: "))
      case _ => readCommand

  def readRoutinesMenu(): Command =
    println("2.1. Показать")
    println("2.2. Добавить")
    println("2.3. Включить/отключить")
    println("2.0. Назад")
    prompt("Выбор: ") match
      case "2.1" => Command.ShowRoutines
      case "2.2" => readAddRoutine()
      case "2.3" => Command.ToggleRoutine(prompt("Название занятия: "))
      case _ => readCommand

  def readBusyMenu(): Command =
    println("3.1. Показать")
    println("3.2. Добавить")
    println("3.0. Назад")
    prompt("Выбор: ") match
      case "3.1" => Command.ShowBusy
      case "3.2" => readAddBusy()
      case _ => readCommand

  def readPlanMenu(): Command =
    println("4.1. Выбрать период")
    println("4.2. Построить план")
    println("4.3. Перестроить план")
    println("4.4. Показать расчёт дня")
    println("4.0. Назад")
    prompt("Выбор: ") match
      case "4.1" =>
        val s = promptDateRequired("Начало периода: ")
        val e = promptDateRequired("Конец периода: ")
        Command.SetPeriod(s, e)
      case "4.2" => Command.BuildPlan
      case "4.3" => Command.RebuildPlan
      case "4.4" => Command.ShowDailyBudget(promptDateRequired("Дата: "))
      case _ => readCommand

  def readReservesMenu(): Command =
    println("5.1. Показать")
    println("5.2. Ежедневный резерв")
    println("5.3. Резерв перед сроком (дней)")
    println("5.4. Режим планирования")
    println("5.0. Назад")
    prompt("Выбор: ") match
      case "5.1" => Command.ShowReserves
      case "5.2" => Command.SetDailyReserve(promptInt("Ежедневный резерв (часов): "))
      case "5.3" => Command.SetReserveBeforeDeadline(promptInt("Резерв перед сроком (дней): "))
      case "5.4" =>
        println("1. раннее  2. равномерное")
        prompt("Выбор: ") match
          case "1" => Command.ChooseMode(PlanMode.Early)
          case "2" => Command.ChooseMode(PlanMode.Even)
          case _ => readCommand
      case _ => readCommand

  def readViewMenu(): Command =
    println("6.1. На день")
    println("6.2. На неделю")
    println("6.3. На весь период")
    println("6.4. Расписание выбранного дела")
    println("6.0. Назад")
    prompt("Выбор: ") match
      case "6.1" => Command.ViewDay(promptDateRequired("Дата: "))
      case "6.2" => Command.ViewWeek(promptDateRequired("Начало недели: "))
      case "6.3" => Command.ViewAll
      case "6.4" => Command.ViewTaskSchedule(prompt("Идентификатор дела: "))
      case _ => readCommand

  def readAddTask(): Command.AddTask =
    val id = prompt("Идентификатор дела: ")
    val name = prompt("Название: ")
    val desc = promptOpt("Описание (пусто — пропустить): ")
    val hours = promptInt("Трудоёмкость (часов): ")
    val start = promptDate("Дата начала (пусто — без ограничений): ")
    val dl = promptDate("Срок завершения (пусто — без ограничений): ")
    val pr = prompt("Приоритет (низкий/средний/высокий/критический): ")
    val div = promptYesNo("Можно делить на части? (да/нет): ")
    val minB = if div then promptInt("Минимальная продолжительность блока (часов): ") else hours
    Command.AddTask(id, name, desc, hours, start, dl, pr, div, minB)

  def readEditTask(): Command.EditTask =
    val id = prompt("Идентификатор дела: ")
    val name = promptOpt(s"Новое название (пусто — оставить): ")
    val desc = promptOpt("Новое описание (пусто — оставить): ")
    val hours = promptOpt("Трудоёмкость (пусто — оставить): ").map(_.toInt)
    val start = promptDate("Новая дата начала (пусто — оставить): ")
    val dl = promptDate("Новый срок (пусто — оставить): ")
    val pr = promptOpt("Новый приоритет (пусто — оставить): ")
    val div = Some(promptYesNo("Делить на части? (да/нет): "))
    val minB = if div.contains(true) then Some(promptInt("Мин. блок (часов): ")) else None
    Command.EditTask(id, TaskUpdates(name, desc, hours, start, dl, pr, div, minB))

  def readAddRoutine(): Command.AddRoutine =
    val name = prompt("Название занятия: ")
    val daysStr = prompt("Дни недели через запятую (ПН,ВТ,СР,ЧТ,ПТ,СБ,ВС): ")
    val days = daysStr.split(",").map(_.trim.toUpperCase).filter(_.nonEmpty).toSeq
    val strict = promptYesNo("Задать строгое время начала/окончания? (да/нет): ")
    var startTime = ""; var endTime = ""; var hours = 0
    var windowFrom = ""; var windowTo = ""; var minBlock = 0
    if strict then
      startTime = prompt("Время начала: ")
      endTime = prompt("Время окончания: ")
      hours = hoursBetween(startTime, endTime)
    else
      hours = promptInt("Общая продолжительность (часов): ")
      windowFrom = prompt("Допустимое время с: ")
      windowTo = prompt("Допустимое время до: ")
      minBlock = promptInt("Минимальный отдельный блок (часов): ")
    val vf = promptDate("Период действия с (пусто — без ограничений): ")
    val vt = promptDate("Период действия до (пусто — без ограничений): ")
    val mand = promptYesNo("Обязательное занятие? (да/нет): ")
    Command.AddRoutine(name, days, startTime, endTime, hours, windowFrom, windowTo, minBlock, vf, vt, mand)

  def readAddBusy(): Command.AddBusy =
    val name = prompt("Название события: ")
    val date = promptDateRequired("Дата: ")
    val st = prompt("Время начала: ")
    val en = prompt("Время окончания: ")
    val desc = promptOpt("Описание (пусто — пропустить): ")
    Command.AddBusy(name, date, st, en, desc)

  // ---- Рендеринг ошибок ----
  def renderError(err: AppError): Unit = err match
    case AppError.TaskNotFound(id) => println(s"Дело '$id' не найдено")
    case AppError.TaskAlreadyExists(id) => println(s"Дело '$id' уже существует")
    case AppError.RoutineNotFound(n) => println(s"Занятие '$n' не найдено")
    case AppError.InvalidHours(h) => println(s"Некорректная трудоёмкость: $h")
    case AppError.InvalidDateRange(s, d) => println(s"Срок $d раньше даты начала $s")
    case AppError.UnknownPriority(r) => println(s"Неизвестный приоритет '$r'")
    case AppError.MinBlockTooLarge(m, r) => println(s"Мин.блок $m больше остатка $r")
    case AppError.RoutineOverlap(a, b, d) => println(s"Пересечение '$a' и '$b' в $d")
    case AppError.ConfigValidationErrors(es) => println("Ошибки конфигурации:"); es.foreach(e => println(s"  - $e"))
    case AppError.StateValidationErrors(es) => println("Ошибки в данных:"); es.foreach(e => println(s"  - $e"))
    case AppError.IoError(m, _) => println(s"Ошибка ввода-вывода: $m")
    case AppError.InvalidCommand(m) => println(s"Некорректная команда: $m")
    case AppError.PeriodNotSet => println("Сначала выберите период планирования")
    case AppError.NoDaysToPlan => println("Нет дней для планирования")
    case AppError.NegativeReserve(v) => println(s"Резерв не может быть отрицательным: $v")
    case AppError.NegativeDeadlineReserve(v) => println(s"Резерв перед сроком не может быть отрицательным: $v")
    case AppError.ExitRequested => ()


  def renderSuccess(cmd: Command, s: AppState): Unit = cmd match
    case Command.ShowTasks =>
      if s.tasks.isEmpty then println("Список дел пуст")
      else s.tasks.foreach { t =>
        println(
          s"${t.id} | ${t.name} | ${t.hours}ч | выполнено ${t.done}ч | " +
            s"с ${t.startDate.map(_.toString).getOrElse("-")} по ${t.deadline.map(_.toString).getOrElse("-")} | " +
            s"приоритет: ${t.priority} | делится: ${if t.divisible then "да" else "нет"} | " +
            s"мин.блок: ${t.minBlock}ч | состояние: ${t.state}")
        t.description.foreach(d => println(s"  описание: $d"))
      }
    case Command.AddTask(_, n, _, _, _, _, _, _, _) => println(s"Дело '$n' добавлено")
    case Command.EditTask(id, _) => println(s"Дело '$id' изменено")
    case Command.DeleteTask(id) => println(s"Дело '$id' удалено")
    case Command.CancelTask(id) =>
      s.tasks.find(_.id == id).foreach(t => println(s"Дело '${t.name}' отменено"))
    case Command.MarkProgress(id, delta) =>
      s.tasks.find(_.id == id).foreach { t =>
        println(s"Дело: ${t.name}")
        println(s"Добавлено: $delta ч")
        println(s"Всего выполнено: ${t.done} ч")
        println(s"Осталось: ${t.remaining} ч")
      }
    case Command.ShowRoutines =>
      if s.routines.isEmpty then println("Регулярных занятий нет")
      else s.routines.foreach { r =>
        val time =
          if r.startTime.nonEmpty && r.endTime.nonEmpty then s"${r.startTime}-${r.endTime}"
          else s"гибкое, окно ${r.windowFrom}-${r.windowTo}, мин.блок ${r.minBlock}ч"
        val period = (r.validFrom, r.validTo) match
          case (None, None) => "без ограничения"
          case (a, b) => s"${a.map(_.toString).getOrElse("")}..${b.map(_.toString).getOrElse("")}"
        println(
          s"${r.name} | дни: ${r.daysOfWeek.mkString(",")} | $time | " +
            s"${Domain.routineHoursFor(r)}ч | период: $period | " +
            s"обязательное: ${if r.mandatory then "да" else "нет"} | " +
            s"включено: ${if r.enabled then "да" else "нет"}")
      }
    case Command.AddRoutine(n, _, _, _, _, _, _, _, _, _, _) => println(s"Занятие '$n' добавлено")
    case Command.ToggleRoutine(n) =>
      s.routines.find(_.name == n).foreach(r =>
        println(s"Занятие '${r.name}' теперь ${if r.enabled then "включено" else "отключено"}"))
    case Command.ShowBusy =>
      if s.busy.isEmpty then println("Разовых занятых интервалов нет")
      else s.busy.foreach { b =>
        println(s"${b.date} ${b.startTime}-${b.endTime} | ${b.name} | ${Domain.busyHoursFor(b)}ч")
        b.description.foreach(d => println(s"  описание: $d"))
      }
    case Command.AddBusy(_, d, _, _, _) => println(s"Разовый интервал на $d добавлен")
    case Command.SetPeriod(a, b) => println(s"Период: с $a по $b")
    case Command.BuildPlan | Command.RebuildPlan => println("План построен")
    case Command.ShowDailyBudget(day) =>
      val b = Domain.computeDailyBudget(s, day)
      println(s"=== $day ===")
      val (dS, dE) = Domain.configDayWindow(s)
      println(f"${"Планируемая часть дня"}%-40s ${minutesToTime(dS)} - ${minutesToTime(dE)}")
      for (name, hrs) <- b.routineItems do println(f"$name%-40s $hrs ч")
      for (name, hrs) <- b.busyItems do println(f"$name%-40s $hrs ч")
      println(f"${"Итого обязательного"}%-40s ${b.totalCommittedHours} ч")
      println(f"${"Резерв"}%-40s ${b.reserveHours} ч")
      b.error match
        case Some(e) => println(s"ОШИБКА: $e")
        case None =>
          println(f"${"Свободно для дел"}%-40s ${b.freeForTasksHours} ч")
          println("Свободные интервалы:")
          if b.freeIntervals.isEmpty then println("  (нет)")
          else b.freeIntervals.foreach { iv =>
            val total = iv.lengthMin; val h = total / 60; val m = total % 60
            val lenStr = if m == 0 then s"${h}ч" else s"${h}ч ${m}м"
            println(s"  ${minutesToTime(iv.startMin)} - ${minutesToTime(iv.endMin)} ($lenStr)")
          }
    case Command.ViewDay(day) =>
      val b = Domain.computeDailyBudget(s, day)
      println(s"План на $day")
      b.error match
        case Some(e) => println(s"ОШИБКА: $e")
        case None =>
          val items = Domain.buildDayDisplay(s, day)
          if items.isEmpty then println("(пусто)")
          else items.foreach { case (st, en, label) =>
            println(f"${formatInterval(st, en)}%-13s $label")
          }
          val planned = Domain.plannedHoursOnDay(s, day)
          val freeUnplanned = math.max(0, b.freeForTasksHours - planned)
          println(s"\nПлановая загрузка: $planned ч")
          println(s"Обязательные занятия: ${b.totalCommittedHours} ч")
          println(s"Резерв: ${b.reserveHours} ч")
          println(s"Свободное незапланированное время: $freeUnplanned ч")
    case Command.ViewWeek(start) =>
      for i <- 0 until 7 do
        val date = start.plusDays(i)
        val b = Domain.computeDailyBudget(s, date)
        val planned = Domain.plannedHoursOnDay(s, date)
        val avail = b.freeForTasksHours
        val filled =
          if avail <= 0 then 0
          else math.min(10, math.round(planned.toDouble / avail * 10).toInt)
        val bar = "█" * filled + "░" * (10 - filled)
        println(f"${weekdayRu(date)}%-12s $bar  $planned из $avail доступных часов")
        println(f"    обязательные: ${b.totalCommittedHours}ч, резерв: ${b.reserveHours}ч, " +
          s"свободно: ${math.max(0, avail - planned)}ч")
    case Command.ViewAll =>
      if s.plan.isEmpty then println("Плана нет")
      else s.plan.toSeq.sortBy(_._1).foreach { case (d, _) =>
        println(s"--- $d ---")
        val items = Domain.buildDayDisplay(s, d)
        if items.isEmpty then println("(пусто)")
        else items.foreach { case (st, en, label) =>
          println(f"${formatInterval(st, en)}%-13s $label")
        }
      }
    case Command.ViewTaskSchedule(id) =>
      s.tasks.find(_.id == id) match
        case None => println(s"Дело '$id' не найдено")
        case Some(t) =>
          println(s"Дело: ${t.name}")
          println(s"Трудоёмкость: ${t.hours}ч, выполнено: ${t.done}ч, осталось: ${t.remaining}ч")
          println(s"Срок: ${t.deadline.map(_.toString).getOrElse("-")}, приоритет: ${t.priority}, состояние: ${t.state}")
          var any = false
          for (d, blocks) <- s.scheduled.toSeq.sortBy(_._1) do
            for b <- blocks if b.taskId == id do
              any = true
              println(s"$d ${minutesToTime(b.startMin)}–${minutesToTime(b.endMin)} (${b.hours}ч)")
          if !any then println("Дело не размещено в плане")
    case Command.ShowReserves =>
      println(s"Ежедневный резерв: ${s.dailyReserveHours} ч")
      println(s"Резерв перед сроком: ${s.reserveBeforeDeadlineDays} дн.")
      println(s"Режим планирования: ${s.planMode}")
    case Command.SetDailyReserve(h) => println(s"Ежедневный резерв: $h ч")
    case Command.SetReserveBeforeDeadline(d) => println(s"Резерв перед сроком: $d дн.")
    case Command.ChooseMode(m) => println(s"Режим: $m")
    case Command.MarkUnavailable(day) => println(s"$day отмечено как недоступное")
    case Command.ShowOverload =>
      println("Невмещённые дела")
      if s.unplacedInfo.isEmpty then println("(нет)")
      else s.unplacedInfo.foreach(u =>
        println(s"${u.taskName} — осталось ${u.remainingHours}ч, " +
          s"срок ${u.deadline.map(_.toString).getOrElse("-")}, " +
          s"доступно ${u.availableHours}ч, дефицит ${u.deficitHours}ч (${u.reason})"))
    case Command.SaveReport(path) => println(s"Отчёт сохранён в $path")
    case Command.SaveState => println(s"Состояние сохранено в ${s.config.stateFile}")
    case Command.ReloadConfig => println("Конфигурация перезагружена")
    case Command.ResetState => println("Состояние сброшено")
    case Command.Exit => println("Работа завершена")
    case _ => ()

object Executor:
  def execute(s: AppState, cmd: Command): Either[AppError, AppState] = cmd match
    // Дела
    case c: Command.AddTask => Domain.addTask(s, c).map(_ => s)
    case c: Command.EditTask => Domain.editTask(s, c.id, c.updates).map(_ => s)
    case c: Command.DeleteTask => Domain.deleteTask(s, c.id).map(_ => s)
    case c: Command.CancelTask => Domain.cancelTask(s, c.id).map(_ => s)
    case c: Command.MarkProgress =>
      Domain.markProgress(s, c.id, c.delta).map { _ =>
        val today = LocalDate.now()
        (s.periodStart, s.periodEnd) match
          case (Some(ps), Some(pe)) if !today.isBefore(ps) && !today.isAfter(pe) =>
            Domain.buildPlanFrom(s, if today.isBefore(ps) then ps else today)
            s
          case _ => s
      }

    // Регулярные занятия
    case c: Command.AddRoutine => Domain.addRoutine(s, c).map(_ => s)
    case c: Command.ToggleRoutine => Domain.toggleRoutine(s, c.name).map(_ => s)

    // Занятые интервалы
    case c: Command.AddBusy =>
      Domain.addBusy(s, c).map { _ =>
        if s.periodStart.isDefined && s.periodEnd.isDefined then
          Domain.buildPlan(s)
        s
      }

    // Период / резервы / режим
    case c: Command.SetPeriod =>
      Domain.setPeriod(s, c.start, c.end); Right(s)
    case Command.BuildPlan | Command.RebuildPlan =>
      Domain.buildPlan(s).map(_ => s)
    case c: Command.SetDailyReserve =>
      Domain.setDailyReserve(s, c.hours).map(_ => s)
    case c: Command.SetReserveBeforeDeadline =>
      Domain.setReserveBeforeDeadline(s, c.days).map(_ => s)
    case c: Command.ChooseMode =>
      Domain.chooseMode(s, c.mode); Right(s)

    // Недоступный день
    case c: Command.MarkUnavailable =>
      Domain.markUnavailable(s, c.day)
      (s.periodStart, s.periodEnd) match
        case (Some(ps), Some(pe)) if !c.day.isBefore(ps) && !c.day.isAfter(pe) =>
          Domain.buildPlanFrom(s, c.day).map(_ => s)
        case _ => Right(s)


    case c: Command.SaveReport =>
      saveReport(s, c.name).map(_ => s)


    case Command.SaveState =>
      StateStore.save(s, s.config.stateFile).map(_ => s)


    case Command.ReloadConfig =>
      ConfigLoader.load("config.json").flatMap { cfg =>
        val errs = Validation.validateConfig(cfg)
        if errs.nonEmpty then Left(AppError.ConfigValidationErrors(errs.toList))
        else { applyConfig(s, cfg); Right(s) }
      }


    case Command.ResetState =>
      resetState(s); Right(s)


    case Command.Exit => Left(AppError.ExitRequested)


    case Command.ShowTasks | Command.ShowRoutines | Command.ShowBusy |
         Command.ShowReserves | Command.ShowOverload |
         _: Command.ShowDailyBudget | _: Command.ViewDay | _: Command.ViewWeek |
         Command.ViewAll | _: Command.ViewTaskSchedule => Right(s)

@main def MenuDemo2(): Unit =
  val state = new AppState

  ConfigLoader.load("config.json") match
    case Right(cfg) =>
      val cfgErrs = Validation.validateConfig(cfg)
      if cfgErrs.nonEmpty then
        println("Конфигурация некорректна, применяется базовый шаблон:")
        cfgErrs.foreach(e => println(s"  - $e"))
        applyConfig(state, AppConfig.default)
      else
        applyConfig(state, cfg)
    case Left(err) =>
      println(s"Не удалось загрузить конфигурацию: $err")
      println("Используется базовый шаблон.")
      applyConfig(state, AppConfig.default)

  StateStore.load(state, state.config.stateFile) match
    case Right(_) =>
      if state.routines.isEmpty then initDefaultRoutines(state)
    case Left(err) =>
      println(s"Файл состояния повреждён: $err")
      println("Запуск с чистым состоянием.")
      resetState(state)
      if state.routines.isEmpty then initDefaultRoutines(state)

  Domain.refreshStates(state)

  var running = true
  while running do
    val cmd = UI.readCommand
    Executor.execute(state, cmd) match
      case Left(AppError.ExitRequested) =>
        running = false
        StateStore.save(state, state.config.stateFile) match
          case Right(_) => ()
          case Left(e) => println(s"Не удалось сохранить состояние при выходе: $e")
        UI.renderSuccess(cmd, state)
      case Left(err) => UI.renderError(err)
      case Right(ns) =>
        Domain.refreshStates(ns)
        UI.renderSuccess(cmd, ns)