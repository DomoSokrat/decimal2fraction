#!/bin/env -S nimrun --cc:gcc
import std/[algorithm, math, sequtils]

func cancelDown(numerator, denominator: int): (int, int) =
  let common = gcd(numerator, denominator)
  result = (numerator div common, denominator div common)

template toNumber(list: seq[int]): int =
  ## Convert a sequence of *digits* into a number
  foldl(list, a*10+b, 0)

func divmod(a, b: int): (int, int) = (a div b, a mod b)

func dec2frac*(wholePart: int; nonPeriodic, periodic: seq[int] = @[];
              negative = false): (int, int) =
  ## Convert a decimal fraction to a proper fraction.
  runnableExamples:
    assert dec2frac(0, @[], @[8, 5, 3, 7]) == (8537, 9999)
    assert dec2frac(0, @[], @[8, 5, 3, 7], true) == (-8537, 9999)
    assert dec2frac(1, @[2, 3], @[4, 5, 6]) == (41111, 33300)
    assert dec2frac(1, @[2, 3], @[4, 5, 6], true) == (41111, 33300)
    assert dec2frac(-1, @[2, 3], @[4, 5, 6], false) == (-41111, 33300)

  let isNegative = wholePart < 0 or (wholePart == 0 and negative)
  let wholePart = if isNegative: -wholePart else: wholePart

  let (num1, denom1) =
    if periodic.len > 0:
      cancelDown(periodic.toNumber, 10^periodic.len - 1)
    else:
      (0, 1)

  let (num2, denom2) =
    if nonPeriodic.len > 0:
      cancelDown(nonPeriodic.toNumber * denom1 + num1, denom1 * 10^nonPeriodic.len)
    else:
      (num1, denom1)

  result = cancelDown(wholePart * denom2 + num2, denom2)
  if isNegative: result[0] = -result[0]

func count_2_and_5(number: int): (int, int) =
  ## Count factors of 2s and 5s in `number`.
  var number = number
  while number mod 2 == 0:
    inc result[0]
    number = number div 2
  while number mod 5 == 0:
    inc result[1]
    number = number div 5

type
  PeriodIter* = iterator (): int {.closure.}
  DecimalFraction* = tuple[negative: bool; wholePart: int;
                          nonPeriodic: seq[int]; periodic: PeriodIter]

func frac2dec*(numerator, denominator: int): DecimalFraction =
  ## Convert a proper fraction into a decimal fraction
  runnableExamples:
    import std/sequtils
    var d: DecimalFraction
    d = frac2dec(617, 7992)
    assert (d[0], d[1], d[2]) == (false, 0, @[0, 7, 7])
    assert toSeq(d.periodic) == @[2, 0, 2]
    d = frac2dec(-617, 7992)
    assert (d[0], d[1], d[2]) == (true, 0, @[0, 7, 7])
    assert toSeq(d.periodic) == @[2, 0, 2]
    d = frac2dec(-617, -7992)
    assert (d[0], d[1], d[2]) == (false, 0, @[0, 7, 7])
    assert toSeq(d.periodic) == @[2, 0, 2]
    d = frac2dec(617, -7992)
    assert (d[0], d[1], d[2]) == (true, 0, @[0, 7, 7])
    assert toSeq(d.periodic) == @[2, 0, 2]

  var denominator = denominator
  if denominator < 0:
    result.negative = true
    denominator = -denominator
  var numerator = numerator
  if numerator < 0:
    result.negative = not result.negative
    numerator = -numerator

  (numerator, denominator) = cancelDown(numerator, denominator)
  var rest: int
  (result.wholePart, rest) = divmod(numerator, denominator)

  let count = block:
    let (twos, fives) = count_2_and_5(denominator)
    max(twos, fives)
  if count > 0:
    rest *= 10^count
    var nonPeriodic: int
    (nonPeriodic, rest) = divmod(rest, denominator)
    result.nonPeriodic = newSeqOfCap[int](count)
    while nonPeriodic > 0:
      var digit: int
      (nonPeriodic, digit) = divmod(nonPeriodic, 10)
      result.nonPeriodic.add digit
    if result.nonPeriodic.len < count:
      result.nonPeriodic.add repeat(0, count - result.nonPeriodic.len)
    reverse(result.nonPeriodic)

  if rest == 0:
    result.periodic = iterator(): int {.closure.} = discard
  else:
    (rest, denominator) = cancelDown(rest, denominator)
    result.periodic = (func(num, denom: int): PeriodIter =
      result = iterator(): int {.closure.} =
        let periodStart = num
        var num = num
        while true:
          num *= 10
          var digit: int
          (digit, num) = divmod(num, denom)
          yield digit
          if num == periodStart:
            break
    )(rest, denominator)
    # result.periodic = iterator(): int {.closure.} =
    #   let periodStart = rest
    #   var num = rest
    #   while true:
    #     num *= 10
    #     var digit: int
    #     (digit, num) = divmod(num, denominator)
    #     yield digit
    #     if num == periodStart:
    #       break

when isMainModule:
  import std/[os, strutils]

  case paramCount()
  of 3:
    let
      wholePart = paramStr(1)
      nonPeriodic = paramStr(2)
      periodic = paramStr(3)
    # catch invalid input
    if nonPeriodic.len > 0: discard parseInt(nonPeriodic)
    if periodic.len > 0: discard parseInt(periodic)

    stdout.write "$#.$#($#) = " % [wholePart, nonPeriodic, periodic]
    flushFile stdout
    let (num, denom) = dec2frac(parseInt(wholePart), nonPeriodic.mapIt(ord(it) - ord('0')),
                                periodic.mapIt(ord(it) - ord('0')), wholePart.startsWith("-"))
    stdout.write "$# / $# = ".format(num, denom)
    flushFile stdout
    let d = frac2dec(num, denom)
    let s = if d.negative: "-" else: ""
    stdout.write "$#$#.$#" % [s, $d.wholePart, d.nonPeriodic.mapIt($it).join("")]
    stdout.write "($#)\n" % [toSeq(d.periodic()).mapIt($it).join("")]
  of 2:
    let
      numerator = parseInt(paramStr(1))
      denominator = parseInt(paramStr(2))
    stdout.write "$# / $# = ".format(numerator, denominator)
    let (num, denom) = cancelDown(numerator, denominator)
    if num != numerator:
      stdout.write "$# / $# = ".format(num, denom)
    flushFile stdout
    let decimals = frac2dec(num, denom)
    let sign = if decimals.negative: "-" else: ""
    stdout.write "$#$#.$#(" % [sign, $decimals.wholePart,
                              decimals.nonPeriodic.mapIt($it).join("")]
    var count = 0
    for d in decimals.periodic():
      stdout.write d
      inc count
      if count mod 128 == 0:
        flushFile stdout
    stdout.write ")\n"
    # var count = 0
    # for _ in decimals.periodic(): inc count
    # echo count
  else: echo "Huh? ", commandLineParams()
