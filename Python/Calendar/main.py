#! vim:fileencoding=utf-8

class DatetimeDiff(object):
    def __init__(self, days, seconds):
        self.days, self.seconds = days, seconds

class Datetime(object):
    def __init__(self, year, month, day, hour = 0, minute = 0, second = 0):
        self.year, self.month, self.day = year, month, day
        self.hour, self.minute, self.second = hour, minute, second

    def getDays(self):
        days = 0
        for year in range(self.BEGINING.year, self.year):
            days += self.getYearDays(year)
        for month in range(1, self.month):
            days += self.getMdays(self.year, month)
        days += self.day - 1
        return days
    def getSeconds(self):
        return self.hour * 3600 + self.minute * 60 + self.second
    def addDatetime(self, year, month, days, seconds):
        seconds += self.getSeconds()
        days += seconds // self.SECONDS_PER_DAY
        seconds %= self.SECONDS_PER_DAY
        self.hour = seconds // 3600
        self.minute = seconds % 3600 // 60
        self.second = seconds % 60

        month += self.month - 1
        year += month // 12
        self.month = month % 12 + 1
        self.year += year
 
        return self._addDays(days)
    def toTime(self):
        return self.getDays() * self.SECONDS_PER_DAY + self.getSeconds()
    def wday(self):
        return (self.BEGINING_WDAY + self.getDays()) % 7
    def clone(self):
        import copy
        return copy.deepcopy(self)
    def _addDays(self, days):
        days += self.getDays()

        self.year = self.BEGINING.year
        while True:
            yearDays = self.getYearDays(self.year)
            if days >= yearDays:
                self.year += 1
                days -= yearDays
            else:
                break

        self.month = 1
        while True:
            mdays = self.getMdays(self.year, self.month)
            if days >= mdays:
                self.month += 1
                days -= mdays
            else:
                break

        self.day = 1 + days
        return self
    def consoleDisplay(self):
        print ('%d年%d月' % (self.year, self.month)).center(32)
        print '日  一  二  三  四  五  六'
        begin = Datetime(self.year, self.month, 1).wday()
        end = begin + self.getMdays(self.year, self.month)
        for row in range(6):
            for wday in range(7):
                idx = row * 7 + wday
                if idx >= begin and idx < end:
                    print '%2d ' % (idx - begin + 1) ,
                else:
                    print '   ',
            print

    def __sub__(self, dt):
        d1, s1 = self.getDays(), self.getSeconds()
        d2, s2 = dt.getDays(), dt.getSeconds()
        return DatetimeDiff(d1 - d2, s1 - s2)
    def __add__(self, diff):
        return self.clone().addDatetime(0, 0, diff.days, diff.seconds)
    def __str__(self):
        return '%d/%d/%d %d:%d:%d' % (self.year, self.month, self.day, self.hour, self.minute, self.second)

    @staticmethod
    def fromTime(t):
        return Datetime.BEGINING + DatetimeDiff(t // Datetime.SECONDS_PER_DAY, t % Datetime.SECONDS_PER_DAY)
    @staticmethod
    def parse(s):
        import re
        m = re.match(r'(\d+)/(\d+)/(\d+) (\d+):(\d+):(\d+)', s)
        if not m: return 
        return Datetime(int(m.group(1)), int(m.group(2)), int(m.group(3)), int(m.group(4)), int(m.group(5)), int(m.group(6)))
    @staticmethod
    def now():
        import time
        return Datetime.fromTime(time.time() - time.timezone)
    @staticmethod
    def isLeapYear(year):
        return (year % 4 == 0 and (year % 100 != 0 or year % 400 == 0))
    @staticmethod
    def getYearDays(year):
        return Datetime.isLeapYear(year) and 366 or 365
    @staticmethod
    def getMdays(year, month):
        return Datetime.MONTH_DAYS[Datetime.isLeapYear(year)][month - 1]

    MONTH_DAYS = [
            [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, ],
            [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, ],
            ]
    BEGINING_WDAY = 4
    SECONDS_PER_DAY = 3600 * 24
Datetime.BEGINING = Datetime(1970, 1, 1, 0, 0, 0)

s = Datetime.now()
s.consoleDisplay()
