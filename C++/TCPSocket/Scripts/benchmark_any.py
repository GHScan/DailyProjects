# vim:fileencoding=utf-8

import sys, subprocess, time
import resource

cmd = sys.stdin.read()
start = time.time()
subprocess.Popen(['bash', '-c', cmd]).wait()
ru = resource.getrusage(resource.RUSAGE_CHILDREN)
print 'user time                :', '%.3f s' % ru.ru_utime
print 'system time              :', '%.3f s' % ru.ru_stime
print 'total time               :', '%.3f s' % (time.time() - start)
print 'max memory size          :', '%.3f MB' % (ru.ru_maxrss / float(1000))
print 'memory page fault        :', ru.ru_minflt
print 'io page fault            :', ru.ru_majflt
print 'block read               :', ru.ru_inblock
print 'block write              :', ru.ru_oublock
print 'context switch           :', ru.ru_nvcsw
print 'preempt context switch   :', ru.ru_inblock
