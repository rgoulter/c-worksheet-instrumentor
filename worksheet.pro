-libraryjars <java.home>/lib/rt.jar

-dontwarn scala.**
-dontwarn scalaz.syntax.**
-dontwarn org.antlr.**

-keepclasseswithmembers public class * {
    public static void main(java.lang.String[]);
}

-keep class * implements org.xml.sax.EntityResolver

-keepclassmembers class * {
    ** MODULE$;
}

-keepclassmembernames class scala.concurrent.forkjoin.ForkJoinPool {
    long ctl;
    long stealCount;
    int plock;
    int indexSeed;
    long eventCount;
    int  workerCounts;
    int  runControl;
    scala.concurrent.forkjoin.ForkJoinPool$WaitQueueNode syncStack;
    scala.concurrent.forkjoin.ForkJoinPool$WaitQueueNode spareStack;
}

-keepclassmembernames class scala.concurrent.forkjoin.ForkJoinPool$WorkQueue {
    int qlock;
}

-keepclassmembernames class scala.concurrent.forkjoin.ForkJoinWorkerThread {
    int base;
    int sp;
    int runState;
}

-keepclassmembernames class scala.concurrent.forkjoin.ForkJoinTask {
    int status;
}

-keepclassmembernames class scala.concurrent.forkjoin.LinkedTransferQueue {
    scala.concurrent.forkjoin.LinkedTransferQueue$PaddedAtomicReference head;
    scala.concurrent.forkjoin.LinkedTransferQueue$PaddedAtomicReference tail;
    scala.concurrent.forkjoin.LinkedTransferQueue$PaddedAtomicReference cleanMe;
}

-adaptresourcefilenames    **.properties,**.stg
-adaptresourcefilecontents **.properties,**.stg,META-INF/MANIFEST.MF

-keep class edu.** {
    void set*(***);
    void set*(int, ***);

    boolean is*();
    boolean is*(int);

    *** get*();
    *** get*(int);
}
