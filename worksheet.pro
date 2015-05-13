-injars      build/install/c-worksheet-instrumentor/lib/scala-library-2.11.6.jar(!META-INF/**)
-injars      build/install/c-worksheet-instrumentor/lib/antlr4-runtime-4.5.jar(!META-INF/**)
-injars      build/install/c-worksheet-instrumentor/lib/scala-xml_2.11-1.0.1.jar(!META-INF/**)
-injars      build/install/c-worksheet-instrumentor/lib/argonaut_2.11-6.0.4.jar(!META-INF/**)
-injars      build/install/c-worksheet-instrumentor/lib/antlr-runtime-3.5.2.jar(!META-INF/**)
-injars      build/install/c-worksheet-instrumentor/lib/ST4-4.0.8.jar(!META-INF/**)
-injars      build/install/c-worksheet-instrumentor/lib/scala-parser-combinators_2.11-1.0.1.jar(!META-INF/**)
-injars      build/install/c-worksheet-instrumentor/lib/scalaz-core_2.11-7.0.6.jar(!META-INF/**)
-injars      build/install/c-worksheet-instrumentor/lib/worksheetify.instrumentor-0.2.3-SNAPSHOT.jar
-outjars     cworksheet.jar
-libraryjars <java.home>/lib/rt.jar

-dontwarn scala.**
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
