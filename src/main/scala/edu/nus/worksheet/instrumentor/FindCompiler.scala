package edu.nus.worksheet.instrumentor

import java.io.File;

object FindCompiler {

  def findOnPath(binName: String) : String = {
    val path = System.getenv("PATH")
    val pathEls = path.split(File.pathSeparator);

    def maybeBin(f : String) : Option[String] = {
      val folder = new File(f);

      if (folder.isDirectory()) {
        val bin = new File(folder, binName);

        if (bin.exists() && bin.canExecute())
          return Some(bin.getAbsolutePath());
        else
          return None;
      } else {
        return None;
      }
    }

    val bins = pathEls.flatMap(maybeBin);
    if (bins.isEmpty)
      throw new RuntimeException(f"Expected to find ${binName} on PATH");
    else
      return bins(0);
  }

  def findCompilerOnPath() : String = {
    // On Windows 8.1, `os.name` is "Windows 8.1".
    val ccName = if (System.getProperty("os.name").toLowerCase().contains("windows"))
                   // On Windows, we expect Mingw GCC to be on the PATH
                   "gcc.exe";
                 else
                   "cc";

    return findOnPath(ccName);
  }

  def main(args : Array[String]) : Unit = {
    println("CC: " + findCompilerOnPath());
  }
}
