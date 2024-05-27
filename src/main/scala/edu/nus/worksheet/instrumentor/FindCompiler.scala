package edu.nus.worksheet.instrumentor;

import java.io.File;

object FindCompiler {

  def findOnPath(binName: String): Option[String] = {
    val path = System.getenv("PATH")
    val pathEls = path.split(File.pathSeparator);

    def maybeBin(f: String): Option[String] = {
      val folder = new File(f);

      if folder.isDirectory() then {
        val bin = new File(folder, binName);
        val binExe = new File(folder, f"${binName}.exe");

        if bin.exists() && bin.canExecute() then
          return Some(bin.getAbsolutePath());
        else if binExe.exists() && binExe.canExecute() then
          return Some(binExe.getAbsolutePath());
        else return None;
      } else {
        return None;
      }
    }

    return pathEls.flatMap(maybeBin).headOption;
  }

  def findCompilerOnPath(): String = {
    // On Windows 8.1, `os.name` is "Windows 8.1".
    val ccName =
      if System.getProperty("os.name").toLowerCase().contains("windows") then
        // On Windows, we expect Mingw GCC to be on the PATH
        "gcc.exe";
      else "cc";

    return findOnPath(ccName) match {
      case Some(cc) => cc;
      case None =>
        throw new RuntimeException("Could not find compiler on PATH.");
    }
  }

  def main(args: Array[String]): Unit = {
    println("CC: " + findCompilerOnPath());
  }
}
