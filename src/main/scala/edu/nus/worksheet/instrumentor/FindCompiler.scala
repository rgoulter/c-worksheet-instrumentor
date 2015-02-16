package edu.nus.worksheet.instrumentor

import java.io.File;

object FindCompiler {

  def findCompilerOnPath() : String = {
    val path = System.getenv("PATH")
    val pathEls = path.split(File.pathSeparator);

    def maybeCC(f : String) : Option[String] = {
      val folder = new File(f);

      if (folder.isDirectory()) {
        val cc = new File(folder, "cc");

        if (cc.exists() && cc.canExecute())
          return Some(cc.getAbsolutePath());
        else
          return None;
      } else {
        return None;
      }
    }
    
    val compilers = pathEls.flatMap(maybeCC);
    if (compilers.isEmpty)
      throw new RuntimeException("Expected to find CC on PATH");
    else
      return compilers(0);
  }
  
  def main(args : Array[String]) : Unit = {
    println("CC: " + findCompilerOnPath());
  }
}