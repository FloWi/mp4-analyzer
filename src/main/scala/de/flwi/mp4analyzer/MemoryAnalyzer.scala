package de.flwi.mp4analyzer

import java.io.File

object MemoryAnalyzer extends App {
  println("Available processors (cores): " + Runtime.getRuntime.availableProcessors)
  println(s"Free memory: ${Runtime.getRuntime.freeMemory} Bytes, ${bytesToMBytes(Runtime.getRuntime.freeMemory)}MB")
  val maxMemory: Long = Runtime.getRuntime.maxMemory
  println(s"Maximum memory: $maxMemory Bytes, ${bytesToMBytes(maxMemory)}MB")
  println(s"Total memory: ${Runtime.getRuntime.totalMemory} Bytes, ${bytesToMBytes(Runtime.getRuntime.totalMemory)}MB")
  val roots: Array[File] = File.listRoots
  for (root <- roots) {
    println("File system root: " + root.getAbsolutePath)
    println(s"Total space (bytes): ${root.getTotalSpace} Bytes, ${bytesToMBytes(root.getTotalSpace)}MB")
    println(s"Free space (bytes): ${root.getFreeSpace} Bytes, ${bytesToMBytes(root.getFreeSpace)}MB")
    println(s"Usable space (bytes): ${root.getUsableSpace} Bytes, ${bytesToMBytes(root.getUsableSpace)}MB")
  }

  def bytesToMBytes(bytes: Long): Long = {
    (bytes.toDouble / 1024 / 1024).toLong
  }
}

