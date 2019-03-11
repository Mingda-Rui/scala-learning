import java.io.PrintWriter
import java.io.File
object WithPrintWriter {
    def withPrintWriter(file: File, op: PrintWriter => Unit) = {
        val writer = new PrintWriter(file)
        try {
            op(writer)
        } finally {
            writer.close()
        }
    }

    // currying 
    def withPrintWriterV2(file: File)(op: PrintWriter => Unit) = {
        val writer = new PrintWriter(file)
        try {
            op(writer)
        } finally {
            writer.close()
        }
    }

    def main(args: Array[String]): Unit = {
        withPrintWriter(
            new File("date.txt"),
            writer => writer.println(new java.util.Date)
        )

        // currying
        val file = new File("date.txt")
        withPrintWriterV2(file) { writer => 
          writer.println(new java.util.Date)
        }
    }
}