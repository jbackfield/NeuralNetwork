package edu.depaul.csc578.midterm

import javax.swing.JFrame
import javax.swing.JLabel
import javax.swing.JTextField
import javax.swing.JPanel
import javax.swing.BoxLayout
import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JOptionPane
import javax.swing.JComponent

import java.io.File
import java.io.FileReader
import java.io.BufferedReader

/**
 * Basic class that is used to do all the displaying
 * 
 * Displays the neural network after parameters have been set
 * 
 * @author jbackfield
 * @constructor Generates a new main application
 */
class Midterm() extends ActionListener {
  
  /** Field for the maximum starting weight */
  val maxRandWeight = new JTextField("5")
  /** Field for the minimum starting weight */
  val minRandWeight = new JTextField("-5")
  /** Field for the maximum number of epochs to run */
  val epochField = new JTextField("10000")
  /** Field for the margin of error when determining correct classifications */
  val errorMarginField = new JTextField("0.05")
  /** Field for the learning rate */
  val etaField = new JTextField("0.1")
  /** Field for the csv file to read from for input data */
  val fileField = new JTextField("/Users/jbackfield/iris.csv")
  /** Field for the number of perceptrons in the hidden layer */
  val hiddenlayerField = new JTextField("10")
  /** Windows frame */
  val frame = new JFrame()
  frame.setSize(800, 600)
  frame.setTitle("CSC-578 Midterm (Joshua Backfield")
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  val mainpane : JPanel = new JPanel();
  val boxlayout = new BoxLayout(mainpane, BoxLayout.Y_AXIS)
  mainpane.setLayout(boxlayout)
  createPanel(mainpane, "Max Starting Weight:", maxRandWeight)
  createPanel(mainpane, "Min Starting Weight:", minRandWeight)
  createPanel(mainpane, "Hidden Layer Count:", hiddenlayerField)
  createPanel(mainpane, "Maximum Epochs:", epochField)
  createPanel(mainpane, "Error Margin:", errorMarginField)
  createPanel(mainpane, "Learning Rate (Eta):", etaField)
  createPanel(mainpane, "File:", fileField)
  createExecute(mainpane)
  frame.setContentPane(mainpane)
  frame.pack()
  frame.setVisible(true)
  
  /**
   * Create a panel and add it to mainpane
   * 
   * This is a helper function which creates a new JPanel, adds a JLabel with 
   * text in it next to obj.  Then packs this into the mainpane
   * 
   * @param mainpane The main JPanel we are adding to
   * @param text The text to display next to the input field
   * @param obj The input field
   */
  def createPanel(mainpane : JPanel, text : String, obj : JComponent) : Unit = {
    val pane = new JPanel()
    val layout = new BoxLayout(pane, BoxLayout.X_AXIS)
    pane.add(new JLabel(text))
    pane.add(obj)
    mainpane.add(pane)
  }
  
  /**
   * Create the execute (Go) button
   * 
   * Helper function which creates the Go button and adds the Midterm class as a listener
   * then sets its action command to "Go" and adds it to the main panel
   * 
   * @param panel The main JPanel we are adding to
   */
  def createExecute(panel : JPanel) : Unit = {
    val execpane = new JPanel()
    val layout = new BoxLayout(execpane, BoxLayout.X_AXIS)
    val go_button = new JButton("Go")
    go_button.setActionCommand("Go")
    go_button.addActionListener(this)
    execpane.add(go_button)
    panel.add(execpane)
  }
  
  /**
   * Override of actionPerformed from ActionListener
   * 
   * Here we listen for "Go" and if heard, we get all of the parameters
   * from the main window, create a new NeuralNetwork object and run the epochs
   * until we hit our ending parameters.
   * <br>
   * Bounding happens by looking at Weight.bounding method.  The formula is used to 
   * convert a random between 0-1 to a number between min-max
   * (x * (max - min) + min)
   * 
   * @param e The ActionEvent that was generated by the caller
   */
  override def actionPerformed(e : ActionEvent) : Unit = {
    if(e.getActionCommand().equals("Go")) {
      val top = maxRandWeight.getText().toDouble
      val bottom = minRandWeight.getText().toDouble
      Weight.bounding = x => {x * (top - bottom) + bottom}
      val file = fileField.getText()
      val cnt = hiddenlayerField.getText().toInt
      val max_epochs = epochField.getText().toInt
      val error_margin = errorMarginField.getText().toDouble
      val eta = etaField.getText().toDouble
      val in : List[(List[Double], Double)] = try {
        Midterm.parseFile(new File(file))
      } catch {
        case e => JOptionPane.showMessageDialog(frame, "Failed to open file " + file); return
      }
      val nn = NeuralNetwork(in.head._1.length, cnt, eta)
      nn.display()
      var cont = true
      while(cont) {
        nn.max_rms = 0.0
        nn.avg_rms = 0.0
        nn.percent_correct = 0.0
        Midterm.test(nn, in)
        val epoch_res = Midterm.epochTest(nn, in, error_margin)
        nn.updateEpochs()
        if((epoch_res._2 / in.length) == 1 || nn.epochs >= max_epochs) {
          cont = false
        }
        println(nn.toString())
        nn.updateDisplay()
      }
    }
  }
}

/**
 * Midterm singleton which provides singleton style methods.
 * 
 * This contains some general functions that can be used to actually perform
 * learning and testing.
 * 
 * @author jbackfield
 */
object Midterm {
  
  /**
   * Given a set of training data, try to learn the data
   * 
   * We take a neural network, a list of (input, output) tuples and iterate over them
   * performing a learn on each of the tuples.
   * 
   * @param nn The NeuralNetwork object we are trying to train
   * @param lists A List of training data, in the form of tuples (List of input, expected output)
   */
  def test(nn : NeuralNetwork, lists : List[(List[Double], Double)]) : Unit = 
    lists match {
    case List() => Unit
    case head :: tail => nn.learn(head._1, head._2); test(nn, tail);
  }
  
  /**
   * Given a set of training data, determine what our rms and correctness is
   * 
   * Taking a neural network, a list of (input, output) tuples and error margin
   * we iterate over them to determine what our average rms is and the percent correctness.
   * 
   * @param nn The NeuralNetwork object we are trying to train
   * @param lists A List of training data, in the form of tuples (List of input, expected output)
   * @param err The error margin
   * @param correct_count Used for tail recursion determining the number of correct tuples
   * @param curr Used for tail recursion, determines the number of tuples
   * @param acc Used for tail recurse, accumulator for the RMS
   * @return Tuple (avg_rms, percent_correct)
   */
  def epochTest(nn : NeuralNetwork, lists : List[(List[Double], Double)], err : Double, 
      correct_count : Int = 0, curr : Int = 0, acc : Double = 0.0) : (Double, Double) = 
    lists match {
    case List() => {
      nn.percent_correct = correct_count.toDouble / curr.toDouble;
      nn.avg_rms = acc / curr.toDouble;
      (nn.avg_rms, nn.percent_correct);
    }
    case head :: tail => {
      val ans = nn.rms(head._1, head._2)
      nn.max_rms = if(ans._2 > nn.max_rms) { ans._2 } else { nn.max_rms }
      if(ans._2 <= err) {
        epochTest(nn, tail, err, correct_count + 1, curr + 1, acc + ans._2)
      } else {
    	  epochTest(nn, tail, err, correct_count, curr + 1, acc + ans._2)
      }
    }
  }
  
  /**
   * Parses a line from a csv file
   * 
   * Parses a line from a csv file into a tuple (input, output).
   * 
   * @param line An array of Strings
   * @return Tuple of (input, output)
   */
  def parseLine(line : Array[String]) : (List[Double], Double) = {
    val lis : List[Double] = line.toList.map( s => s.toDouble)
    (lis.dropRight(1), lis.last)
  }
  
  /**
   * Parse a csv file
   * 
   * Given a file, we will iterate over all lines returning a list of tuples (input, output)
   * 
   * @param file The file object which we will read from
   * @return List of tuples (input, output)
   */
  def parseFile(file : File) : List[(List[Double], Double)] = {
    val br = new BufferedReader(new FileReader(file))
    /**
     * Parse the file line by line
     * 
     * Runs against the closure br, and reads line by line doing a parseLine
     * 
     * @param list Used for tail recursion, contains the return
     * @return List of tuples (input, double)
     */
    def readFile(list : List[(List[Double], Double)] = List()) : List[(List[Double], Double)] = br.readLine() match {
      case null => list
      case s : String => readFile(parseLine(s.split(",")) :: list)
    }
    readFile()
  }
  
  /**
   * Main entry point
   * 
   * The main entry point into the application
   * 
   * @param args Command line arguments
   */
  def main(args : Array[String]) : Unit = {
    new Midterm()
  }
}