package edu.depaul.csc578.midterm

import javax.swing.JFrame
import javax.swing.JComponent
import java.awt.Container
import java.awt.Point
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Color

/**
 * Singleton of the NeuralNetwork class
 * 
 * Used for doing some general methods and also for creating the NeuralNetwork object
 * 
 * @author jbackfield
 */
object NeuralNetwork {
  
  /**
   * Get a color for a given value.
   * 
   * Pass in a value and receive a color representation of the value
   * 
   * @param value The value to get the color for
   * @return The color to display the value for
   */
  def getColorForWeight(value : Double) : Color = {
    if(value <= -5) {
      Color.RED
    } else if(value <= -1) {
      Color.ORANGE
    } else if(value <= 0.5) {
      Color.YELLOW
    } else if(value <= 0.5) {
      Color.GREEN
    } else if(value <= 1) {
      Color.BLUE
    } else if(value <= 5) {
      Color.CYAN
    } else {
      Color.PINK
    }
  }

  /**
   * Method used to draw each part of the color palette
   * 
   * Method draws one square of the palette and a line at the bottom of it.
   * 
   * @param x The X coordinate of the top left corner
   * @param y The Y coordinate of the top left corner
   * @param w The width/height of the square
   * @param color The color of this square
   * @param g2 The graphics object which allows us to draw
   * @param line_text The text to display to the right of the line
   * @param draw_line Whether or not to draw the line at the bottom of the square
   */
  def drawColorPaletteLine(x : Int, y : Int, w : Int, color : Color, g2 : Graphics2D, 
      line_text : String, draw_line : Boolean = true) : Unit = {
    g2.setColor(color)
    g2.fillRect(x, y, w, w)
    if(draw_line) {
      g2.setColor(Color.BLACK)
      g2.drawLine(x, y + 20, x + 30, y + 20)
      g2.drawString(line_text, x + 35, y + 25)
    }
  }
  
  /**
   * Draw the color palette representing the legend
   * 
   * Draws the legend to the left of the output image
   * 
   * @param g2 The graphics object which allows us to draw.
   */
  def drawColorPalette(g2 : Graphics2D) : Unit = {
    val w = 20
    drawColorPaletteLine(10, 10, w, Color.RED, g2, "-5")
    drawColorPaletteLine(10, (w * 1) + 10, w, Color.ORANGE, g2, "-1")
    drawColorPaletteLine(10, (w * 2) + 10, w, Color.YELLOW, g2, "-.5")
    drawColorPaletteLine(10, (w * 3) + 10, w, Color.GREEN, g2, ".5")
    drawColorPaletteLine(10, (w * 4) + 10, w, Color.BLUE, g2, "1")
    drawColorPaletteLine(10, (w * 5) + 10, w, Color.CYAN, g2, "5")
    drawColorPaletteLine(10, (w * 6) + 10, w, Color.PINK, g2, null, false)
  }
  
  /**
   * Generate all inputs
   * 
   * Given a count of inputs, generate the list of inputs.
   * 
   * @param count The number of inputs to generate, 1 indexed
   * @param layer Tail recursive parameter containing the return
   * @return List of the input objects
   */
  def gen_inputs(count : Int, layer : List[Input] = List()) : List[Input] = count match {
    case 0 => layer
    case _ => gen_inputs(count - 1, Input() :: layer)
  }
  
  /**
   * Generate a layer
   * 
   * Given a set of calculatables and a bias, create a list of perceptrons
   * 
   * @param inputs The inputs each of our perceptrons will link to
   * @param bias The bias to use for each of our perceptrons
   * @param count_h The number of perceptrons in this layer
   * @param layer Tail recursive parameter which represents the return
   * @return List of perceptrons which represents a layer
   */
  def gen_layer(inputs : List[Calculatable], bias: Input, count_h : Int, layer : List[Perceptron] = List()) 
  : List[Perceptron] = count_h match {
    case 0 => layer
    case _ => gen_layer(inputs, bias, count_h - 1, gen_perceptron(inputs, bias) :: layer)
  }
  
  /**
   * Generate a weight list
   * 
   * Given a set of calculatables, translate that into a list of weights.
   * 
   * @param inputs A list of calculatables
   * @return The list of weights
   */
  def gen_weightlist(inputs : List[Calculatable]) : List[Weight] = {
    inputs.foldLeft[List[Weight]](List()) { (acc, next) => Weight(next) :: acc }
  }
  
  /**
   * Generates a perceptron
   * 
   * Given a list of calculatables and a bias, generate a perceptron.  The calculatables
   * are actually the upstream objects.  In the hidden layer, this would be a list of Inputs.
   * In the output layer this would be a list of Perceptrons.  Bias will always be an input.
   * 
   * @param inputs The list of calculatables which is upstream
   * @param bias The bias of this layer.
   * @return The new perceptron
   */
  def gen_perceptron(inputs : List[Calculatable], bias : Input) : Perceptron = {
    Perceptron(gen_weightlist(inputs), bias)
  }
  
  /**
   * Creates a new NeuralNetwork
   * 
   * Allows us to just call NeuralNetwork() without specifying new but still letting us to work
   * before the constructor
   * 
   * @param input_count The number of inputs we want to have
   * @param per_h The number of perceptrons in the hidden layer
   * @param eta The learning rate
   * @return A new NeuralNetwork given the parameters requested.
   */
  def apply(input_count : Int, per_h : Int = 1, eta : Double = 0.3) : NeuralNetwork = {
    val hiddenlayer_bias = Input(-1.0, true)
    val outputlayer_bias = Input(-1.0, true)
    val input_list = gen_inputs(input_count)
    val hiddenlayer = gen_layer(input_list, hiddenlayer_bias, per_h)
    val outputlayer = gen_layer(hiddenlayer, outputlayer_bias, 1)
    
    new NeuralNetwork(input_list, hiddenlayer, outputlayer, eta)
  }
}

/**
 * Represents a Neural Network
 * 
 * This class represents a NeuralNetwork, it contains all of the inputs, hidden layer and output 
 * required for the network.  It is a JComponent so that it can be drawn to screen
 * 
 * @author jbackfield
 * @constructor Create a new NeuralNetwork given a set of inputs, hiddenlayer, output and eta
 * @param inputs A list of inputs
 * @param hiddenlayer A list of perceptrons representing the hidden layer
 * @param output A list of perceptrons representing the output layer
 * @param eta The learning rate for this neural network
 */
class NeuralNetwork(inputs : List[Input], hiddenlayer : List[Perceptron], output : List[Perceptron], 
    eta : Double) extends JComponent {

  /** Width of the frame */
  val _width = 800
  /** Height of the frame */
  val _height = 600
  /** Frame to display to */
  val frame = new JFrame()
  /** Epochs that have been run on this neural network */
  var epochs = 0
  /** Average RMS for this epoch */
  var avg_rms = 0.0
  /** Maximum RMS for this epoch */
  var max_rms = 0.0
  /** The percent correct for this epoch */
  var percent_correct = 0.0
  
  /** Update the number of epochs */
  def updateEpochs() = {
    epochs = epochs + 1
  }
  
  /** 
   * Set the inputs for this neural network
   * 
   * Given a list of values for our inputs, we will attempt to set the inputs appropriately
   * 
   * @param in List of the values for our input layer
   * @param ins List of our inputs representing our input layer
   * @return This for chaining
   */
  def setInputs(in : List[Double], ins : List[Input] = inputs) : NeuralNetwork = (in, ins) match {
    case (List(), List()) => this
    case (List(), head :: tail) => throw new IllegalArgumentException("Must be same size as input")
    case (head :: tail, List()) => throw new IllegalArgumentException("Must be same size as input")
    case (in_head :: in_tail, ins_head :: ins_tail) => ins_head.value = in_head; setInputs(in_tail, ins_tail)
  }
  
  /** 
   * Sets the positions of drawables
   * 
   * Sets the positions of a list of drawables given an x_offset and a y_offset.  We will set the actual
   * position to (x_offset, y_offset * num).
   * 
   * @param drawables List of the drawables we need to set positions for
   * @param x_offset The x position we will be drawing our drawables to
   * @param y_offset The y distance between each element
   * @param num The current number we are drawing (used to compute the y parameter)
   * @return This for chaining
   */
  def setPositions(drawables : List[Drawable], x_offset : Int, y_offset : Int, num : Int = 1) : NeuralNetwork = 
    drawables match {
    case List() => this
    case head :: tail => {
      head.setPosition(x_offset, y_offset * num)
      setPositions(tail, x_offset, y_offset, num + 1);
    }
  }
  
  /**
   * Draw all objects in our list
   * 
   * For each objects in the list of drawables, calls getDrawColor and getDrawable and getString on
   * each drawable.  We then set the color, and draw the drawable as well as the string that is returned.
   * 
   * @param drawables List of our drawables
   * @param g The graphics object that we will use to draw to the screen
   * @return This for chaining
   */
  def drawAll(drawables : List[Drawable], g : Graphics2D) : NeuralNetwork = drawables match {
    case List() => this
    case head :: tail => {
      g.setColor(head.getDrawColor)
      g.draw(head.getDrawable())
      drawAll(tail, g)
    }
  }
  
  /**
   * Calculate the difference between actual and what we got
   * 
   * Calculate a tuple containing the difference between actual and got as well
   * as the absolute value of the same.
   * 
   * @param actual What we expected to get
   * @param got What we actually got
   * @return Tuple of (actual - got, |actual - got|)
   */
  def rms(actual : Double, got : Double) : (Double, Double)= { 
    val diff = actual - got
    (diff, scala.math.abs(diff))
  }
  
  /**
   * Given a sample row, get the RMS for that row
   * 
   * Sets the inputs from example_input and calculates the output.  We then get the rms given
   * example_out.
   * 
   * @param example_input A list of the input values
   * @param example_output The expected output value
   * @return Tuple of (actual - got, |actual - got|)
   */
  def rms(example_input : List[Double], example_output : Double) : (Double, Double) = {
    val out = this.setInputs(example_input).calculate()
    rms(example_output, out)
  }
  
  /**
   * Perform a learning epoch for a specific example
   * 
   * Given a set of input values and an output value, perform a calculation and do a
   * backpropagation on the result.
   * 
   * @param example_input A list of the input values
   * @param example_output The expected output value
   */
  def learn(example_input : List[Double], example_output : Double) : Unit = {
    val out = this.setInputs(example_input).calculate()
    this.output.foreach(_ gamma_output example_output)
    this.hiddenlayer.foreach(x => {
      x.gamma_hidden(this.output.foldLeft[Double](0.0) { (acc, next) => {
        acc + (next.getConnectingWeight(x) * next.gamma)} })
    })
    this.output.foreach(_ updateWeights eta)
    this.hiddenlayer.foreach(_ updateWeights eta)
  }
  
  /**
   * Perform an initial display of the frame
   * 
   * This sets up the frame, sets the positions of everything and also
   * sets up the screen as needed to display properly.
   * 
   * @return The frame we are drawing to.
   */
  def display() : JFrame = {
    frame.setSize(this._width, this._height)
    frame.setTitle("Neural Network")
    val x_offset = (this._width / 6)
    val output_spacing = (this._width - 100) / (output.length + 1)
    setPositions(output, x_offset * 5, output_spacing)
    output.head.bias.setPosition(x_offset * 5 - 40, 20)
    val hidden_spacing = (this._height - 100) / (hiddenlayer.length + 1)
    setPositions(hiddenlayer, x_offset * 3, hidden_spacing)
    hiddenlayer.head.bias.setPosition(x_offset * 3 - 40, 20)
    val input_spacing = (this._height - 100) / (inputs.length + 1)
    setPositions(inputs, x_offset * 1, input_spacing)
    
    output.foreach(_ setUpstream)
    hiddenlayer.foreach(_ setUpstream)
    frame.add(this)
    frame.setVisible(true)
    frame
  }
  
  /**
   * Force a repaint of this JComponent
   */
  def updateDisplay() : NeuralNetwork = {
    this.repaint(0, 0, 800, 600)
    this
  }
      
  /**
   * Perform a calculation given a set of outputs
   * 
   * We iterate over the output list and create a list of values.  We return only the head of
   * the list since we only actually only have 1 output.  We should actually just return the 
   * entire list
   * 
   * @return The output value
   */
  def calculate() : Double = {
        this.output.foldLeft[List[Double]](List()) { (acc, next) => next.calculate :: acc }.head
  }
  
  /**
   * Called when we need to be redrawn
   * 
   * Draw all of the representational parts of the image (input, hidden, output layers).
   * 
   * @param g The graphics parameter which we will use to draw to the screen with
   */
  override def paintComponent(g : Graphics) : Unit = {
    val g2 = g.asInstanceOf[Graphics2D]
    this.drawAll(output, g2)
    this.drawAll(hiddenlayer, g2)
    this.drawAll(inputs, g2)
    output.head.bias.drawAsBias(g2)
    hiddenlayer.head.bias.drawAsBias(g2)
    this.drawAll(this.output.foldLeft[List[Weight]](List()) { (acc, next) => next.weights ::: acc }, g2)
    this.drawAll(this.hiddenlayer.foldLeft[List[Weight]](List()) { (acc, next) => next.weights ::: acc }, g2)
    g2.setColor(Color.BLACK)
    g2.drawString("Epochs " + epochs, 700, 500)
    g2.drawString("AVG RMS: " + avg_rms, 100, 500)
    g2.drawString("MAX RMS: " + max_rms, 100, 525)
    g2.drawString("Percent Correct: " + (percent_correct * 100) + "%", 100, 550)
    NeuralNetwork.drawColorPalette(g2)
  }
  
  /**
   * Returns a string that is of the format as requested
   * 
   * ***** Epoch <num> *****
   * Maximum RMSE: <num>
   * Average RMSE: <num>
   * Percent Correct: <num>%
   * 
   * @return String representing where this neural network is in epochs and learning
   */
  override def toString() : String = {
    "***** Epoch " + epochs + "*****\nMaximum RMSE: " + max_rms + "\nAverage RMSE: " + avg_rms +
    "\nPercent Correct: " + (percent_correct * 100) + "%"
  }
}