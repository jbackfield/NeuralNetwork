package edu.depaul.csc578.midterm

import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Rectangle
import javax.swing.JComponent
import java.awt.Point
import java.awt.geom.Ellipse2D
import java.awt.Dimension
import java.awt.Color
import java.awt.Shape

/**
 * Singleton of the Perceptron class
 * 
 * Contains helper methods for the Perceptron
 * 
 * @author jbackfield
 */
object Perceptron {
  
  /**
   * Creates a new Perceptron
   * 
   * Method that allows us to create a new Perceptron given a list of weights and a bias.
   * 
   * @param weights List of the weights this Perceptron is connected to
   * @param bias An input representing the bias
   * @return New Perceptron
   */
  def apply(weights : List[Weight], bias : Input) : Perceptron = {
    new Perceptron(Weight(bias, -0.8) :: weights, bias)
  }
}

/**
 * Class representation of a Perceptron
 * 
 * This class represents a Perceptron and the weights that it is connected to.
 * 
 * @constructor Create a new perceptron.
 * @param weights The weights this perceptron is connected to
 * @param bias The bias for the layer this perceptron is located in
 */
class Perceptron(var weights : List[Weight], val bias : Input) extends Drawable with Calculatable {

  /** X position of this perceptron */
  var x_pos : Int = 0
  /** Y position of this perceptron */
  var y_pos : Int = 0
  /** The previous calculation this perceptron came up with */
  var prev_calculation : Double = 0.0
  /** The gamma calculated by this perceptron */
  var gamma : Double = 0.0
  
  /** 
   * Calculate the output of this perceptron
   * 
   * Calculate the output of this perceptron per the function
   * <br>
   * 1/(1 + e^(-net))
   * <br>
   * net = sum for each input(w * x)
   * 
   * @return Calculation
   */
  override def calculate() : Double = {
    prev_calculation = 1 / (1 + scala.math.pow(scala.math.E, 
        -weights.foldLeft[Double](0.0) { (acc, next) => acc + next.calculate }))
    prev_calculation
  }
  
  /**
   * Set the position of this Perceptron
   * 
   * Set the x and y positions of this Perceptron
   * 
   * @param x X coordinate
   * @param y Y coordinate
   */
  override def setPosition(x : Int, y : Int) : Unit = {
    this.x_pos = x
    this.y_pos = y
  }
  
  /**
   * Calculate gamma for the output layer
   * 
   * Calculate the gamma for the output layer
   * <br>
   * g = o * (1 - o) * (t - o)
   * 
   * @param actual The value we actually expected
   * @return The gamma value
   */
  def gamma_output(actual : Double) : Double = {
    gamma = prev_calculation * (1 - prev_calculation) * (actual - prev_calculation)
    gamma
  }
  
  /**
   * Calculate gamma for any hidden layer
   * 
   * Calculate the gamma for any hidden layer
   * g = o * (1 - o) * diff
   * <br>
   * diff = SUM(w * g) where g in this instance is gamma of the output layer
   * 
   * @param diff SUM(w * g)
   * @return The gamma value
   */
  def gamma_hidden(diff : Double) : Double = {
    gamma = prev_calculation * (1 - prev_calculation) * diff
    gamma
  }
  
  /**
   * Update the weights our perceptron is connected to
   * 
   * We update the weights this perceptron is directly connected to.
   * <br>
   * w = w + n*g*x
   * 
   * @param eta Learning rate
   */
  def updateWeights(eta : Double) : Unit = {
    weights.foldLeft[Double](0.0) { (acc, next) => acc + (next updateWeights(gamma, eta)) }
  }
  
  /**
   * Set the endpoints for our connected weights
   * 
   * Go through all of our weights and set the endpoints appropriately in order to draw the
   * weights properly
   */
  def setUpstream() : Unit = {
    weights.foreach(x => x.setEndpoints(this.x_pos, this.y_pos))
  }
  
  /**
   * Find a weights value which a perceptron is connected to
   * 
   * We iterate over all of our weights and find which one the given perceptron is connected to
   * 
   * @param per Perceptron we are looking for.
   */
  def getConnectingWeight(per : Perceptron) : Double = {
    weights.filter( _.input == per ).head.value
  }
  
  /**
   * Get the position of this perceptron
   * 
   * For drawing purposes we get the x, y coordinates to draw this perceptron.
   * 
   * @return Tuple representing the (x, y) coordinate of this object.
   */
  override def getPosition() : (Int, Int) = {
    (this.x_pos, this.y_pos)
  }
  
  /**
   * Get the image representation of this perceptron
   * 
   * For drawing purposes we get a shape (in this case a circle) representing this perceptron
   * 
   * @return Shape of type circle which represents this perceptron
   */
  override def getDrawable() : Shape = {
    new Ellipse2D.Double(this.x_pos, this.y_pos, 20, 20)
  }
  
  /**
   * Get the color of this perceptron.
   * 
   * For drawing purposes we get the color of this perceptron.
   * 
   * @return Color to draw our shape as.
   */
  override def getDrawColor() : Color = {
    Color.BLACK
  }
}