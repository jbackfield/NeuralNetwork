package edu.depaul.csc578.midterm

import scala.util.Random

import java.awt.geom.Line2D
import java.awt.Shape
import java.awt.Color

/**
 * Singleton for the Weight class
 * 
 * Contains a few different methods and parameters that we will use for creating new weights
 * 
 * @author jbackfield
 */
object Weight {
  
  /** The random number generator */
  val rand : Random = new Random()
  
  /** 
   * Method which will be used to bound our random number given the following formula
   * 
   * Where x is the incoming number and the return from bounding is the new random
   */
  var bounding : (Double => Double) = x => {(x * 1) - .5}
  
  /**
   * Method which creates a new Weight object
   * 
   * Method which given an upstream calculatable and a weight value will create a new weight
   * and return it as such.
   * 
   * @param input The upstream calculatable
   * @param value The starting weight
   * @return New weight
   */
  def apply(input : Calculatable, value : Double = bounding(rand.nextDouble())) : Weight = {
    new Weight(input, value)
  }
}

/**
 * Weight class which represents the connection between two layers.
 * 
 * This class is a representation of the connector which resides between
 * two separate layers.
 * 
 * @author jbackfield
 * @constructor Generates a new weight
 * @param input The upstream calculatable we are connecting to
 * @param value The starting value of the weight
 */
class Weight(val input : Calculatable, var value : Double) extends Drawable {

  /** X Position of the upstream */
  var x_pos : Int = 0
  /** Y Position of the upstream */
  var y_pos : Int = 0
  /** X Position of the downstream */
  var x2_pos : Int = 0
  /** Y Position of the downstream */
  var y2_pos : Int = 0
  /** Last value we saw from upstream */
  var last_input_value : Double = 0.0
  
  /**
   * Perform a calculation of this weight
   * 
   * Get the upstream value and calculate our weight output
   * 
   * @return The output of this weight
   */
  def calculate() : Double = {
    last_input_value = input.calculate
    last_input_value * value
  }
  
  /**
   * Set the end points of this weight
   * 
   * Sets the endpoints of this weight; is mainly used for drawing
   * 
   * @param x2 The downstream x coordinate
   * @param y2 The downstream y coordinate
   */
  def setEndpoints(x2 : Int, y2 : Int) : Unit = {
    val position = input.asInstanceOf[Drawable].getPosition()
    this.x_pos = position._1 + 20
    this.y_pos = position._2 + 10
    this.x2_pos = x2
    this.y2_pos = y2 + 10
  }
  
  /**
   * Update the weights value
   * 
   * Given a gamma and eta, we calculate our new weight given the formula
   * eta * gamma * input
   * 
   * @param gamma Gamma which was derrived
   * @param eta Learning rate
   * @return The amount of change
   */
  def updateWeights(gamma : Double, eta : Double) : Double = {
    val change = eta * last_input_value * gamma
    value = value + change
    change
  }
  
  /**
   * Not used for this class, only implemented because Drawable contains it.
   */
  override def setPosition(x : Int, y : Int) : Unit = {}
  
  /**
   * Not used for this class, only implemented because Drawable contains it
   */
  def getPosition() : (Int, Int) = { (0, 0) }
  
  /**
   * Produce a line for drawing
   * 
   * We create a 2D line between x_pos, y_pos and x2_pos, y2_pos.
   * 
   * @return The line representing this weight
   */
  override def getDrawable() : Shape = {
    new Line2D.Double(this.x_pos, this.y_pos, this.x2_pos, this.y2_pos)
  }
  
  /**
   * Get the color for this line
   * 
   * Given our value we call NeuralNetwork.getColorForWeight to get the color representing this
   * weight.
   * 
   * @return Color representing this weight
   */
  override def getDrawColor() : Color = {
    NeuralNetwork.getColorForWeight(this.value)
  }
}