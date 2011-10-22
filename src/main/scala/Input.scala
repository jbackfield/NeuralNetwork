package edu.depaul.csc578.midterm

import java.awt.Shape
import java.awt.geom.Ellipse2D
import java.awt.geom.Rectangle2D
import java.awt.Graphics2D
import java.awt.Color

/**
 * Singleton representing the Input class
 * 
 * Singleton of the Input class which contains all helper methods the Input class needs
 * 
 * @author jbackfield
 */
object Input {
  
  /**
   * Creates a new Input class
   * 
   * Given a value for the input and if it is a bias or not, create a new Input
   * 
   * @param value Value for this Input
   * @param is_bias Whether or not this Input will be used as a bias or not
   * @return New Input object
   */
  def apply(value : Double = 1.0, is_bias : Boolean = false) : Input = {
    new Input(value, is_bias)
  }
}

/**
 * Class representation of an Input
 * 
 * Class representation of an Input which will eventually feed into a Weight.
 * 
 * @author jbackfield
 * @constructor Creates a new Input
 * @param value The initial value of this Input
 * @param is_bias Whether or not this Input will be used as a bias
 */
class Input(var value : Double, is_bias : Boolean) extends Drawable with Calculatable {
  
  /** The x position of this object */
  var x_pos = 0
  /** The y position of this object */
  var y_pos = 0
  
  /** 
   * Calculate this value
   * 
   * Will always just return value
   * 
   * @return this.value
   */
  override def calculate() : Double = {
    this.value
  }
  
  /** 
   * Set the position of this Input
   * 
   * Sets the position of this Input for drawing purposes.
   * 
   * @param x The X for this Input
   * @param y The Y for this Input
   */
  override def setPosition(x : Int, y : Int) : Unit = {
    this.x_pos = x
    this.y_pos = y
  }
  
  /**
   * Get the position of this Input
   * 
   * Gets the position of this Input for drawing purposes
   * 
   * @return Tuple containing the (x, y) coordinates
   */
  override def getPosition() : (Int, Int) = {
    if(is_bias) {
      (this.x_pos - 10, this.y_pos + 10)
    } else {
      (this.x_pos, this.y_pos)
    }
  }
  
  /**
   * Get the drawable shape
   * 
   * Gets the drawable shape (a square) representation of this Input.
   * 
   * @return Shape (Rectangle2D) for drawing
   */
  override def getDrawable() : Shape = {
    new Rectangle2D.Double(this.x_pos, this.y_pos, 20, 20)
  }
  
  /**
   * Get the color to draw our drawable as
   * 
   * Get the color that our drawable should be painted as
   * 
   * @return Color black
   */
  override def getDrawColor() : Color = { Color.BLACK }
  
  /**
   * Draw this input as a bias
   * 
   * Since there is no Triangle shape in awt, have to draw it by hand.
   * 
   * @param g2 The graphics handle we will draw to.
   */
  def drawAsBias(g2 : Graphics2D) : Unit = {
    g2.drawLine(this.x_pos, this.y_pos + 20, this.x_pos + 20, this.y_pos + 20)
    g2.drawLine(this.x_pos, this.y_pos + 20, this.x_pos + 10, this.y_pos)
    g2.drawLine(this.x_pos + 20, this.y_pos + 20, this.x_pos + 10, this.y_pos)
  }
}