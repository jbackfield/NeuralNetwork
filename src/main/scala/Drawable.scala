package edu.depaul.csc578.midterm

import java.awt.Shape
import java.awt.Color

/**
 * Indicates that a class is drawable
 * 
 * This allows NeuralNetwork to draw objects easier
 * 
 * @author jbackfield
 */
trait Drawable {

  /**
   * Set the position of this object
   * 
   * Sets the position of this object to (x, y) coordinates
   * 
   * @param x X coordinate to set
   * @param y Y coordinate to set
   */
  def setPosition(x : Int, y : Int) : Unit
  
  /**
   * Get the shape representation of this object
   * 
   * Gets the drawable shape of this object so that it can be drawn to the graphics handle
   * 
   * @return Shape of the object
   */
  def getDrawable() : Shape
  
  /**
   * Get the position of this object
   * 
   * Gets the position of this object in (x, y) coordinates
   * 
   * @return Tuple representing (x, y) coordinates
   */
  def getPosition() : (Int, Int)
  
  /**
   * Get the color this drawable should be rendered as
   * 
   * Get the color that this drawable should be drawn in
   * 
   * @return Color to draw our Shape as.
   */
  def getDrawColor() : Color
  
}