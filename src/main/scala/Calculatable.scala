package edu.depaul.csc578.midterm

/**
 * A trait for a class that is calculatable
 * 
 * Class implementing this interface will have some way to calculate its internals 
 * and return the value.  Mainly used for Input and Perceptron so that Weight can
 * be generic.
 * 
 * @author jbackfield
 */
trait Calculatable {

  /**
   * Calculate our value and return the calculation
   * 
   * See Input and Perceptron to see how this method is used.
   * 
   * @return The calculation
   */
  def calculate() : Double  
  
}