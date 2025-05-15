#include <Rcpp.h>
using namespace Rcpp;
#include <iostream>
using namespace std;

// function to be called from R
// [[Rcpp::export]]
NumericMatrix art_data(int iter, int layers, int pixels, double zoom) {

  // storage matrices
  NumericMatrix image(pixels, pixels); 
  NumericMatrix count(pixels, pixels);
  NumericMatrix coeffs(9, layers);

  // set coefficients
  for(int i = 0; i < 9; i++) {
    for(int j = 0; j < layers; j++) {
      coeffs(i,j) = R::runif(-1,1) * 0.5;
    }
  }

  // set image matrix to zeros
  for(int r = 0; r < pixels; r++) {
    for(int c = 0; c < pixels; c++) {
      image(c, r) = 0;
      count(c, r) = 0;
    }
  }
  
  // iterate
  int layer;
  int variant;

  // indices for storing coordinates
  int x_ind;
  int y_ind;
  
  // values for current state
  double x = 0;
  double y = 0;
  double z = 0;
  double w = 0; 
  
  // values for previous state
  double x_old = R::runif(-1, 1);
  double y_old = R::runif(-1, 1);
  double z_old = R::runif(-1, 1);
  
  // handy variables
  double s;
  double theta;
  double u1;
  double u2;
  double u3;
  double f;

  // constants
  const double pi = 3.1415926535;

  // chaos game
  int t = 0;
  while (t < iter) {

    layer = rand() % layers;  // which affine transform to use?
    variant = rand() % 4;     // which variant function to use?

    // coordinates after random transform
    x = x_old + coeffs(0, layer) * x_old + coeffs(1, layer) * y_old + coeffs(2, layer);
    y = y_old + coeffs(3, layer) * x_old + coeffs(4, layer) * y_old + coeffs(5, layer);
    z = z_old + coeffs(6, layer) * x_old + coeffs(7, layer) * y_old + coeffs(8, layer);
    
    if (variant == 0) {
      s = pow(x*x + y*y + z*z, 1/3);
      x = x + sin(s);
      y = y + cos(s);
      z = z + s;

    } else if (variant == 1) {
      s = 1;
      f = pi / 12; 
      x = s * cos(x * f);
      y = s * cos(y * f);
      z = s * cos(z);

    } else if (variant == 2) {
      s = 2;
      f = pi / 12; 
      
      x = s * sin(x * f);
      y = s * sin(y * f);
      z = s * sin(z);
    }

    // compute indices to be updated
    x_ind = int (x * pixels * zoom) + pixels/2;
    y_ind = int (y * pixels * zoom) + pixels/2;
    
    // // wrap
    // while (x_ind < 0) {
    //   x_ind = x_ind + pixels;
    // }
    // while (y_ind < 0) {
    //   y_ind = y_ind + pixels;
    // }
    // while (x_ind >= pixels) {
    //   x_ind = x_ind - pixels;
    // }
    // while (y_ind >= pixels) {
    //   y_ind = y_ind - pixels;
    // }
    
    // store results if they fall within the range
    if(x_ind >= 0 & x_ind < pixels) {
      if(y_ind >= 0 & y_ind < pixels) {
        
        t++;
        count(x_ind, y_ind)++;
        w = count(x_ind, y_ind) / (count(x_ind, y_ind) + 1); 
        image(x_ind, y_ind) = (1-w) * z + w * image(x_ind, y_ind);
        
      }
    }
    
    // move new to old
    x_old = x;
    y_old = y;
    z_old = z; 
    
  }

  return image;
}


