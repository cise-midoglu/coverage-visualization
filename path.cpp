#include <math.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector make_path(int n) {
  NumericVector path(2*n);
  double angle = M_PI/8;
  path[0] = 0, path[n] = 0;
  int cnt = 1;
  double dx = 1, dy = 0;
  do {
    int nr = rand()%4;
    double dtheta = angle*(2.0*rand()/RAND_MAX - 1.0);
    double ct = cos(dtheta), st = sin(dtheta);
    for(int i=0; i<nr && cnt<n; i++, cnt++) {
      //path[2*cnt + 0] = path[2*(cnt-1) + 0] + x;
      //path[2*cnt + 1] = path[2*(cnt-1) + 1] + y;
      //path[0+cnt] = path[0+cnt-1] + x;
      //path[n+cnt] = path[n+cnt-1] + y;
      double t1 = dx;
      dx = 1;
	//dx =  dx*ct + dy*st;
      dy = -t1*st + dy*ct;
      double mag = sqrt(dx*dx + dy*dy);
      dx /= mag, dy /= mag;
      
      path[0+cnt] = path[0+cnt-1] + dx; 
      path[n+cnt] = path[n+cnt-1] + dy;
      
    }
  } while(cnt<n);
  return path;
}

//https://rosettacode.org/wiki/Haversine_formula#C
#define R 6371
#define TO_RAD (3.1415926536 / 180)
static double haversine_dist(double th1, double ph1, double th2, double ph2) {
  double dx, dy, dz;
  ph1 -= ph2;
  ph1 *= TO_RAD, th1 *= TO_RAD, th2 *= TO_RAD;

  dz = sin(th1) - sin(th2);
  dx = cos(ph1) * cos(th1) - cos(th2);
  dy = sin(ph1) * cos(th1);
  return asin(sqrt(dx * dx + dy * dy + dz * dz) / 2) * 2 * R;
}

static double dist(double x0, double y0, double x1, double y1) {
  double dx = x1 - x0, dy = y1 - y0;
  return dx*dx + dy*dy;
  //return sqrt(dx*dx + dy*dy);
}

static int min_dist(double x1, double y1, double *x, double*y, int* idxa, int n) {
  double r = DBL_MAX;
  int idx = -1;
  for(int i=0; i<n; i++) {
    int i2 = idxa[i];
    if(i2==-1) continue;
    //double tr = dist(x1, y1, x[i2], y[i2]);
    double tr = haversine_dist(x1, y1, x[i2], y[i2]);
    if(tr<r) r = tr, idx = i;
  }
  return idx;
}

static int max_dist(int n, double* points) {
  double *x = &points[0], *y = &points[n];
  double r = 0;
  int idx = -1;
  for(int i=0; i<n-1; i++) {
    double x0 = x[i], y0 = y[i];
    for(int j=i+1; j<n; j++) {
      double t = dist(x0, y0, x[j], y[j]);
      if(t>r) r = t, idx = i;
    }
  }
  return idx;
}

// [[Rcpp::export]]
int get_max_dist(NumericVector rcpp_points) {
  double* points = rcpp_points.begin();
  int n = rcpp_points.size()/2;
  return max_dist(n, points);
}

// [[Rcpp::export]]
IntegerVector find_path(NumericVector rcpp_points) {
  int n = rcpp_points.size()/2;
  IntegerVector out(n);
  int* idxa = out.begin();
  //auto t = Rcpp::as<std::vector<double>>(rcpp_points);
  //double* points = &t[0];
  double* points = rcpp_points.begin();
    
  double *x = &points[0], *y = &points[n];
  int* tidxa = (int*)malloc(sizeof *tidxa * n);
  for(int i = 0; i<n; i++) idxa[i] = i;
  int idx = max_dist(n, points);
  double x0 = x[idx], y0 = y[idx];
  tidxa[0] = idx;
  idxa[idx] = -1;
  for(int i = 1; i<n; i++) {
    int idx = min_dist(x0, y0, x, y, idxa, n);
    idxa[idx] = -1;
    tidxa[i] = idx;
    x0 = x[idx], y0 = y[idx];
  }
  for(int i=0; i<n;i++) idxa[i] = tidxa[i];
  free(tidxa);
  return out;
}
