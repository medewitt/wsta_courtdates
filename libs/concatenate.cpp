#include <fstream>
#include <sstream>
#include <string>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector read_file_cpp1(std::string path) {
  std::ifstream t(path.c_str());
  std::stringstream ss;
  ss << t.rdbuf();
  return ss.str();
}

// [[Rcpp::export]]
CharacterVector read_file_cpp2(std::string path) {
  std::ifstream in(path.c_str());
  std::string contents;
  in.seekg(0, std::ios::end);
  contents.resize(in.tellg());
  in.seekg(0, std::ios::beg);
  in.read(&contents[0], contents.size());
  in.close();
  return(contents);
}

// [[Rcpp::export]]
Rcpp::StringVector concatenate(Rcpp::StringVector x, Rcpp::StringVector y)
{
  int nx=x.size(), n=x.size()+y.size(),i,j;
  Rcpp::StringVector out=no_init(n);
  for (i=0; i<nx; ++i){ 
    out[ i ] = x[ i ];
    }
  for (j=i, i=0; j<n; ++j, ++i){ 
    out[ j ] = y[i] ;
    }
  return out;
}