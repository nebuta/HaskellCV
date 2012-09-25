//
//  mainlib.h
//  HaskellCV
//
//  Created by Hiroyuki Kai on 8/31/12.
//  Copyright (c) 2012 Hiroyuki Kai. All rights reserved.
//

#ifndef __HaskellCV__mainlib__
#define __HaskellCV__mainlib__

#include <iostream>
#include "opencv2/opencv.hpp"
#include "opencv2/highgui/highgui.hpp"

extern "C" {
using namespace cv;
using namespace std;
void cellDetectTest();
void matTest();
int zeros1D(int length, int type);
int registerMat(Mat &mat);
Mat& getMat(int id);
}

#endif /* defined(__HaskellCV__mainlib__) */
