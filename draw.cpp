//
//  draw.cpp
//  HaskellCV
//
//  Created by Hiroyuki Kai on 10/5/12.
//  Copyright (c) 2012 Hiroyuki Kai. All rights reserved.
//

#include "draw.h"

#include "opencv2/opencv.hpp"
#include "opencv2/highgui/highgui.hpp"

#include <map>

extern "C" {
#include "mainlib.h"
    
    using namespace cv;
    using namespace std;

    void m_circle(Mat* mat, int y, int x, int radius, int b, int g, int r){
        circle(*mat, Point(x,y), radius, Scalar(b,g,r));
    }
    
}