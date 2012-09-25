//
//  mainlib.cpp
//  HaskellCV
//
//  Created by Hiroyuki Kai on 8/31/12.
//  Copyright (c) 2012 Hiroyuki Kai. All rights reserved.
//
// OpenCV Haskell interface via FFI

 #include "mainlib.h"


#include <map>

extern "C" {
    
    
    using namespace cv;
    using namespace std;
    

    
    static map<int,Mat> mats;
    static int new_id = 0;
    
    int zeros1D(int length, int type){
        Mat m = Mat::zeros(1,length,CV_32S);
//        m.clone();
        new_id++;
        mats.insert(map<int,Mat>::value_type(new_id,m));
        return mats.size()-1;
    }
/*    int valAt(int id,int pos){
        Mat m = mats.at(map<int,Mat>::key_type(id));
        return *(m.ptr<int>(pos));
    } */
    int length(int id){
        printf("id: %d\n",id);
        printf("%d matrices in memory\n",(int)(mats.size()));
        Mat m = mats.at(0);
        printf("(%d,%d)",m.size().width,m.size().height);
        return m.size().height * m.size().width;
    }
    int meanMat(int id){
        return 0;
    }
    
    void maxDetectTest(){
        char imgfile[] = "/Users/hiroyuki/Google Drive/Documents/Groves Lab Data/Scope Pics/120423 Scope 5 TCR Tracking/Sample 1b and 3b/sample1b_crop.tif";
        Mat image = imread(imgfile);
        int dim = image.dims;
        int numCh = image.channels();
        printf("dimension: %d\nchannels: %d",dim,numCh);
    }
    
    void cellDetectTest(){
        /* 細胞検出器のロード */
        CascadeClassifier cascade = CascadeClassifier::CascadeClassifier( "/Users/hiroyuki/Downloads/MachineLearning/HOG/cascade.xml" );
        
        int fileNum;
        
        for(fileNum = 1; fileNum <= 398; fileNum++){
            /* 画像のロード */
            char imgfile[1000];
            sprintf(imgfile,"/Users/hiroyuki/Downloads/MachineLearning/OK/%04d.jpg",fileNum);
            Mat image = imread(imgfile);
            
            /* 細胞検出 */
            vector<Rect> rects;
            cascade.detectMultiScale( image, rects, 1.1, 3, 0, Size(80,80), Size(200,200) );
            
            groupRectangles(rects,0);
            
            /* 細胞領域の描画 */
            int i;
            for( i = 0; i < rects.size(); i++ )
            {
                /* extract the rectanlges only */
                Rect r = rects.at(i);
                int width = r.width;
                Point center = Point(r.x+width/2,r.y+width/2);
                int radius = width / 2;
                circle(image,center,radius,CV_RGB(255,0,0),3);
                //                rectangle(image,r,CV_RGB(255,0,0),3);
            }
            
            /* 画像の表示 */
            cvNamedWindow( "cell_detect", 0 );
            imshow( "cell_detect", image );
            waitKey(0);
        }
        return;
    }
    
    int randMat(int y,int x){
        Mat mat(y, x, CV_8UC1);
        new_id++;
        cv::randu(mat, cv::Scalar(0), cv::Scalar(256));
        mats.insert(map<int,Mat>::value_type(new_id,mat));
        //        std::cout << *mat << std::endl << std::endl;
 //       imshow("randMat()",mat);
 //       mat.clone();
//        waitKey();
        return new_id;
    }
    
    Mat* monoColor(int h, int w, int r, int g, int b){
        Mat_<Vec3b>* mat = new Mat_<Vec3b>(h,w,Vec3b(r,g,b));
        return mat;
    }
    
    void showMat(Mat* mat){
        imshow("showMat()",*mat);
        waitKey();
    }

    int registerMat(Mat &mat){
        new_id++;
        mats.insert(map<int,Mat>::value_type(new_id,mat));
        return new_id;
    }
    
    Mat& getMat(int id) {
        return mats.at(map<int,Mat>::key_type(id));
    }
    
    Mat* readImg(char* path){
        string str(path);
        Mat mat = cv::imread(str);
        return &mat;
    }
    
    void matTest()
    {
        // create a 100x100x100 8-bit array
        //    int sz[] = {100, 100};
        cv::Mat array2D = cv::Mat::eye(1000, 1000, CV_8U);;
        
        imshow("test",array2D);
        waitKey();
    }
    
    int cvtColor(int code, int id) {
        Mat res;
        Mat mat = getMat(id);
        cv::cvtColor(mat,res,code);
        return registerMat(res);
    }
    
    int gaussian(int kernel_w, int kernel_h, int sigma_x, int sigma_y, int id) {
        Mat res;
        Mat mat = getMat(id);
        cv::GaussianBlur(mat, res, Size(kernel_w,kernel_h),sigma_x,sigma_y);
        return registerMat(res);
    }
    
}