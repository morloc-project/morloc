#ifndef __CNN_HPP__
#define __CNN_HPP__

#include "mlc_tensor.hpp"
#include <cmath>
#include <algorithm>

// --- Input data (now parameterized by dimension args) ---

// Create an image with a cross pattern of the given dimensions
mlc::Tensor2<double> makeImage(int h, int w) {
    mlc::Tensor2<double> img({(int64_t)h, (int64_t)w});
    int midR = h / 2;
    int midC = w / 2;
    // vertical bar
    for (int i = 0; i < h; i++) img(i, midC) = 1.0;
    // horizontal bar
    for (int j = 0; j < w; j++) img(midR, j) = 1.0;
    return img;
}

// Create k convolution kernels of size fh x fw
// Kernel 0: horizontal edge detector
// Kernel 1: vertical edge detector (if k >= 2)
mlc::Tensor3<double> makeKernels(int k, int fh, int fw) {
    mlc::Tensor3<double> kern({(int64_t)k, (int64_t)fh, (int64_t)fw});
    int ksize = fh * fw;
    if (k >= 1) {
        // Kernel 0: horizontal edges (-1 top, 0 mid, 1 bottom)
        for (int i = 0; i < fh; i++) {
            double row_val = (i < fh/2) ? -1.0 : (i > fh/2) ? 1.0 : 0.0;
            for (int j = 0; j < fw; j++) {
                kern[i * fw + j] = row_val;
            }
        }
    }
    if (k >= 2) {
        // Kernel 1: vertical edges (-1 left, 0 mid, 1 right)
        for (int i = 0; i < fh; i++) {
            for (int j = 0; j < fw; j++) {
                double col_val = (j < fw/2) ? -1.0 : (j > fw/2) ? 1.0 : 0.0;
                kern[ksize + i * fw + j] = col_val;
            }
        }
    }
    return kern;
}

// Bias per filter
mlc::Tensor1<double> makeBias(int k) {
    mlc::Tensor1<double> b({(int64_t)k});
    for (int i = 0; i < k; i++) b[i] = 0.1;
    return b;
}

// --- Layers ---

// 2D convolution (valid, no padding, stride 1)
mlc::Tensor3<double> conv2d(
    const mlc::Tensor2<double>& image,
    const mlc::Tensor3<double>& kernels,
    const mlc::Tensor1<double>& bias)
{
    int64_t H = image.shape(0);
    int64_t W = image.shape(1);
    int64_t F = kernels.shape(0);
    int64_t Kh = kernels.shape(1);
    int64_t Kw = kernels.shape(2);
    int64_t Oh = H - Kh + 1;
    int64_t Ow = W - Kw + 1;

    mlc::Tensor3<double> out({F, Oh, Ow});
    for (int64_t f = 0; f < F; f++) {
        for (int64_t i = 0; i < Oh; i++) {
            for (int64_t j = 0; j < Ow; j++) {
                double sum = bias[f];
                for (int64_t ki = 0; ki < Kh; ki++) {
                    for (int64_t kj = 0; kj < Kw; kj++) {
                        sum += image(i + ki, j + kj)
                             * kernels.data()[f * Kh * Kw + ki * Kw + kj];
                    }
                }
                out.data()[f * Oh * Ow + i * Ow + j] = sum;
            }
        }
    }
    return out;
}

// ReLU: element-wise max(0, x)
mlc::Tensor3<double> reluMap(const mlc::Tensor3<double>& t) {
    mlc::Tensor3<double> out({t.shape(0), t.shape(1), t.shape(2)});
    for (size_t i = 0; i < t.size(); i++) {
        out[i] = std::max(0.0, t.data()[i]);
    }
    return out;
}

// Flatten 3D to 1D
mlc::Tensor1<double> flatten3d(const mlc::Tensor3<double>& t) {
    size_t n = t.size();
    mlc::Tensor1<double> out({(int64_t)n});
    for (size_t i = 0; i < n; i++) {
        out[i] = t.data()[i];
    }
    return out;
}

// Dense layer: out = W * x + b
mlc::Tensor1<double> dense(
    const mlc::Tensor2<double>& W,
    const mlc::Tensor1<double>& b,
    const mlc::Tensor1<double>& x)
{
    int64_t out_dim = W.shape(0);
    int64_t in_dim = W.shape(1);
    mlc::Tensor1<double> out({out_dim});
    for (int64_t i = 0; i < out_dim; i++) {
        double sum = b[i];
        for (int64_t j = 0; j < in_dim; j++) {
            sum += W(i, j) * x[j];
        }
        out[i] = sum;
    }
    return out;
}

// Dense layer weights (parameterized by dimensions)
mlc::Tensor2<double> makeWeights(int out_dim, int in_dim) {
    mlc::Tensor2<double> W({(int64_t)out_dim, (int64_t)in_dim});
    for (int i = 0; i < out_dim; i++) {
        for (int j = 0; j < in_dim; j++) {
            if (i == 0) W(i, j) = 0.01 * (j % 5 - 2);  // small, centered around 0
            else if (i == 1) W(i, j) = 0.1;              // uniformly positive
            else W(i, j) = -0.05;                         // uniformly negative
        }
    }
    return W;
}

// Dense bias (parameterized by dimension)
mlc::Tensor1<double> makeDenseBias(int n) {
    mlc::Tensor1<double> b({(int64_t)n});
    for (int i = 0; i < n; i++) b[i] = 0.0;
    if (n >= 2) b[1] = 0.5;  // boost class 1
    return b;
}

// Argmax: index of maximum element
int argmax(const mlc::Tensor1<double>& v) {
    int best = 0;
    for (int64_t i = 1; i < v.shape(0); i++) {
        if (v[i] > v[best]) best = (int)i;
    }
    return best;
}

#endif
